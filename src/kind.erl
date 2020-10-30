-module(kind).
-export([load/1, load/2, run/2, run/3, type_compile_load/2]).
-import(lists, [zip/2, zip3/3, unzip/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

run(Sources, Args) -> run(Sources, Args, #{sandboxed => true}).
run([S | _] = Sources, Args, Options) when is_list(S) ->
    case load(Sources, Options) of
        {error, Errs}   -> {error, Errs};
        {ok, Mods} -> 
            case [M || M <- Mods,
                       erlang:function_exported(M, main, length(Args)),
                       string:slice(atom_to_list(M), 0, 5) =:= "main_"] of
                []          -> 
                    unload_modules(Mods),
                    error:format({no_main_function_for_arity, length(Args)}, {kind});
                [Module]    ->
                    Out = case catch erlang:apply(Module, main, Args) of
                              {'EXIT', Err}     -> error:format({main_throw, Err}, {kind});
                              {error, Errs}     -> {error, Errs};
                              Res               -> {ok, Res}
                          end,
                    unload_modules(Mods),
                    Out;
               Modules      -> 
                    unload_modules(Mods),
                    error:format({multiple_main_functions_for_arity, length(Args), Modules}, {kind})
            end
    end;
run(Code, Args, Options) -> run([Code], Args, Options).



load(Sources) -> load(Sources, #{}).
load([C | _] = Sources, Options) when is_list(C) ->
    case parser:parse([{text, S} || S <- Sources], Options) of
        {error, Errs}   -> {error, Errs};
        {ok, ASTs}     ->
            case error:collect([macros:expand(AST) || AST <- ASTs]) of
                {error, Errs}      -> {error, Errs};
                {ok, ExpandedASTs}  ->
                    case error:collect([type_compile_load(AST, Options) || AST <- ExpandedASTs]) of
                        {error, Errs}       -> {error, Errs};
                        {ok, Modules}       -> {ok, lists:flatten(Modules)}
                    end
            end
    end;
load(Code, Options) -> load([Code], Options).

type_compile_load(AST, Options) ->
    case typer:type(AST, Options) of
        {error, Errs}                                   -> {error, Errs};
        {ok, {TypedAST, TypesEnv, TypeModules}}  ->
            case def_gen:gen(TypesEnv, TypedAST) of
                {error, Errs}       -> unload_modules(TypeModules),
                                       {error, Errs};
                {ok, Forms}         ->
                    case compile(Forms) of
                        {error, Errs}       -> {error, Errs};
                        {ok, ModuleNames}   -> {ok, ModuleNames ++ TypeModules}
                    end
            end
    end.

compile(Forms) ->
    Options = [report, verbose, from_core],
    CompiledList = [compile_form(Form, Options) || {_, Form} <- Forms],
    case error:collect(CompiledList) of
        {error, Errs}   -> {error, Errs};
        {ok, Bins}      ->
            LoadedModules = [load_binary(ModuleName, Bin) || {ModuleName, Bin} <- Bins],
            error:collect(LoadedModules)
    end.

compile_form(Form, Options) ->
    ModuleName = cerl:atom_val(cerl:module_name(Form)),
    case compile:forms(Form, Options) of
        {ok, ModuleName, Bin}   -> {ok, {ModuleName, Bin}};
        Error                   -> error:format({compile_error, Error, ModuleName, Form}, {kind})
    end.

load_binary(ModuleName, Bin) ->
    BeamName = lists:flatten(io_lib:format("~w.beam", [ModuleName])),
    case code:load_binary(ModuleName, BeamName, Bin) of
        {module, ModuleName}    -> {ok, ModuleName};
        {error, Err}            -> error:format({loading_error, Err}, {kind, ModuleName})
    end.

unload_modules(Modules) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].


-ifdef(TEST).

-define(setup(Code, Args, Tests), {setup, fun() -> kind:run(Code, Args) end, fun(_) -> none end, Tests}).

happy_path_test_() ->
    {"run code and see that it executed correctly",
     ?setup("def main -> False",
            [],
            fun(Res) -> 
                    [?test({ok, 'Boolean/False'}, Res)]
            end)}.

happy_path_arg_test_() ->
    {"run code and see that it executed correctly",
     ?setup("def main arg -> arg",
            ['test'],
            fun(Res) -> 
                    [?test({ok, 'test'}, Res)]
            end)}.

no_main_function_test_() ->
    {"calling run should result in error if the code has no main function",
     ?setup("def blup -> True",
            [],
            fun(Res) -> [?testError({no_main_function_for_arity, 0}, Res)] end)}.

main_error_test_() ->
    {"when the main function returns an error, it be returned as an error",
     ?setup("def main\n"
            " | True -> False\n"
            " | False -> True",
            ['hello'],
            fun(Res) -> [?testError({no_matching_pattern, [hello]}, Res)] end)}.
main_throw_test_() ->
    {"when the main function throws, it should be caught and wrapped in an error",
     ?setup("import erlang/throw\n"
            "def main arg -> throw()\n",
            ['hello'],
            fun(Res) -> [?testError({main_throw, {undef, _}}, Res)] end)}.
    
multiple_main_error_test_() ->
    {"When calling kind:run, the code can't contain or export more than one single main function",
     ?setup(["module test1 { main }\n"
             "def main -> False",
             "module test1 { main }\n"
             "def main -> False"],
            [],
            fun(Res) -> [?testError({multiple_main_functions_for_arity, 0, _}, Res)] end)}.


-endif.
