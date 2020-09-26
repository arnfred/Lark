-module(kind).
-export([load/1, load/2, run/2, run/3]).
-import(lists, [zip/2, zip3/3, unzip/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

run(Code, Args) -> run(Code, Args, #{sandboxed => true}).
run(Code, Args, Options) ->
    case load(Code, Options) of
        {error, Errs}   -> {error, Errs};
        {ok, Mods} -> 
            case [M || M <- Mods, erlang:function_exported(M, main, length(Args))] of
                []          -> error:format({no_main_function_for_arity, length(Args)}, {kind});
                [Module]    ->
                    Out = case catch erlang:apply(Module, main, Args) of
                              {'EXIT', Err}   -> error:format({main_throw, Err}, {kind});
                              Res             -> {ok, Res}
                          end,
                    clean(Mods),
                    Out;
               Modules      -> error:format({multiple_main_functions_for_arity, length(Args), Modules}, {kind})
            end
    end.


load(Code) -> load(Code, #{}).
load(Code, Options) ->
    case parser:parse([{text, Code}], Options) of
        {error, Errs}   -> {error, Errs};
        {ok, ASTs}     ->
            io:format("Tagged AST is ~p~n", [ASTs]),
            case error:collect([type_and_compile(AST) || AST <- ASTs]) of
                {error, Errs}       -> {error, Errs};
                {ok, Modules}       -> {ok, lists:flatten(Modules)}
            end
    end.

% Current problem: As we run typegen and codegen, we call out to compiled type
% definitions from other modules that might or might not be compiled yet. To
% ensure that types are compiled as we need them, we have to order the sources
% by their dependencies
type_and_compile(AST) ->
    case typer:type(AST) of
        {error, Errs}                               -> {error, Errs};
        {ok, {_, Types, TypeModules, DomainDef}} ->
            {ok, Forms} = codegen:gen(AST, DomainDef, Types),
            case compile(Forms) of
                {error, Errs}       -> {error, Errs};
                {ok, ModuleNames}   -> {ok, ModuleNames ++ TypeModules}
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
        Error                   -> error:format({compile_error, Error}, {kind, ModuleName})
    end.

load_binary(ModuleName, Bin) ->
    BeamName = lists:flatten(io_lib:format("~w.beam", [ModuleName])),
    case code:load_binary(ModuleName, BeamName, Bin) of
        {module, ModuleName}    -> {ok, ModuleName};
        {error, Err}            -> error:format({loading_error, Err}, {kind, ModuleName})
    end.

clean(Modules) ->
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

main_throw_test_() ->
    {"when the main function throws, it should be caught and wrapped in an error",
     ?setup("def main arg\n"
            " | True -> False\n"
            " | False -> True",
            ['hello'],
            fun(Res) -> [?testError({main_throw, {if_clause, _}}, Res)] end)}.
    
multiple_main_error_test_() ->
    {"When calling kind:run, the code can't contain or export more than one single main function",
     ?setup("module test { main }\n"
            "def main -> False",
            [],
            fun(Res) -> [?testError({multiple_main_functions_for_arity, 0, _}, Res)] end)}.


-endif.
