-module(lark).
-export([load/1, load/2, run/2, run/3, compile_and_load/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

run(Source, Args) -> run(Source, Args, #{sandboxed => true}).
run(Source, Args, Options) ->
    case load(Source, Options) of
        {error, Errs}       -> {error, Errs};
        {ok, Mod}    ->
            case erlang:function_exported(Mod, main, length(Args)) of
                false   -> error:format({no_main_function_for_arity, length(Args)}, {lark});
                true    ->
                    Out = case catch erlang:apply(Mod, main, Args) of
                              {'EXIT', Err}     -> error:format({main_throw, Err}, {lark});
                              {error, Errs}     -> {error, Errs};
                              Res               -> {ok, Res}
                          end,
                    unload_modules([Mod]),
                    Out
            end
    end.


load(Source) -> load(Source, #{}).
load(Source, Options)  ->
    LibPaths = maps:get(libpaths, Options, []),
    RootName = "root_" ++ integer_to_list(erlang:phash2(Source)),
    Target = maps:get(target, Options, symbol:tag([source, list_to_atom(RootName)])),
    ParseLibPaths = [{path, P} || P <- LibPaths],
    case parser:parse([{text, RootName, Source}] ++ ParseLibPaths, Options) of
        {error, Errs}   -> {error, Errs};
        {ok, Parsed}    ->
            Libs = maps:from_list([{ModPath, Mod} || {module, _, ModPath, _, _, _} = Mod <- Parsed]),
            Mod = maps:get(symbol:path(Target), Libs),
            {module, _Ctx, _Path, _Imports, _Exports, _Defs} = Mod,
            %?debugVal(_Defs, 100),
            case monomorphize:module(Mod, Libs, Options) of
                {error, Errs}   -> {error, Errs};
                {ok, Module}    -> compile_and_load(Module)
            end
    end.

compile_and_load(Module) ->
    {module, _Ctx, _Path, _Imports, _Exports, _Defs} = Module,
    %?debugTerm(maps:get(main, _Defs)),
    case code_gen:gen(Module) of
        {error, Errs}               -> {error, Errs};
        {ok, ModuleForm}            ->
            %?debugVal(ModuleForm, 100),
            case compile(ModuleForm) of
                {error, Errs}       -> {error, Errs};
                {ok, ModuleName}    -> {ok, ModuleName}
            end
    end.

compile(Form) ->
    Options = [report, verbose, from_core],
    case compile_form(Form, Options) of
        {error, Errs}           -> {error, Errs};
        {ok, {ModuleName, Bin}} -> load_binary(ModuleName, Bin)
    end.

compile_form(Form, Options) ->
    ModuleName = cerl:atom_val(cerl:module_name(Form)),
    case compile:forms(Form, Options) of
        {ok, ModuleName, Bin}   -> {ok, {ModuleName, Bin}};
        Error                   -> error:format({compile_error, Error, ModuleName, Form}, {lark})
    end.

load_binary(ModuleName, Bin) ->
    BeamName = lists:flatten(io_lib:format("~w.beam", [ModuleName])),
    case code:load_binary(ModuleName, BeamName, Bin) of
        {module, ModuleName}    -> {ok, ModuleName};
        {error, Err}            -> error:format({loading_error, Err}, {lark, ModuleName})
    end.

unload_modules(Modules) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

-ifdef(TEST).

-define(setup(Code, Args, Tests), {setup, fun() -> lark:run(Code, Args) end, fun(_) -> none end, Tests}).

happy_path_test_() ->
    {"run code and see that it executed correctly",
     ?setup("def main -> boolean/False",
            [],
            fun(Res) -> 
                    [?test({ok, 'lark/prelude/boolean/False'}, Res)]
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
     ?setup("import boolean/_
             def main
               True -> False,
               False -> True",
            ['hello'],
            fun(Res) -> [?testError({no_matching_pattern, [hello]}, Res)] end)}.

main_throw_test_() ->
    {"when the main function throws, it should be caught and wrapped in an error",
     ?setup("import beam/erlang/raise\n"
            "def main arg -> raise('error', 'test', [])\n",
            ['hello'],
            fun(Res) -> [?testError({main_throw, {test, []}}, Res)] end)}.
-endif.
