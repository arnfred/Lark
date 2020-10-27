-module(macros).
-export([expand/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

expand({ast, Ctx, Modules, Imports, Defs} = AST) ->
    Macros = maps:from_list([{Name, {def, MacroCtx, Name, Args, Expr}} ||
                              {macro, MacroCtx, Name, Args, Expr} <- maps:values(Defs)]),
    case maps:size(Macros) of
        0   -> {ok, AST};
        _N  ->
            Module = symbol:id('macros'),
            MacroAST = {ast, Ctx, [{module, Ctx, [Module], Macros}], Imports, Macros},
            case kind:type_compile_load(MacroAST, #{}) of
                {error, Errs}       -> {error, Errs};
                {ok, LoadedModules} -> 
                    NoMacros = maps:filter(fun(_, {T, _, _, _, _}) -> not(T =:= macro) end, Defs),
                    NoMacroAST = {ast, Ctx, Modules, Imports, NoMacros},
                    case ast:traverse(fun(_, _, Term) -> expand_macro(Module, Term) end, NoMacroAST) of
                        {error, Errs}               -> clean(LoadedModules), {error, Errs};
                        {ok, {_Env, ExpandedAST}}   -> clean(LoadedModules), {ok, ExpandedAST}
                    end
            end
    end.

expand_macro(Module, {macro_application, Ctx, Name, Args}) ->
    case erlang:function_exported(Module, Name, length(Args)) of
        true    -> {ok, erlang:apply(Module, Name, Args)};
        false   -> error:format({wrong_macro_arity, Name, length(Args)}, {macros, Ctx})
    end;
expand_macro(_, _) -> ok.
    
clean({error, _}) -> noop;
clean({ok, Modules}) -> clean(Modules);
clean(Modules) ->
    F = fun(M) -> true = code:soft_purge(M),
                  true = code:delete(M)
        end,
    [F(M) || M <- Modules].

-ifdef(TEST).


-define(setup(Code, Tests), {setup, fun() -> kind:load(Code, #{add_kind_libraries => false}) end, fun clean/1, Tests}).


happy_macro_test_() ->
    ?setup("module test { test }
            import erlang/{+, list_to_tuple: tuple}
            macro inc _
              | ['value', ctx, typ, n]  -> ['value', ctx, typ, n + 1].tuple
              | otherwise               -> otherwise

            def test -> 4.inc",
           fun({ok, _}) ->
                   [?test(5, test:test())]
           end).

unhappy_macro_test_() ->
    ?setup("module test { test }
            import erlang/{+, list_to_tuple}
            macro inc _
              | ['value', ctx, typ, n]  -> ['value', ctx, typ, n + 1].list_to_tuple
              | otherwise               -> otherwise

            def test -> 4.inc(2)",
           fun(Err) ->
                   [?testError({wrong_macro_arity, inc, 2}, Err)]
           end).

-endif.
