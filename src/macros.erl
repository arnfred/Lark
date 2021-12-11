-module(macros).
-export([expand/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

expand(Mod) ->
    case ast:traverse(fun expand_macros_pre/3, fun expand_macros_post/3, Mod) of
        {error, Errs}               -> {error, Errs};
        {ok, {_Env, ExpandedMod}}   -> {ok, ExpandedMod}
    end.

expand_macros_pre(_, _, {application, _, {macro_symbol, _, _, _}, _}) -> leave_intact;
expand_macros_pre(_, _, _) -> ok.

expand_macros_post(_, _, {application, Ctx, {macro_symbol, _, Path, Name}, Args}) ->
    ModuleName = module:beam_name(Path),
    case erlang:function_exported(ModuleName, Name, length(Args)) of
        true    -> {ok, erlang:apply(ModuleName, Name, Args)};
        false   -> error:format({macro_undefined_for_arity, Name, length(Args)}, {macros, Ctx})
    end;
expand_macros_post(_, _, {macro_symbol, Ctx, Path, Name}) ->
    ModuleName = module:beam_name(Path),
    case erlang:function_exported(ModuleName, Name, 0) of
        true    -> {ok, erlang:apply(ModuleName, Name, [])};
        false   -> error:format({macro_undefined_for_arity, Name, 0}, {macros, Ctx})
    end;
expand_macros_post(_, _, _) -> ok.
    
-ifdef(TEST).


-define(setup(Code, Tests), {setup, fun() -> kind:load(Code, #{import_kind_libraries => false}) end, fun (_) -> noop end, Tests}).


happy_macro_test_() ->
    ?setup("module test { test }
            import beam/erlang/{+, list_to_tuple: tuple}

            macro inc ['value', ctx, typ, n]  -> ['value', ctx, typ, n + 1].tuple
            def test -> 4.inc",
           fun({ok, _}) ->
                   [?test(5, test:test())]
           end).

unhappy_macro_test_() ->
    ?setup("module test { test }
            import beam/erlang/{+, list_to_tuple}

            macro inc ['value', ctx, typ, n]  -> ['value', ctx, typ, n + 1].list_to_tuple
            def test -> 4.inc(2)",
           fun(Err) ->
                   [?testError({macro_undefined_for_arity, inc, 2}, Err)]
           end).

-endif.
