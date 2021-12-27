-module(macros).
-export([expand/2]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

expand(Mod, Libs) ->
    % Go through code and for every application, check in libs if it's a macro.
    % If it is, return in Env. Then create macro module with all macros called
    % from this module and the same imports as the original module. Then
    % compile the macro module to byte code and use it to expand macros.
    % Finally decompile the macro module.
    case create_macro_module(Mod, Libs) of
        {error, Errs}                   -> {error, Errs};
        {ok, no_macros}                 -> {ok, Mod};   
        {ok, {MacroEnv, MacroModName}}  ->
            Pre = fun(_, _, Term) -> expand_macros(MacroModName, MacroEnv, Term) end,
            Post = fun(_, _, _) -> ok end,
            case ast:traverse(Pre, Post, Mod) of
                {error, Errs}                                       -> {error, Errs};
                {ok, {_Env, {module, _, _, _, _, _Defs} = ResMod}}  ->
                    true = code:soft_purge(MacroModName),
                    true = code:delete(MacroModName),
                    %OnlyDefs = maps:from_list([{P, D} || {P, {def, _, _, _} = D} = maps:to_list(Defs)]),
                    %{ok, setelement(6, ResMod, OnlyDefs)}
                    {ok, ResMod}
            end
    end.

create_macro_module({module, Ctx, ModulePath, Imports, _, _Defs} = Mod, Libs) ->
    Pre = fun(_, _, _) -> ok end,
    Post = fun(_, _, Term) -> mark_macros(Libs, Term) end,
    case ast:traverse(Pre, Post, Mod) of
        {error, Errs}                               -> {error, Errs};
        {ok, {Empty, _}} when map_size(Empty) == 0  -> {ok, no_macros};
        {ok, {Env, _}}                              ->
            MacroDefs = maps:from_list([{module:beam_name(Path), Term} || {Path, Term} <- maps:to_list(Env)]),
            MacroExports = maps:from_list([{Name, {export, symbol:ctx(Def), [Name], none}} ||
                                           {Name, Def} <- maps:to_list(MacroDefs)]),
            MacroMod = {module, Ctx, ModulePath ++ [macros], Imports, MacroExports, MacroDefs},
            case lark:compile_and_load(MacroMod, Libs, #{}) of
                {error, Errs}       -> {error, Errs};
                {ok, MacroModName}  -> MacroEnv = maps:from_list(lists:zip(maps:keys(Env), maps:keys(MacroDefs))),
                                       {ok, {MacroEnv, MacroModName}}
            end
    end.
    
mark_macros(Libs, {qualified_application, _, ModulePath, Name, _Args}) ->
    {module, _, _, _, _, Defs} = maps:get(ModulePath, Libs),
    case maps:get(Name, Defs) of
        {macro, MCtx, _, MExpr} -> Path = ModulePath ++ [Name],
                                   {ok, Path, {def, MCtx, module:beam_name(Path), MExpr}};
        _                       -> ok
    end;
mark_macros(_, _) -> ok.

expand_macros(MacroModName, Scope, {qualified_application, Ctx, ModulePath, Name, Args}) ->
    Path = ModulePath ++ [Name],
    case maps:get(Path, Scope, undefined) of
        undefined   -> ok;
        MacroName   -> case erlang:function_exported(MacroModName, MacroName, length(Args)) of
                           true    -> {ok, erlang:apply(MacroModName, MacroName, Args)};
                           false   -> error:format({macro_undefined_for_arity, Name, length(Args)}, {macros, Ctx})
                       end
    end;
expand_macros(_, _, _) -> ok.
    

-ifdef(TEST).

-define(options, #{target => [lark, test],
                   include_lark_libraries => false}).

-define(setup(Code, Tests), {setup, fun() -> lark:load(Code, ?options) end, fun clean/1, Tests}).

clean({ok, M}) -> true = code:soft_purge(M),
                  true = code:delete(M);
clean(_) -> noop.


happy_macro_test_() ->
    ?setup("module m1 (
                export {inc}
                import beam/erlang/+
                macro inc #('value', ctx, typ, n) -> #('value', ctx, typ, n + 1))

            module lark/test (export {test}
                import m1/inc
                def test -> 4.inc)",
           fun({ok, _}) ->
                   [?test(5, lark_test:test())]
           end).

lib_macro_test_() ->
    ?setup("module m1 (
                export {inc}
                import beam/erlang/+
                macro inc #('value', ctx, typ, n) -> #('value', ctx, typ, n + 1))

            module m2 (export {n}
                import m1/inc
                def n -> 4.inc)

            module lark/test (
                export {test}
                import m2
                def test -> m2/n)",
           fun({ok, _}) ->
                   [?test(5, lark_test:test())]
           end).

unhappy_macro_test_() ->
    ?setup("module lark/test (export { test }
                import beam/erlang/+

                macro inc #('value', ctx, typ, n)  -> #('value', ctx, typ, n + 1)
                def test -> 4.inc(2))",
           fun(Err) ->
                   [?testError({macro_undefined_for_arity, inc, 2}, Err)]
           end).

-endif.
