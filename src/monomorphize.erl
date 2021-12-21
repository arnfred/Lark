-module(monomorphize).
-export([module/2, module/3]).

module(Mod, Libs) -> module(Mod, Libs, #{}).
module({module, Ctx, ModulePath, Imports, Exports, Defs}, Libs, _Options) ->
    % Collect definitions to linearize
    Targets = [{Name, Def} || {Name, Def} <- maps:to_list(Defs), is_target(Name, Exports)],
    % Linearize definitions
    case linearize_targets(Targets, Libs) of
        {error, Errs}            -> {error, Errs};
        {ok, Env, LocalTreeMap}  -> 
            GlobalTreeMap = maps:from_list([{Name, wrap_in_def(Name, Tree)} || {{Path, _}, Tree} <- maps:to_list(Env),
                                                                               length(Path) > 1,
                                                                               Name <- [symbol:tag(Path)]]),
            Trees = maps:merge(LocalTreeMap, GlobalTreeMap),
            NewExports = case maps:is_key(main, Defs) of
                             true   -> maps:put(main, {}, Exports);
                             false  -> Exports
                         end,
            {ok, {module, Ctx, ModulePath, Imports, NewExports, Trees}}
    end.


linearize_targets(Targets, Libs) -> linearize_targets(Targets, #{}, Libs, []).
linearize_targets([{Name, Def} | Rest], Env, Libs, Res) ->
    case linearize:term(Def, Libs, Env) of
        {error, Errs}           -> linearize_targets(Rest, Env, Libs, [{error, Errs} | Res]);
        {ok, {NewEnv, Tree}}    -> linearize_targets(Rest, NewEnv, Libs, [{ok, {Name, Tree}} | Res])
    end;
linearize_targets([], Env, _, Res) -> 
    case error:collect(Res) of
        {error, Errs}           -> {error, Errs};
        {ok, TreeList}          -> {ok, Env, maps:from_list(TreeList)}
    end.


is_target(main, _) -> true;
is_target(Name, Exports) -> maps:is_key(Name, Exports).

wrap_in_def(Name, Tree) -> {def, symbol:ctx(Tree), Name, Tree}.


