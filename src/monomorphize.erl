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
            GlobalTreeMap = maps:from_list([{path_to_name(Path), Tree} || {{Path, _}, Tree} <- maps:to_list(Env)]),
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
        {error, Errs}               -> linearize_targets(Rest, Env, Libs, [{error, Errs} | Res]);
        {ok, {_, {'fun', _, F}}}    ->
            case F(args(Def), []) of
                {error, Errs}           -> linearize_targets(Rest, Env, Libs, [{error, Errs} | Res]);
                {ok, {NewEnv, FunTree}} -> 
                    DefTree = setelement(4, Def, FunTree),
                    linearize_targets(Rest, NewEnv, Libs, [{ok, {Name, DefTree}} | Res])
            end
    end;
linearize_targets([], Env, _, Res) -> 
    case error:collect(Res) of
        {error, Errs}           -> {error, Errs};
        {ok, TreeList}          -> {ok, Env, maps:from_list(TreeList)}
    end.


is_target(main, _) -> true;
is_target(Name, Exports) -> maps:is_key(Name, Exports).

path_to_name(Path) -> symbol:tag(Path).

args({def, _, _, {'fun', _, []}}) -> [];
args({def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> [whatever || _ <- Ps];
args({def, _, _, _Expr}) -> [].


