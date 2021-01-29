-module(module).
-export([parse/1, beam_name/1, kind_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

parse(Sources) ->
    case error:collect([prepare(FileName, AST) || {FileName, AST} <- Sources]) of
        {error, Errs}           -> {error, Errs};
        {ok, ModuleList}        ->
            Modules = [{Path, Module, File} || {File, Ms} <- ModuleList,
                                               {module, _, Path, _, _, _, _} = Module <- Ms],
            KeyF = fun({Path, _, _}) -> module:beam_name(Path) end,
            ErrF = fun({{Path, Mod1, File1}, {Path, Mod2, File2}}) ->
                           error:format({duplicate_module, module:kind_name(Path), File1, File2},
                                        {module, Mod1, Mod2}) end,
            case utils:duplicates(Modules, KeyF) of
                []      -> ModuleMap = maps:from_list([{beam_name(M), M} || {_, M, _} <- Modules]),
                           {ok, ModuleMap};
                Dups    -> error:collect([ErrF(D) || D <- Dups])
            end
    end.

prepare(FileName, Code) ->
    Modules = [M || M = {module, _, _, _, _} <- Code],
    Imports = [I || I = {import, _, _} <- Code],
    DefList = [D || D = {Type, _, _, _} <- Code, Type == type_def orelse Type == def orelse Type == macro],
    RootName = list_to_atom(FileName),
    case types(DefList) of
        {error, Errs}   -> {error, Errs};
        {ok, RootTypes} ->
            case error:collect([parse_module(M, Imports, DefList, RootTypes, RootName) || M <- Modules]) of
                {error, Errs}   -> {error, Errs};
                {ok, Mods}      -> RootModule = root_module(RootName, Imports, DefList, RootTypes),
                                   SubModules = lists:flatten([sub_modules(M) || M <- [RootModule | Mods]]),
                                   {ok, {FileName, [RootModule | Mods ++ SubModules]}}
            end
    end.

root_module(RootName, Imports, DefList, Types) ->
    Defs = maps:from_list([{Name, Def} || {_, _, Name, _} = Def <- DefList]),
    Exports = maps:from_list([{Name, {export, Ctx, [Name], none}} || {_, Ctx, Name, _} <- DefList]),
    {module,
     #{filename => atom_to_list(RootName), line => 0},
     [RootName],
     Imports,
     Exports,
     Defs,
     Types}.


parse_module({module, ModuleCtx, Path, Exports, Statements}, RootImports, RootDefs, RootTypes, RootName) ->
    LocalImports = [I || I = {import, _, _} <- Statements],
    LocalDefMap = maps:from_list([{Name, D} || D = {Type, _, Name, _} <- Statements,
                                             Type == type_def orelse Type == def orelse Type == macro]),
    RootDefMap = maps:from_list([{Name, T} || T = {_, _, Name, _} <- RootDefs]),
    
    case types(Statements) of
        {error, Errs}       -> {error, Errs};
        {ok, LocalTypes}    ->

            Types = maps:merge(RootTypes, LocalTypes),
            DefMap = maps:merge(RootDefMap, LocalDefMap),

            case error:collect([parse_export(E, Types, DefMap) || E <- Exports]) of
                {error, Errs}       -> {error, Errs};
                {ok, ExportTerms}   ->
                    ExportMap = maps:from_list(ExportTerms),
                    ModulePath = [S || {symbol, _, _, S} <- Path],

                    % Import all defs in the source file defined outside a module
                    % alongside the explicitly stated imports within and outside the
                    % module
                    RootImport = {import, ModuleCtx, [{symbol, ModuleCtx, variable, RootName},
                                                      {symbol, ModuleCtx, variable, '_'}]},
                    Imports = [RootImport | LocalImports ++ RootImports],

                    % A module can export functions defined within the module or
                    % outside it in the source file. For functions defined outside the
                    % module, we create a function stub in the module which calls the
                    % function in the root module
                    Exported = [{DefName, apply_def_term(DefName, arity(Term), Ctx)}
                                || {DefName, Term} <- maps:to_list(DefMap),
                                   maps:is_key(DefName, ExportMap),
                                   maps:is_key(DefName, RootDefMap),
                                   {export, Ctx, _, _} <- [maps:get(DefName, ExportMap)]],

                    Defs = maps:merge(maps:from_list(Exported), LocalDefMap),
                    {ok, {module, ModuleCtx, ModulePath, Imports, ExportMap, Defs, LocalTypes}}
            end
    end.


% For each export we transform the term to an `export` term and check that if
% the export already exists or if the definition is missing
parse_export({pair, Ctx, K, V}, Types, DefMap) ->
    error:map(parse_export(K, Types, DefMap), fun({T, {export, _, Key, none}}) -> {T, {export, Ctx, Key, V}} end);

% Boolean/True
parse_export({qualified_symbol, Ctx, Symbols} = Elem, Types, DefMap) when (length(Symbols) == 2) ->
    [P, T] = [S || {symbol, _, _, S} <- Symbols],
    case maps:is_key(P, DefMap) andalso 
         maps:is_key(P, Types) andalso
         lists:member(T, [symbol:tag(C) || C <- maps:get(P, Types)]) of
        false -> error:format({export_missing, module:kind_name([P, T])}, {module, Elem});
        true  -> case maps:is_key(T, DefMap) of
                     false  -> {ok, {T, {export, Ctx, [P, T], none}}};
                     true   -> error:format({export_already_defined, symbol:tag([P, T]), T}, {module, Elem})
                 end
    end;

% Unsupported qualified symbol
parse_export({qualified_symbol, _, Symbols} = Elem, _Types, _DefMap) ->
    error:format({export_unsupported, kind_name([S || {_, _, _, S} <- Symbols])},
                 {module, Elem});

% any other symbol (say `blah`)
parse_export({symbol, Ctx, _, Val} = Elem, _Types, DefMap) ->
    case maps:is_key(symbol:tag(Elem), DefMap) of
        true  -> {ok, {symbol:tag(Elem), {export, Ctx, [Val], none}}};
        false -> error:format({export_missing, symbol:tag(Elem)}, {module, Elem})
    end.


% If we export 'Boolean' in 'prelude', then we want to be able to import
% 'prelude/Boolean/_'.  To do this, we need a module for 'prelude/Boolean',
% which exports anything defined by 'Boolean'.
sub_modules({module, BaseCtx, BasePath, BaseImports, _BaseExports, BaseDefMap, BaseTypes}) ->
    F = fun(Parent, Children) ->
                BaseImport = {import, BaseCtx, [{symbol, BaseCtx, variable, S} || S <- BasePath ++ ['_']]},
                Imports = [BaseImport | BaseImports],
                Exports = maps:from_list([{symbol:tag(T), T} || T <- Children]),
                Path = BasePath ++ [Parent],

                % We use the type env in `tagged_gen` to determine if a type is
                % a literal. However, at this point we don't know if a symbol
                % refers to a type constant (i.e. a literal) or an imported
                % type. We do know though, that any type definition we create
                % for an imported type will be removed in the `tagger` module.
                % For this reason, we only include tagged type symbols in the
                % type env.
                SubTypeEnv = maps:from_list([{symbol:tag(T), T}
                                             || {tagged, _, _, _} = T <- lists:flatten(maps:values(BaseTypes))]),
                TypeEnv = maps:merge(BaseDefMap, SubTypeEnv),
                ChildDefs = [tagged_gen:term(TypeEnv, Term) || Term <- Children],
                DefMap = maps:from_list([{symbol:tag(D), D} || D <- ChildDefs]),

                Ctx = symbol:ctx(maps:get(Parent, BaseDefMap)),
                {module, Ctx, Path, Imports, Exports, DefMap, #{}}
        end,
    [F(T, Members) || {T, Members} <- maps:to_list(BaseTypes)].


arity({type_def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> length(Ps);
arity({type_def, _, _, _}) -> 0.

apply_def_term(Name, 0, Ctx) -> 
    {def, Ctx, Name,
     {application, Ctx, {symbol, Ctx, 'variable', Name}, []}};
apply_def_term(Name, Arity, Ctx) -> 
    Args = [{symbol, Ctx, 'variable', symbol:id('')} || _ <- lists:seq(1, Arity)],
    {def, Ctx, Name,
     {'fun', Ctx,
      [{clause, Ctx, Args,
        {application, Ctx, {symbol, Ctx, 'variable', Name}, Args}}]}}.

% At this point in the compilation, we don't know if a symbol is defined
% elsewhere or created within this def. At this point, we assume any symbol is
% a new constant, and then when tagging, we remove the ones that have already
% been defined.
types_post(expr, _, {symbol, _, type, Name} = Term) -> 
    Parent = ast:get_tag(parent, Term),
    % Don't include recursive types
    case Parent == Name of
        true    -> {ok, Term};
        false   -> {ok, {Parent, Term}, Term}
    end;
types_post(expr, _, {tagged, _Ctx, _Path, _Val} = Term) -> 
    Parent = ast:get_tag(parent, Term),
    {ok, {Parent, Term}, Term};
types_post(_, _, _) -> ok.


types([]) -> {ok, maps:from_list([])};
types(AST) ->
    case ast:traverse(fun types_post/3, AST) of
        {error, Errs}   -> 
            {error, Errs};
        {ok, {Env, _}}  -> 
            GetKey = fun({K, _}) -> K end,
            GetVal = fun({_, V}) -> V end,
            Types = maps:from_list(utils:group_by(GetKey, GetVal, maps:keys(Env))),
            {ok, Types}
    end.

beam_name({module, _, Path, _, _, _, _}) -> beam_name(Path);
beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name({module, _, Path, _, _, _, _}) -> kind_name(Path);
kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).
