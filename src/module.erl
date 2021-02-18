-module(module).
-export([parse/1, beam_name/1, kind_name/1]).

parse(Sources) ->
    case error:collect([prepare(FileName, AST) || {FileName, AST} <- Sources]) of
        {error, Errs}           -> {error, Errs};
        {ok, ModuleList}        ->
            Modules = [{Path, Module, File} || {File, Ms} <- ModuleList,
                                               {module, _, Path, _, _, _} = Module <- Ms],
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
    DefList = [D || D = {Type, _, _, _} <- Code, Type == type_def orelse Type == def orelse Type == macro],
    RootName = list_to_atom(FileName),
    RootImports = [I || I = {import, _, _} <- Code],
    case types(DefList, RootImports) of
        {error, Errs}   -> {error, Errs};
        {ok, RootTypes} ->
            Root = root_module_and_types(RootName, RootImports, RootTypes, DefList),
            case error:collect([parse_module(M, Root) || M <- Modules]) of
                {error, Errs}       -> {error, Errs};
                {ok, ModsAndTypes}  -> SubModules = lists:flatten([sub_modules(M, Ts) || {M, Ts} <- [Root | ModsAndTypes]]),
                                       {Mods, _} = lists:unzip([Root | ModsAndTypes]),
                                       {ok, {FileName, Mods ++ SubModules}}
            end
    end.

root_module_and_types(RootName, RootImports, Types, DefList) ->
    Ctx = #{filename => atom_to_list(RootName), line => 0, module => RootName},
    Defs = maps:from_list([{Name, Def} || {_, _, Name, _} = Def <- DefList]),
    Imports = RootImports ++ [{import, Ctx, [{symbol, Ctx, type, Parent}]} || Parent <- maps:keys(Types)],
    Exports = maps:from_list([{Name, {export, DefCtx, [Name], none}} || {_, DefCtx, Name, _} <- DefList]),
    {tag_symbols({module, Ctx, [RootName], Imports, Exports, Defs}), Types}.


parse_module({module, ModuleCtx, Path, Exports, Statements},
             {{module, _RootCtx, RootPath, RootImports, _, RootDefs}, RootTypes}) ->
    LocalImports = [I || I = {import, _, _} <- Statements],
    LocalDefMap = maps:from_list([{Name, D} || D = {Type, _, Name, _} <- Statements,
                                             Type == type_def orelse Type == def orelse Type == macro]),
    RootDefMap = maps:from_list([{Name, T} || T = {Type, _, Name, _} <- maps:values(RootDefs),
                                             Type == type_def orelse Type == def orelse Type == macro]),
    
    case types(Statements, LocalImports) of
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
                    % module and finally all types defined within the module
                    RootImport = {import, ModuleCtx,
                                  [{symbol, ModuleCtx, variable, P} || P <- RootPath] ++
                                  [{symbol, ModuleCtx, variable, '_'}]},
                    TypeImports = [{import, ModuleCtx, [{symbol, ModuleCtx, type, Parent}]} ||
                                   Parent <- maps:keys(LocalTypes)],
                    Imports = [RootImport | RootImports ++ TypeImports ++ LocalImports],

                    {ok, {tag_symbols({module, ModuleCtx, ModulePath, Imports, ExportMap, LocalDefMap}), LocalTypes}}
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
sub_modules({module, BaseCtx, BasePath, BaseImports, BaseExports, BaseDefMap}, BaseTypes) ->
    F = fun(Parent, Children) ->
                % Any function defined in the base module could be called from
                % a definition in the sub module, so the base module needs to
                % be in scope.
                %
                % A def might have the same name as a subtype in the def. For
                % this reason we import only the base exports that don't match
                % the names of any of the children
                ChildMap = maps:from_list([{symbol:name(C), true} || C <- Children]),
                BaseExportsNotInChildren = [E || {K, E} <- maps:to_list(BaseExports), not(maps:is_key(K, ChildMap))],
                BaseImportDict = {dict, BaseCtx, [{symbol, BaseCtx, variable, lists:last(P)}
                                                  || {export, _, P, _} <- BaseExportsNotInChildren]},
                ImportOfBaseItself = {import, BaseCtx,
                                      [{symbol, BaseCtx, variable, S} || S <- BasePath] ++ [BaseImportDict]},
                BaseImportsExceptParent = [Import || {import, _, [{symbol, _, _, S} | _]} = Import <- BaseImports,
                                                    not(S =:= Parent)],
                Imports = [ImportOfBaseItself | BaseImportsExceptParent],
                Exports = maps:from_list([{symbol:name(T), {export, symbol:ctx(T), [symbol:name(T)], none}} || T <- Children]),
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
                DefMap = maps:from_list([{symbol:name(D), D} || D <- ChildDefs]),

                Ctx = symbol:ctx(maps:get(Parent, BaseDefMap)),
                tag_symbols({module, Ctx, Path, Imports, Exports, DefMap})
        end,
    [F(T, Members) || {T, Members} <- maps:to_list(BaseTypes)].


arity({def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> length(Ps);
arity({def, _, _, _}) -> 0;
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
% elsewhere or created within this def. We assume any symbol is a new constant,
% and then when tagging, we remove the ones that have already been defined.
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


types([], _) -> {ok, maps:from_list([])};
types(Defs, Imports) ->
    case ast:traverse(fun types_post/3, Defs) of
        {error, Errs}   -> 
            {error, Errs};
        {ok, {Env, _}}  -> 
            GetKey = fun({K, _}) -> K end,
            GetVal = fun({_, V}) -> V end,
            TypeList = utils:group_by(GetKey, GetVal, maps:keys(Env)),
            % Pick up any def for which there is a local import
            LocallyImported = maps:from_list([{P, Cs} || {P, Cs} <- TypeList, is_local_wildcard_import(P, Imports)]),
            % Then filter out any of the child types of the local imports from any other types
            Types = maps:from_list([{P, Fs} || {P, Cs} <- TypeList,
                                               Fs <- [filter_imported(LocallyImported, P, Cs)],
                                               length(Fs) > 0]),
            {ok, Types}
    end.


is_local_wildcard_import(_, []) -> false;
is_local_wildcard_import(Type, [{import, _, Symbols} | Rest]) when length(Symbols) > 1 ->
    case lists:nthtail(length(Symbols) - 2, Symbols) of
        [{symbol, _, _, Type}, {symbol, _, _, '_'}] -> true;
        _                                           -> is_local_wildcard_import(Type, Rest)
    end;
is_local_wildcard_import(Type, [_ | Rest]) -> is_local_wildcard_import(Type, Rest).

filter_imported(AllImported, P, Terms) ->
    AllImportedExceptChildrenOfP = lists:flatten(maps:values(maps:remove(P, AllImported))),
    ConstantMap = maps:from_list([{symbol:tag(C), true} || C <- AllImportedExceptChildrenOfP]),
    [T || T <- Terms, not(maps:is_key(symbol:tag(T), ConstantMap))].


tag_symbols({module, _, Path, _, _, _} = Mod) ->
    {ok, {_, Tagged}} = ast:traverse(fun(_, _, Term) -> tag_symbols_post(Path, Term) end, Mod),
    Tagged.

tag_symbols_post(ModulePath, {symbol, _, _, _} = Term)  -> {ok, ast:tag(module, Term, ModulePath)};
tag_symbols_post(_, _)                                  -> ok.

beam_name({module, _, Path, _, _, _}) -> beam_name(Path);
beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

kind_name({module, _, Path, _, _, _}) -> kind_name(Path);
kind_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).
