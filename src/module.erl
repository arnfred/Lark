-module(module).
-export([parse/1, path/1, beam_name/1, lark_name/1, is_submodule/1, empty/1]).

-include("test/macros.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(FILEEXT, ".lark").

parse(Sources) ->
    case error:collect([prepare(FileName, AST) || {FileName, AST} <- Sources]) of
        {error, Errs}           -> {error, Errs};
        {ok, ModuleList}        ->
            Modules = [{Path, Module, File} || {File, Ms} <- ModuleList,
                                               {module, _, Path, _, _, _} = Module <- Ms],
            KeyF = fun({Path, _, _}) -> module:beam_name(Path) end,
            ErrF = fun({{Path, Mod1, File1}, {Path, Mod2, File2}}) ->
                           error:format({duplicate_module, module:lark_name(Path), File1, File2},
                                        {module, Mod1, Mod2}) end,
            case utils:duplicates(Modules, KeyF) of
                []      -> ModuleMap = maps:from_list([{path(M), M} || {_, M, _} <- Modules]),
                           {ok, ModuleMap};
                Dups    -> error:collect([ErrF(D) || D <- Dups])
            end
    end.

prepare(FileName, Code) ->
    RootModule = root_module(FileName, Code),
    Modules = [M || M = {module, _, _, _} <- Code],
    case error:collect([parse_module(M, Code) || M <- [RootModule | Modules]]) of
        {error, Errs}       -> {error, Errs};
        {ok, ModsAndTypes}  -> SubModules = lists:flatten([sub_modules(M, Ts) || {M, Ts} <- ModsAndTypes]),
                               {Mods, _} = lists:unzip(ModsAndTypes),
                               {ok, {FileName, Mods ++ SubModules}}
    end.

root_module(FileName, Statements) ->
    RootPath = filename_to_module_path(FileName),
    ModuleImports = [{import, Ctx, Path} || {module, Ctx, Path, ModStatements} <- Statements,
                                            {exports, _, Exports} <- ModStatements,
                                            length(Exports) > 0],
    RootName =lark_name([P || {symbol, _, _, P} <- RootPath]),
    Ctx = #{filename => FileName, line => 0, module => RootName},
    {module, Ctx, RootPath, Statements ++ ModuleImports}.

parse_module({module, Ctx, Path, Statements}, RootStatements) ->
    ModulePath = [S || {symbol, _, _, S} <- Path],
    Exports = lists:append([Exs || {exports, _, Exs} <- Statements]),
    RootImports = [I || I = {import, _, _} <- RootStatements],
    ModuleImports = [I || I = {import, _, _} <- Statements],
    Imports = [import(I) || I <- utils:unique(RootImports ++ ModuleImports)],
    DefMap = maps:from_list([{Name, D} || D = {Type, _, Name, _} <- Statements,
                                          Type == def orelse Type == macro]),
    
    case types(maps:values(DefMap), Imports, ModulePath) of
        {error, Errs}       -> {error, Errs};
        {ok, Types}    ->
            SubDefMap = sub_defs(Types),
            case error:collect([parse_export(E, Types, DefMap) || E <- Exports]) of
                {error, Errs}       -> {error, Errs};
                {ok, ExportTerms}   ->
                    ExportMap = maps:from_list(ExportTerms),
                    Module = {module, Ctx, ModulePath, Imports, ExportMap, maps:merge(SubDefMap, DefMap)},
                    {ok, {tag_symbols(Module), Types}}
            end
    end.

% If we export 'Boolean' in 'prelude', then we want to be able to import
% 'prelude/Boolean/_'.  To do this, we need a module for 'prelude/Boolean',
% which exports anything defined by 'Boolean'.
sub_modules({module, _, BasePath, _, _, _} = BaseMod, BaseTypes) ->
    [link_submodule(T, Members, BaseMod, BasePath) || {T, Members} <- maps:to_list(BaseTypes)].


sub_defs(Types) ->
    maps:from_list([{sub_symbol(Parent, Child), tagged_gen:term(Term)} ||
                    {Parent, Children} <- maps:to_list(Types), {Child, Term} <- Children]).

% For each export we transform the term to an `export` term and check that if
% the export already exists or if the definition is missing
parse_export({pair, Ctx, K, V}, Types, DefMap) ->
    error:map(parse_export(K, Types, DefMap), fun({T, {export, _, Key, none}}) -> {T, {export, Ctx, Key, V}} end);

% Boolean/True
parse_export({qualified_symbol, Ctx, Symbols} = Elem, Types, DefMap) when (length(Symbols) == 2) ->
    [P, T] = [S || {symbol, _, _, S} <- Symbols],
    case maps:is_key(P, DefMap) andalso 
         maps:is_key(P, Types) andalso
         lists:member(T, [C || {C, _} <- maps:get(P, Types)]) of
        false -> error:format({export_missing, module:lark_name([P, T])}, {module, Elem});
        true  -> {ok, {T, {export, Ctx, [P, T], none}}}
    end;

% Unsupported qualified symbol
parse_export({qualified_symbol, _, Symbols} = Elem, _Types, _DefMap) ->
    error:format({export_missing, lark_name([S || {_, _, _, S} <- Symbols])},
                 {module, Elem});

% any other symbol (say `blah`)
parse_export({symbol, Ctx, _, Val} = Elem, _Types, DefMap) ->
    case maps:is_key(symbol:tag(Elem), DefMap) of
        true  -> {ok, {symbol:tag(Elem), {export, Ctx, [Val], none}}};
        false -> error:format({export_missing, symbol:tag(Elem)}, {module, Elem})
    end.


% Sub module for keywords and tagged values exported from the base module, but not defined there
link_submodule(Parent, Children, {module, BaseCtx, BasePath, _, BaseExports, BaseDefs}, RootPath) ->
    F = fun({keyword, _, _, _} = Keyword) -> Keyword;
           (C) -> {link, symbol:ctx(C), RootPath, sub_symbol(Parent, symbol:name(C))} end,
    Links = maps:from_list([{Name, F(C)} || {Name, C} <- Children]),
    Exports = maps:from_list([{Name, {export, symbol:ctx(C), [Name], none}} || {Name, C} <- Children,
                                                                               maps:is_key(Parent, BaseExports)]),
    Imports = [],
    Ctx = case maps:get(Parent, BaseDefs, undefined) of
              undefined -> maps:put(submodule, true, BaseCtx);
              Def       -> maps:put(submodule, true, symbol:ctx(Def))
          end,
    tag_symbols({module, Ctx, BasePath ++ [Parent], Imports, Exports, Links}).

sub_symbol(Parent, ChildName) -> list_to_atom(atom_to_list(Parent) ++ "/" ++ atom_to_list(ChildName)).

% At this point in the compilation, we don't know if a symbol is defined
% elsewhere or created within this def. We assume any symbol is a new keyword,
% and then when tagging, we remove the ones that have already been defined.
types_post(expr, _, {symbol, _, keyword, Name} = Term) -> 
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


types([], _, _) -> {ok, maps:from_list([])};
types(Defs, Imports, Path) ->
    case ast:traverse(fun types_post/3, Defs) of
        {error, Errs}   -> 
            {error, Errs};
        {ok, {Env, _}}  -> 
            GetKey = fun({K, _}) -> K end,
            GetVal = fun({_, V}) -> V end,
            TypeList = utils:group_by(GetKey, GetVal, maps:keys(Env)),

            % If a type keyword has been imported locally, we other types that
            % refer to this keyword to represent it as a link to the type
            % where it was defined
            LocalImports = local_imports(Imports, TypeList),
            Link = fun(P, Cs) -> keywords(LocalImports, P, Cs, Path) end,
            Types = maps:from_list([{P, Link(P, Cs)} || {P, Cs} <- TypeList]),

            {ok, Types}
    end.

local_imports(Imports, TypeList) ->
    Tag = fun(Cs, '_')                  -> [{C, C} || C <- Cs];
             (_, Ts) when is_map(Ts)    -> maps:to_list(Ts);
             (Cs, T)                    -> [{C, C} || C <- Cs, C =:= T]
        end,
    TagChildren = fun(Cs, T) -> Tag([symbol:name(C) || C <- Cs], T) end,

    ImportList = [{P, K, V} || {import, _, [P, T]} <- Imports,
                               {Parent, Cs} <- TypeList,
                               Parent =:= P,
                               {K, V} <- TagChildren(Cs, T)],

    ExpandedImportList = expand_imports(ImportList),

    maps:from_list(utils:group_by(fun({P, _, _}) -> P end, fun({_, K, V}) -> {K, V} end, ExpandedImportList)).

% Local imports can be chained in the sense that a keyword imported from one
% def can be used in another def which in turn can be imported. Take the
% following example:
%
% ```
% def R -> (A | B)
% import R/_
% def S -> (C | B)
% import S/{B: D}
% def T -> (A | D)
% ```
%
% Here, T consists of `R/A` (since A is imported in the local scope), and "`T/D`"
% which is an import that maps to `S/D`. However, `D` is an alias for `B` which
% is declared by `R`. To correctly map `T` to `(R/A | R/B)` we need to make
% sure follow the chain of imports to where the keywords are originally created
expand_imports(ImportList) -> expand_imports(ImportList, #{}, []).
expand_imports([], _, Res) -> Res;
expand_imports([{P, K, V} = Triplet | Rest], Seen, Res) ->
    case maps:is_key(Triplet, Seen) of
        true    -> expand_imports(Rest, Seen, [Triplet | Res]);
        false   -> {Linked, Unlinked} = lists:partition(fun({_, Key, _}) -> V =:= Key end, Rest),
                   New = [{P, K, Val} || {_, _, Val} <- Linked],
                   expand_imports(Unlinked ++ [Triplet] ++ New, maps:put(Triplet, true, Seen), Res)
    end.

keywords(Imported, P, Terms, BasePath) ->
    Imps = maps:from_list([{Alias, {Parent, Name}} || {Parent, Cs} <- maps:to_list(maps:remove(P, Imported)),
                                                      {Name, Alias} <- Cs]),

    F = fun({symbol, Ctx, keyword, S}) when is_map_key(S, Imps) -> 
                {Parent, Name} = maps:get(S, Imps),
                {keyword, Ctx, BasePath ++ [Parent], Name};
           ({symbol, Ctx, keyword, S}) ->
                {keyword, Ctx, BasePath ++ [maps:get(parent, Ctx)], S};
           ({tagged, Ctx, Path, Term}) ->
                {tagged, Ctx, Path, Term}
        end,

    [{symbol:name(T), F(T)} || T <- Terms].


tag_symbols({module, _, Path, _, _, _} = Mod) ->
    {ok, {_, Tagged}} = ast:traverse(fun(_, _, Term) -> tag_symbols_post(Path, Term) end, Mod),
    Tagged.

tag_symbols_post(ModulePath, {symbol, _, _, _} = Term)  -> {ok, ast:tag(module, Term, ModulePath)};
tag_symbols_post(_, _)                                  -> ok.

path({module, _, Path, _, _, _}) -> Path.
beam_name({module, _, _, _, _, _} = Mod) -> beam_name(path(Mod));
beam_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('_', Path)],
    list_to_atom(lists:flatten([PathString])).

lark_name({module, _, _, _, _, _} = Mod) -> lark_name(path(Mod));
lark_name(Path) ->
    PathString = [atom_to_list(A) || A <- lists:join('/', Path)],
    list_to_atom(lists:flatten([PathString])).

filename_to_module_path(FileName) ->
    FilePath = lists:map(fun erlang:list_to_atom/1, filename:split(FileName)),
    FileTail = case length(FilePath) > 2 of
                   true     -> lists:nthtail(length(FilePath) - 2, lists:droplast(FilePath));
                   false    -> lists:droplast(FilePath)
               end,
    BaseName = list_to_atom(filename:basename(FileName, ?FILEEXT)),
    [{symbol, #{}, variable, P} || P <- [source] ++ FileTail ++ [BaseName]].

import({import, Ctx, Path}) -> {import, Ctx, import(Path, [])}.
import([], Path) -> lists:reverse(Path);
import([{symbol, _, _, P} | Rest], Path) -> import(Rest, [P | Path]);
import([{dict, _, Pairs} | Rest], Path) -> import(Rest, [import_dict_pairs(Pairs) | Path]).
import_dict_pairs(Pairs) -> maps:from_list(import_dict_pairs(Pairs, [])).
import_dict_pairs([], Pairs) -> lists:reverse(Pairs);
import_dict_pairs([{symbol, _, _, P} | Rest], Pairs) -> import_dict_pairs(Rest, [{P, P} | Pairs]);
import_dict_pairs([{pair, _, {symbol, _, _, K}, {symbol, _, _, V}} | Rest], Pairs) ->
    import_dict_pairs(Rest, [{K, V} | Pairs]).


is_submodule({module, Ctx, _, _, _, _}) -> maps:get(submodule, Ctx, false).

empty({module, _, _, _, Exports, Defs}) -> maps:size(Exports) =:= 0 andalso maps:size(Defs) =:= 0.
