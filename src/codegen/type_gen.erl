-module(type_gen).
-export([gen/3]).

gen(TypesEnv, ArgsEnv, AST) ->
    case gen_types(TypesEnv, AST) of
        {error, Errs}    -> {error, Errs};
        {ok, {Types, _}} -> gen_modules(Types, TypesEnv, ArgsEnv, AST)
    end.

gen_types(TypesEnv, AST) ->
    Pre = fun(Type, Scope, Term) -> pre_gen_term(TypesEnv, Type, Scope, Term) end,
    Post = fun(_, _, _) -> ok end,
    ast:traverse(Pre, Post, AST).

pre_gen_term(_, top_level, _, {def, _, _, _, _}) -> skip;
pre_gen_term(TypesEnv, top_level, _, Term)       -> {change, expr_gen:gen(TypesEnv), Term};
pre_gen_term(TypesEnv, pattern, _, Term)         -> {change, pattern_gen:gen(TypesEnv), Term};
pre_gen_term(TypesEnv, expr, _, Term)            -> {change, expr_gen:gen(TypesEnv), Term};
pre_gen_term(_, _, _, _)                         -> ok.


gen_modules(Types, TypesEnv, ArgsEnv, {ast, _, Modules, _, _} = AST) ->
    % Create a map from types to type members
    F = fun([Parent | _] = Path, {tagged, _, _, _} = Term) ->
                Tag = symbol:tag(Term),
                Name = lists:last(Path),
                Args = maps:get(Tag, ArgsEnv),
                {Parent, {Name, Tag, Args}};
           ([Parent | _], {type, _, Name, _} = T) ->
                Tag = symbol:tag(T),
                {Parent, {Name, Tag, []}}
        end,
    TypePaths = [F(Path, Term) || {Path, Term} <- maps:to_list(TypesEnv), length(Path) > 1],
    TypeParents = utils:group_by(fun({K, _}) -> K end,
                                 fun({_, V}) -> V end,
                                 TypePaths),

    % For each top level type (e.g. `Boolean`) we create a module which exports
    % its type members (e.g. `True` and `False`). These type modules serve to
    % simplify the import logic by making it trivial to import type members
    % from compiled beam files. For a bit more context see commit
    % 34ea58050b0200236324e963cd14ceb58ac1a32d
    TypeModules = [gen_type_modules(Modules, Parent, Children, Types)
                   || {Parent, Children} <- TypeParents],

    % The scanner needs access to call all type functions. For this purpose a
    % temporary type module is created which exports these types.
    ScannerModule = gen_scanner_module(Types, AST),

    % A normal module must contain the type definitions that it exports. To do
    % so we generate a map of these exports that can be included in codegen
    % when the module is created
    case error:collect([gen_exported_types(Mod, Types, ArgsEnv) || Mod <- Modules]) of
        {error, Errs}           -> {error, Errs};
        {ok, ExportedTypeList}  ->
            ExportedTypes = lists:flatten(ExportedTypeList),
            {ok, {ExportedTypes, ScannerModule, lists:flatten(TypeModules)}}
    end.


% For each module in the AST we create a type module which exports the children of the type
gen_type_modules(_, _, [], _) -> [];
gen_type_modules(Modules, Parent, Children, Types) ->
    [{Name, gen_type_module(Name, Children, Types)} || {module, _, Path, Exports} <- Modules,
                                                       Name <- [module:beam_name(Path ++ [Parent])],
                                                       maps:is_key(Parent, Exports)].

gen_type_module(ModuleName, Children, AllTypes) ->
    Types = maps:from_list([{Name, maps:get(Tag, AllTypes)} || {Name, Tag, _} <- Children]),
    DomainDef = gen_domain(maps:merge(AllTypes, Types)),
    TypeDefs = [gen_type_def(Name, Tag, Args) || {Name, Tag, Args} <- Children],
    Defs = [DomainDef | TypeDefs],
    {Exports, _} = lists:unzip(Defs),
    cerl:c_module(cerl:c_atom(ModuleName), Exports, [], Defs).

gen_exported_types({module, _, _, Exports}, Types, ArgsEnv) ->
    F = fun({Name, {type_export, Ctx, Key, _}}) ->
                Tag = symbol:tag(Key),
                case maps:is_key(Tag, Types) of
                    false   -> error:format({undefined_type_export, Tag}, {typegen, Ctx});
                    true    -> {ok, {Name, Tag}}
                end
        end,
    ExportedTypeTags = [F(Entry) || {_, {type_export, _, _, _}} = Entry <- maps:to_list(Exports)],
    case error:collect(ExportedTypeTags) of
        {error, Errs}       -> {error, Errs};
        {ok, Tagged}        ->
            ExportedTypes = maps:from_list([{Name, maps:get(Tag, Types)} || {Name, Tag} <- Tagged]),
            DomainDef = gen_domain(maps:merge(Types, ExportedTypes)),
            Exported = [gen_type_def(Name, Tag, maps:get(Tag, ArgsEnv)) || {Name, Tag} <- Tagged],
            {ok, [DomainDef | Exported]}
    end.

gen_scanner_module(Types, {ast, _, _, _, Defs}) ->
    DomainDef = gen_domain(Types),
    ModuleName = symbol:id(types),

    % Top level defs `TypeMod:T()` for `T` in `type T -> ...`
    ArgsF = fun(Args) ->
                    [{var, symbol:tag(A)} || A <- Args] end,
    TopLevelDefs = [gen_type_def(Name, Name, ArgsF(Args)) || {type_def, _, Name, Args, _} <- maps:values(Defs)],

    {Exports, _} = lists:unzip([DomainDef | TopLevelDefs]),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), Exports, [], [DomainDef | TopLevelDefs])}.

% Generate the function for `TypeMod:domain(T)`
gen_domain(Types) when map_size(Types) =:= 0 ->
    {cerl:c_fname(domain, 1),
     cerl:c_fun([cerl:c_var('_')], cerl:c_atom(none))};
gen_domain(Types) ->
    Clauses = [cerl:c_clause([cerl:c_atom(Tag)], Form) || {Tag, Form} <- maps:to_list(Types)],
    Arg = cerl:c_var('_domain_arg'),
    Body = cerl:c_case(Arg, Clauses),
    Compacted = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(compact), [Body]),
    FName = cerl:c_fname(domain, 1),
    {FName, cerl:c_fun([Arg], Compacted)}.

gen_type_def(Name, Tag, []) ->
    Body = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)]),
    FName = cerl:c_fname(Name, 0),
    {FName, cerl:c_fun([], Body)};
gen_type_def(Name, Tag, Args) ->
    ArgForms = [cerl:c_var(A) || {var, A} <- Args],
    Body = expr_gen:call_type_tag(Tag, ArgForms),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgForms, Body)}.
