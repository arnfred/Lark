-module(type_gen).
-export([gen/3]).

-include_lib("eunit/include/eunit.hrl").

gen(TypesEnv, ArgsEnv, AST) ->
    case gen_types(TypesEnv, AST) of
        {error, Errs}    -> {error, Errs};
        {ok, {Types, _}} -> gen_modules(Types, TypesEnv, ArgsEnv, AST)
    end.


gen_types(TypesEnv, AST) ->
    ast:traverse(fun pre_gen_term/3, fun code_gen:gen/3, TypesEnv, AST).

pre_gen_term(top_level, _, {def, _, _, _})   -> skip;
pre_gen_term(Type, Scope, Term)              -> code_gen:pre_gen(Type, Scope, Term).

gen_modules(Types, TypesEnv, ArgsEnv, {ast, _, Modules, _, _}) ->
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

    % For each type we create a type constructor
    case error:collect([gen_def(Tag, maps:get(Tag, ArgsEnv), TypesEnv) || Tag <- maps:keys(Types)]) of
        {error, Errs}       -> {error, Errs};
        {ok, TypeDefList}   ->
            TypeDefs = maps:from_list(utils:group_by(fun({FName, _}) -> cerl:fname_id(FName) end, lists:flatten(TypeDefList))),

            % For each top level type (e.g. `Boolean`) we create a module which exports
            % its type members (e.g. `True` and `False`). These type modules serve to
            % simplify the import logic by making it trivial to import type members
            % from compiled beam files. For a bit more context see commit
            % 34ea58050b0200236324e963cd14ceb58ac1a32d
            TypeModules = [Module || {Parent, Children} <- TypeParents,
                                     Module <- gen_type_modules(Modules, Parent, Children, Types, TypeDefs)],

            % The scanner needs access to call all type functions. For this purpose a
            % temporary type module is created which exports these types.
            ScannerModule = gen_scanner_module(Types, TypeDefs),

            % A normal module must contain the type definitions that it exports. To do
            % so we generate a map of these exports that can be included in codegen
            % when the module is created
            Exported = lists:flatten([exported_types(Mod, Types) || Mod <- Modules]),
            Defs = gen_module_defs(Exported, Types, TypeDefs),
            {ok, {Defs, ScannerModule, TypeModules}}
    end.


% For each module in the AST we create a type module which exports the children of the type
gen_type_modules(_, _, [], _, _) -> [];
gen_type_modules(Modules, Parent, Children, Types, TypeDefs) ->
    [{Name, gen_type_module(Name, Children, Types, TypeDefs)} || {module, _, Path, Exports} <- Modules,
                                                                 Name <- [module:beam_name(Path ++ [Parent])],
                                                                 maps:is_key(Parent, Exports)].

gen_type_module(ModuleName, Children, Types, TypeDefs) ->
    ChildTypes = maps:from_list([{Name, maps:get(Tag, Types)} || {Name, Tag, _} <- Children]),
    DomainDef = gen_domain(maps:merge(Types, ChildTypes)),
    ChildDefs = lists:flatten([name_defs(Name, Tag, TypeDefs) || {Name, Tag, _Args} <- Children]),
    Defs = [DomainDef | ChildDefs],
    {Exports, _} = lists:unzip(Defs),
    cerl:c_module(cerl:c_atom(ModuleName), Exports, [], Defs).

% TODO: Rewrite because each key in `TypeDefs` contains a list of defs
gen_module_defs(Exported, Types, TypeDefs) ->
    ExportedTypeDefs = maps:from_list([{Name, name_defs(Name, Tag, TypeDefs)} || {Name, Tag} <- Exported]),
    Defs = lists:flatten(maps:values(maps:merge(ExportedTypeDefs, TypeDefs))),
    ExportedTypeDomains = maps:from_list([{Name, maps:get(Tag, Types)} || {Name, Tag} <- Exported]),
    DomainDef = gen_domain(maps:merge(Types, ExportedTypeDomains)),
    [DomainDef | Defs].

gen_scanner_module(Types, TypeDefs) ->
    DomainDef = gen_domain(Types),
    ModuleName = symbol:id(types),

    Defs = [DomainDef | lists:flatten(maps:values(TypeDefs))],

    {Exports, _} = lists:unzip(Defs),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), Exports, [], Defs)}.

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

gen_def(Tag, Args, TypesEnv) ->
    TagEnv = maps:from_list([{symbol:tag(Path), Term} || {Path, Term} <- maps:to_list(TypesEnv)]),
    case maps:get(Tag, TagEnv) of
        {tagged, _, _, _} = T   -> tagged_gen:gen(TypesEnv, T);
        _                       -> {ok, [gen_domain_call(Tag, Args)]}
    end.

name_defs(Name, Tag, TypeDefs) ->
    [{cerl:c_fname(Name, cerl:fname_arity(FName)), Form} || {FName, Form} <- maps:get(Tag, TypeDefs)].

gen_domain_call(Name, []) ->
    Body = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Name)]),
    FName = cerl:c_fname(Name, 0),
    {FName, cerl:c_fun([], Body)};
gen_domain_call(Name, Args) ->
    ArgForms = [cerl:c_var(A) || {var, A} <- Args],
    Body = expr_gen:call_type_tag(Name, ArgForms),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgForms, Body)}.

exported_types({module, _, _, Exports}, Types) ->
    [{Name, symbol:tag(Key)} || {Name, {export, _, Key, _}} <- maps:to_list(Exports),
                    maps:is_key(symbol:tag(Key), Types)].
