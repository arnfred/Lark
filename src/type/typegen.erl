-module(typegen).
-import(lists, [unzip/1, unzip3/1, zip/2]).
-import(symbol, [tag/1]).
-export([gen/2]).


gen(Module, AST) when is_list(AST) ->
    TypeDefs = maps:from_list([{Name, Args} || {type_def, _, Name, Args, _} <- AST]),
    {Mappings, _, _} = unzip3([domain([], [], TypeDef) || TypeDef <- AST]),
    Env = gen_env(Mappings),
    io:format("Type Env: ~p~n", [Env]),

    case error:collect([compile(Tag, Env, TypeDefs) || Tag <- maps:keys(Env)]) of
        {error, Errs} -> {error, Errs};
        {ok, AllTypes} ->
            % Generate the function for `TypeMod:domain(T)`
            DomainDef = gen_domain_def(AllTypes),

            % Top level defs `TypeMod:T()` for `T` in `type T -> ...`
            TopLevelDefs = [gen_top_level_def(Name, Args) || {type_def, _, Name, Args, _} <- AST],

            {Exports, _} = unzip([DomainDef | TopLevelDefs]),
            {ok, cerl:c_module(cerl:c_atom(Module), Exports, [], [DomainDef | TopLevelDefs])}
    end.

compile(Tag, Env, TypeDefs) -> 
    {Vars, Domain} = maps:get(Tag, Env),
    case Vars of
        [] -> error:map(abstract_form(Env, TypeDefs, Domain), fun(Form) -> {Tag, Form} end);
        _N -> error:map(abstract_form(Env, TypeDefs, {f, Tag, Vars, Domain}), fun(Form) -> {Tag, Form} end)
    end.

gen_top_level_def(Name, []) ->
    Body = cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Name)]),
    FName = cerl:c_fname(Name, 0),
    {FName, cerl:c_fun([], Body)};
gen_top_level_def(Name, Args) -> 
    ArgForms = [cerl:c_var(tag(A)) || A <- Args],
    Body = unsafe_call_form(cerl:c_atom(Name), ArgForms),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgForms, Body)}.

gen_domain_def([]) -> {cerl:c_fname(domain, 1),
                       cerl:c_fun([cerl:c_var('_')], cerl:c_atom(none))};
gen_domain_def(DomainList) ->
    Domains = ordsets:to_list(ordsets:from_list(DomainList)),
    Clauses = [cerl:c_clause([cerl:c_atom(Tag)], Form) || {Tag, Form} <- Domains],
    Arg = cerl:c_var(type),
    Body = cerl:c_case(Arg, Clauses),
    Compacted = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(compact), [Body]),
    Name = cerl:c_fname(domain, 1),
    {Name, cerl:c_fun([Arg], Compacted)}.

gen_env(Envs) when is_list(Envs) ->
    Key = fun({Tag, _, _}) -> Tag end,
    Grouped = group_by(Key, lists:flatten(Envs)),
    Deduped = lists:map(fun F({Tag, [{_, Vars, Domain}]}) -> {Tag, Vars, Domain};
                            F({Tag, [{_, _, {type, _}} | Rest]}) -> F({Tag, Rest});
                            F({Tag, [{_, Vars, Domain} | _]}) -> {Tag, Vars, Domain} 
                            end, Grouped),
    maps:from_list([{Tag, {Vars, Domain}} || {Tag, Vars, Domain} <- Deduped]).

domain([], [], {type_def, _, Name, Args, Clauses}) when is_list(Clauses) -> 
    DefArgs = [tag(A) || A <- Args],
    {Env, _, Domain} = domain([Name], DefArgs, {clauses, Clauses}),
    {[{Name, DefArgs, Domain} | Env], DefArgs, Domain};

domain([], [], {type_def, _, Name, Args, Expr}) -> 
    DefArgs = [tag(A) || A <- Args],
    {Env, _, Domain} = domain([Name], DefArgs, Expr),
    {[{Name, DefArgs, Domain} | Env], DefArgs, Domain};

domain(Path, Args, {clauses, Clauses}) ->
    {Envs, _, ClauseDomains} = unzip3([domain(Path, Args, Clause) || Clause <- Clauses]),
    Domain = {clauses, Args, ClauseDomains},
    Env = lists:flatten(Envs),
    {Env, Args, Domain};

domain(Path, Args, {clause, _, Patterns, Expr}) ->
    {_, PatternDomains} = unzip([pattern_domain(Pattern) || Pattern <- Patterns]),
    {Env, _, ExprDomain} = domain(Path, Args, Expr),
    {Env, [], {clause, PatternDomains, ExprDomain}};

domain(Path, Args, {tuple, _, [Expr]}) -> domain(Path, Args, Expr);

domain(Path, Args, {tuple, _, Expressions}) ->
    {EnvList, Vars, Domains} = unzip3([domain(Path, Args, Expr) || Expr <- Expressions]),
    Domain = {sum, ordsets:from_list(Domains)},
    {lists:flatten(EnvList), order(Args, Vars), Domain};


domain(Path, Args, {dict, _, Elems}) ->
    {EnvList, Vars, Domains} = unzip3([domain(Path, Args, Expr) || {pair, _, _, Expr} <- Elems]),
    Keys = [symbol:name(P) || P <- Elems],
    {lists:flatten(EnvList), order(Args, Vars), {product, maps:from_list(zip(Keys, Domains))}};

domain(Path, Args, {pair, _, Key, Expr}) ->
    Tag = tag(Key),
    NewPath = [Tag | Path],
    {Env, Vars, ExprDomain} = domain(NewPath, Args, Expr),
    Domain = {tagged, Tag, ExprDomain},
    {[{Tag, order(Args, Vars), Domain} | Env], order(Args, Vars), Domain};

domain(_, _, {variable, _, _, Tag}) -> {[], [Tag], {variable, Tag}};

domain(Path, Args, {application, _, Expr, AppArgs}) ->
    {ExprEnv, ExprVars, ExprDomain} = domain(Path, Args, Expr),
    {ArgsEnvs, ArgVars, ArgDomains} = unzip3([domain(Path, Args, A) || A <- AppArgs]),
    Env = lists:flatten(ArgsEnvs) ++ ExprEnv,
    Vars = ordsets:to_list(ordsets:from_list(ExprVars ++ ArgVars)),
    Domain = {application, ExprDomain, ArgDomains},
    {Env, order(Args, Vars), Domain};

domain(Path, _, {type, _, _} = Type) -> 
    Tag = tag(Type),
    case lists:member(Tag, Path) of
        true -> {[], [], {recur, Tag}};
        false -> {[{Tag, [], {type, Tag}}], [], {type, Tag}}
    end.

%pattern_domain([], Args, {dict, _, Elems}) ->
%    io:format("Elems: ~p~n", [Elems]),
%    Tag = fun({pair, _, Key, Val}) ->
%                  {KeyEnv, KeyVars, KeyDomain} = domain([], Args, Key),
%                  {ValEnv, ValVars, ValDomain} = domain([], Args, Val),
%                  {
%    {EnvList, Vars, Domains} = unzip3([domain([], Args, Expr) || {pair, _, _, Expr} <- Elems]),
%    Keys = [symbol:name(P) || P <- Elems],
%    {lists:flatten(EnvList), order(Args, Vars), {product, maps:from_list(zip(Keys, Domains))}};

pattern_domain({variable, _, _, Tag}) -> {[Tag], {variable, Tag}};

pattern_domain({type, _, _} = Type) -> {[], {type, tag(Type)}}.

abstract_form(Env, TypeDefs, {f, Tag, Vars, Domain}) ->
    FDomain = abstract_form(Env, TypeDefs, Domain),
    ArgsForm = [cerl:c_var(V) || V <- Vars],
    error:map(
      FDomain,
      fun(FD) -> cerl:c_tuple([cerl:c_atom(f), cerl:c_atom(Tag), cerl:c_fun(ArgsForm, FD)]) end);

abstract_form(Env, TypeDefs, {sum, Set}) -> 
    case error:collect([abstract_form(Env, TypeDefs, Elem) || Elem <- ordsets:to_list(Set)]) of
        {error, Errs} -> {error, Errs};
        {ok, ElemForms} ->
            Elements = cerl:make_list(ElemForms),
            DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [Elements]),
            {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])}
    end;

abstract_form(Env, TypeDefs, {product, Map}) ->
    io:format("Map: ~p~n", [Map]),
    RawForms = [abstract_form(Env, TypeDefs, D) || D <- maps:values(Map)],
    io:format("Raw Forms: ~p~n", [RawForms]),
    case error:collect(RawForms) of
        {error, Errs} -> {error, Errs};
        {ok, Forms} ->
            io:format("Abstract Form Sum Forms: ~p~n", [Forms]),
            Entries = [cerl:c_map_pair(cerl:c_atom(K), Form) || {Form, K} <- zip(Forms, maps:keys(Map))],
            DomainMap = cerl:c_map(Entries),
            {ok, cerl:c_tuple([cerl:c_atom(product), DomainMap])}
    end;

abstract_form(Env, TypeDefs, {tagged, Tag, Domain}) ->
    error:map(abstract_form(Env, TypeDefs, Domain), fun(DomainForm) ->
        cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), DomainForm]) end);

abstract_form(_, _, {variable, Tag}) -> {ok, cerl:c_var(Tag)};

abstract_form(Env, TypeDefs, {type, Tag}) -> 
    case maps:get(Tag, Env) of
        {_, {type, Tag}} -> {ok, cerl:c_atom(Tag)};
        {_, Domain} -> error:map(Domain, fun(D) -> abstract_form(Env, TypeDefs, D) end)
    end;

abstract_form(_, _, {recur, Tag}) ->
    BranchFun = cerl:c_fun([], cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_atom(Tag)])),
    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

abstract_form(Env, TypeDefs, {application, Expr, Args} = Current) ->
    CompiledArgs = error:collect([abstract_form(Env, TypeDefs, A) || A <- Args]),
    case {Expr, CompiledArgs} of

        {_, {error, E}} -> {error, E};

        % type argument function e.g.: `type FunctorPair f t -> { a: t, b: f(t) }`
        {{variable, Tag}, {ok, ArgForms}} -> {ok, unsafe_call_form(cerl:c_var(Tag), ArgForms)};

        % type function e.g.: type IntOption -> Option(Int) 
        {{type, Tag}, {ok, ArgForms}} ->
            case {maps:get(Tag, Env), maps:is_key(Tag, TypeDefs)} of

                {_, false} -> error:format({nonexistent_type_def, Tag}, {typegen, Current});

                {{Vars, _}, _} when length(Vars) =:= length(Args) -> 
                    {ok, cerl:c_apply(cerl:c_fname(Tag, length(Args)), ArgForms)};

                {{Vars, _}, _} ->                     
                    error:format({wrong_number_of_arguments, Tag, Args, Vars}, {typegen, Current})
            end;

        % type recursion e.g.: List a -> Nil | Cons: { head: a, tail: List(a) }
        {{recur, Tag}, {ok, ArgForms}} ->
            case {maps:get(Tag, Env), maps:is_key(Tag, TypeDefs)} of

                {_, false} -> error:format({nonexistent_type_def, Tag}, {typegen, Current});

                {{Vars, _}, _} when length(Vars) =:= length(Args) -> 
                    BranchFun = cerl:c_fun([], unsafe_call_form(cerl:c_atom(Tag), ArgForms)),
                    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

                {{Vars, _}, _} ->                     
                    error:format({wrong_number_of_arguments, Tag, Args, Vars}, {typegen, Current})
            end
    end;

abstract_form(Env, TypeDefs, {clauses, Args, ClauseDomains}) ->
    CompiledClauses = error:collect([abstract_form(Env, TypeDefs, Clause) || Clause <- ClauseDomains]),
    CompiledArgs = error:collect([cerl:c_var(A) || A <- Args]),
    error:map2(CompiledArgs,
               CompiledClauses,
               fun(CArgs, CClauses) -> cerl:c_case(cerl:c_values(CArgs), CClauses) end);

abstract_form(Env, TypeDefs, {clause, PatternDomains, ExprDomain}) ->
    CompiledPatterns = error:collect([abstract_pattern(Env, TypeDefs, P) || P <- PatternDomains]),
    CompiledExpr = abstract_form(Env, TypeDefs, ExprDomain),
    error:map2(CompiledPatterns,
               CompiledExpr,
               fun(CPs, CExpr) -> cerl:c_clause(CPs, CExpr) end);

abstract_form(_, _, Key) -> {ok, cerl:c_atom(Key)}.

abstract_pattern(_, _, {variable, Tag}) -> {ok, cerl:c_var(Tag)};

abstract_pattern(Env, TypeDefs, {type, Tag}) ->
    case maps:get(Tag, Env) of
        {_, {type, Tag}} -> {ok, cerl:c_atom(Tag)};
        {_, Domain} -> error:map(Domain, fun(D) -> abstract_pattern(Env, TypeDefs, D) end)
    end.


order(Args, Vars) ->
    FlatVars = lists:flatten(Vars),
    [A || A <- Args, lists:member(A, FlatVars)].

group_by(F, L) -> dict:to_list(lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ])).

unsafe_call_form(NameForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), cerl:c_apply(cerl:c_fname(domain, 1), [NameForm])]),
      ArgForms).
