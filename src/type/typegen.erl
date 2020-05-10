-module(typegen).
-import(lists, [unzip/1, unzip3/1, zip/2]).
-export([gen/2]).

-include_lib("eunit/include/eunit.hrl").

gen(Module, AST) when is_list(AST) ->
    {Mappings, _, Domains} = unzip3([domains([], TypeDef) || TypeDef <- AST]),
    Env = gen_env(Mappings),
    io:format("Type Env: ~p~n", [Env]),

    % All types defined within the module
    AllTypes = [compile(Tag, Env) || Tag <- maps:keys(Env)], 
    DomainDef = gen_domain_def(AllTypes),

    % Top level types (e.g. `T` in `type T -> ...`)
    TopLevelTypes = [{Name, Args, Domain} || {{type_def, _, Name, Args, _}, Domain} <- zip(AST, Domains)],
    TypeDefs = [gen_type_def(Name, Args, abstract_form(Env, Domain)) || {Name, Args, Domain} <- TopLevelTypes],

    {Exports, _} = unzip([DomainDef | TypeDefs]),
    {ok, cerl:c_module(cerl:c_atom(Module), Exports, [], [DomainDef | TypeDefs])}.

gen_type_def(Name, Args, Body) -> 
    ArgsForm = [cerl:c_var(gen_tag(A)) || A <- Args],
    Compacted = cerl:c_call(cerl:c_atom(domain), cerl:c_atom(compact), [Body]),
    FName = cerl:c_fname(Name, length(Args)),
    {FName, cerl:c_fun(ArgsForm, Compacted)}.

gen_domain_def([]) -> {cerl:c_fname(domain, 1), cerl:c_fun([cerl:c_var('_')],cerl:c_atom(none))};
gen_domain_def(DomainList) ->
    Domains = sets:to_list(sets:from_list(DomainList)),
    Clauses = [cerl:c_clause([cerl:c_atom(Tag)], Form) || {Tag, Form} <- Domains],
    Arg = cerl:c_var(type),
    Body = cerl:c_case(Arg, Clauses),
    Name = cerl:c_fname(domain, 1),
    {Name, cerl:c_fun([Arg], Body)}.

gen_env(Envs) when is_list(Envs) ->
    Key = fun({Tag, _, _}) -> Tag end,
    Grouped = group_by(Key, lists:flatten(Envs)),
    Deduped = lists:map(fun F({Tag, [{_, Vars, Domain}]}) -> {Tag, Vars, Domain};
                            F({Tag, [{_, _, {type, _}} | Rest]}) -> F({Tag, Rest});
                            F({Tag, [{_, Vars, Domain} | _]}) -> {Tag, Vars, Domain} 
                            end, Grouped),
    maps:from_list([{Tag, {Vars, Domain}} || {Tag, Vars, Domain} <- Deduped]).

domains([], {type_def, _, Name, Args, Expr}) -> 
    Vars = [gen_tag(A) || A <- Args],
    {Env, _, Domain} = domains(Vars, Expr),
    {[{Name, Vars, Domain} | Env], Vars, Domain};

domains(Args, {tuple, _, [Expr]}) -> domains(Args, Expr);

domains(Args, {tuple, _, Expressions}) ->
    {EnvList, Vars, Domains} = unzip3([domains(Args, Expr) || Expr <- Expressions]),
    Domain = domain:compact({sum, sets:from_list(Domains)}),
    {lists:flatten(EnvList), order(Args, Vars), Domain};

domains(Args, {dict, _, Pairs}) ->
    {EnvList, Vars, Domains} = unzip3([domains(Args, Expr) || {pair, _, _, Expr} <- Pairs]),
    Keys = [Key || {pair, _, Key, _} <- Pairs],
    io:format("Args: ~p, Vars: ~p~n", [Args, Vars]),
    {lists:flatten(EnvList), order(Args, Vars), {product, maps:from_list(zip(Keys, Domains))}};

domains(Args, {pair, _, Key, Expr}) ->
    {Env, Vars, ExprDomain} = domains(Args, Expr),
    Tag = gen_tag(Key),
    Domain = {tagged, Tag, ExprDomain},
    {[{Tag, order(Args, Vars), Domain} | Env], order(Args, Vars), Domain};

domains(_, {variable, _, _, Tag}) -> {[], [Tag], {variable, Tag}};

domains(_, {type, _, _} = Type) -> 
    Tag = gen_tag(Type),
    Domain = {type, Tag},
    {[{Tag, [], Domain}], [], Domain}.

compile(Tag, Env) -> 
    {Vars, Domain} = maps:get(Tag, Env),
    case Vars of
        [] -> {Tag, abstract_form(Env, Domain)};
        _ -> {Tag, abstract_form(Env, {f, Vars, Domain})}
    end.

abstract_form(Env, {f, Vars, Domain}) ->
    DomainForm = abstract_form(Env, Domain),
    ArgsForm = [cerl:c_var(V) || V <- Vars],
    cerl:c_tuple([cerl:c_atom(f), cerl:c_fun(ArgsForm, DomainForm)]);

abstract_form(Env, {sum, Set}) -> 
    Elements = cerl:make_list([abstract_form(Env, Elem) || Elem <- sets:to_list(Set)]),
    DomainSet = cerl:c_call(cerl:c_atom(sets), cerl:c_atom(from_list), [Elements]),
    cerl:c_tuple([cerl:c_atom(sum), DomainSet]);

abstract_form(Env, {product, Map}) ->
    Entries = [cerl:c_map_pair(cerl:c_atom(gen_tag(K)), abstract_form(Env, D)) || 
               {K, D} <- maps:to_list(Map)],
    DomainMap = cerl:c_map(Entries),
    cerl:c_tuple([cerl:c_atom(product), DomainMap]);

abstract_form(Env, {tagged, Tag, Domain}) ->
    cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), abstract_form(Env, Domain)]);

abstract_form(_, {variable, Tag}) -> 
    cerl:c_apply(cerl:c_fname(domain, 1), [cerl:c_var(Tag)]);

abstract_form(Env, {type, Tag}) -> 
    case maps:get(Tag, Env) of
        {_, {type, Tag}} -> cerl:c_atom(Tag);
        {_, Domain} -> abstract_form(Env, Domain)
    end.

order(Args, Vars) ->
    FlatVars = lists:flatten(Vars),
    [A || A <- Args, lists:member(A, FlatVars)].

gen_tag({type, _, Symbols}) -> 
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));

gen_tag({symbol, _, S}) -> S;

gen_tag({variable, _, _, Tag}) -> Tag.

group_by(F, L) -> dict:to_list(lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ])).

-ifdef(TEST).

run(Code, RunAsserts) ->
    {ok, _, {TypeAST, _}} = kind:get_AST(Code),
    io:format("TypeAST: ~p~n", [TypeAST]),
    {ok, TypeMod} = typer:load("test", TypeAST),
    io:format("TypeMod: ~p~n", [TypeMod]),
    RunAsserts(TypeMod),
    true = code:soft_purge(TypeMod),
    true = code:delete(TypeMod).

sum_type_boolean_test() ->
    Code = "type Boolean -> True | False",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, sets:from_list(['Boolean/True', 'Boolean/False'])},
                         Actual = Mod:domain('Boolean'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'Boolean'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

product_type_test() ->
    Code = "type P -> {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {product, #{'P/a' => 'P/A', 'P/b' => 'P/B'}},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

tagged_type_test() ->
    Code = "type P -> K: {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'P/K', {product, #{'P/a' => 'P/A', 'P/b' => 'P/B'}}},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

type_parameter_test() ->
    Code = "type Id a -> a\n"
           "type T -> A | B",
    RunAsserts = fun(Mod) -> 
                         ?assertEqual('T/A', Mod:'Id'('T/A')),
                         {f, DomainFun} = Mod:domain('Id'),
                         ?assertEqual('T/B', DomainFun('T/B'))
                 end,
    run(Code, RunAsserts).

tagged_type_reuse_name_test() ->
    Code = "type P -> P: Int",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'P', 'P/Int'},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

tagged_subtype_test() ->
    Code = "type TimeUnit -> Hour: Int | Minute: Int | Second: Int",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'TimeUnit/Minute', 'TimeUnit/Int'},
                         Actual = Mod:domain('TimeUnit/Minute'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

tagged_product_subttype_test() ->
    Code = "type Time -> {hour: (Hour: Int), minute: (Minute: Int), second: (Second: Int)}",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Time/Minute', 'Time/Int'},
                         Actual = Mod:domain('Time/Minute'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

sum_var_test() ->
    Code = "type Boolean -> True | False\n"
           "type Option a -> a | None",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, sets:from_list(['Boolean/True',
                                                          'Boolean/False',
                                                          'Option/None'])},
                         Actual = Mod:'Option'('Boolean'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

buried_var_test() ->
    Code = "type Buried a -> Surface | Bottom: { var: a }\n"
           "type Hidden -> Treasure",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Buried/Bottom', 
                                     {product, #{'Buried/var' => 'Hidden/Treasure'}}},
                         {f, DomainFun} = Mod:domain('Buried/Bottom'),
                         Actual = DomainFun('Hidden/Treasure'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

var_order_test() ->
    Code = "type Order a b c -> T: (C: c | B: b | A: a)\n"
           "type Args -> A | B | C",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Order/T', 
                                     {sum, sets:from_list([
                                                           {tagged, 'Order/C', 'Args/C'},
                                                           {tagged, 'Order/B', 'Args/B'},
                                                           {tagged, 'Order/A', 'Args/A'}])}},
                         {f, DomainFun} = Mod:domain('Order/T'),
                         Actual = DomainFun('Args/A', 'Args/B', 'Args/C'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).



-endif.
