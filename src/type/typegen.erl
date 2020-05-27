-module(typegen).
-import(lists, [unzip/1, unzip3/1, zip/2]).
-import(symbol, [tag/1]).
-export([gen/2]).

-include_lib("eunit/include/eunit.hrl").

gen(Module, AST) when is_list(AST) ->
    TypeDefs = maps:from_list([{Name, Args} || {type_def, _, Name, Args, _} <- AST]),
    {Mappings, _, _} = unzip3([domains([], [], TypeDef) || TypeDef <- AST]),
    Env = gen_env(Mappings),
    io:format("Type Env: ~p~n", [Env]),

    case error:flatten([compile(Tag, Env, TypeDefs) || Tag <- maps:keys(Env)]) of
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

domains([], [], {type_def, _, Name, Args, Expr}) -> 
    DefArgs = [tag(A) || A <- Args],
    {Env, _, Domain} = domains([Name], DefArgs, Expr),
    {[{Name, DefArgs, Domain} | Env], DefArgs, Domain};

domains(Path, Args, {tuple, _, [Expr]}) -> domains(Path, Args, Expr);

domains(Path, Args, {tuple, _, Expressions}) ->
    {EnvList, Vars, Domains} = unzip3([domains(Path, Args, Expr) || Expr <- Expressions]),
    Domain = domain:compact({sum, ordsets:from_list(Domains)}),
    {lists:flatten(EnvList), order(Args, Vars), Domain};

domains(Path, Args, {dict, _, Pairs}) ->
    {EnvList, Vars, Domains} = unzip3([domains(Path, Args, Expr) || {pair, _, _, Expr} <- Pairs]),
    Keys = [symbol:name(P) || P <- Pairs],
    {lists:flatten(EnvList), order(Args, Vars), {product, maps:from_list(zip(Keys, Domains))}};

domains(Path, Args, {pair, _, Key, Expr}) ->
    Tag = tag(Key),
    NewPath = [Tag | Path],
    {Env, Vars, ExprDomain} = domains(NewPath, Args, Expr),
    Domain = {tagged, Tag, ExprDomain},
    {[{Tag, order(Args, Vars), Domain} | Env], order(Args, Vars), Domain};

domains(_, _, {variable, _, _, Tag}) -> {[], [Tag], {variable, Tag}};

domains(Path, Args, {application, _, Expr, AppArgs}) ->
    {ExprEnv, ExprVars, ExprDomain} = domains(Path, Args, Expr),
    {ArgsEnvs, ArgVars, ArgDomains} = unzip3([domains(Path, Args, A) || A <- AppArgs]),
    Env = lists:flatten(ArgsEnvs) ++ ExprEnv,
    Vars = ordsets:to_list(ordsets:from_list(ExprVars ++ ArgVars)),
    Domain = {application, ExprDomain, ArgDomains},
    {Env, order(Args, Vars), Domain};

domains(Path, _, {type, _, _} = Type) -> 
    Tag = tag(Type),
    case lists:member(Tag, Path) of
        true -> {[], [], {recur, Tag}};
        false -> {[{Tag, [], {type, Tag}}], [], {type, Tag}}
    end;

domains(_, _, {key, _, Key}) -> 
    {[], [], Key}.

abstract_form(Env, TypeDefs, {f, Tag, Vars, Domain}) ->
    error:map(abstract_form(Env, TypeDefs, Domain), fun(DomainForm) ->
        ArgsForm = [cerl:c_var(V) || V <- Vars],
        cerl:c_tuple([cerl:c_atom(f), cerl:c_atom(Tag), cerl:c_fun(ArgsForm, DomainForm)]) end);

abstract_form(Env, TypeDefs, {sum, Set}) -> 
    case error:flatten([abstract_form(Env, TypeDefs, Elem) || Elem <- ordsets:to_list(Set)]) of
        {error, Err} -> {error, Err};
        {ok, ElemForms} ->
            Elements = cerl:make_list(ElemForms),
            DomainSet = cerl:c_call(cerl:c_atom(ordsets), cerl:c_atom(from_list), [Elements]),
            {ok, cerl:c_tuple([cerl:c_atom(sum), DomainSet])}
    end;

abstract_form(Env, TypeDefs, {product, Map}) ->
    case error:flatten([abstract_form(Env, TypeDefs, D) || D <- maps:values(Map)]) of
        {error, Errors} -> {error, Errors};
        {ok, Forms} ->
            Entries = [cerl:c_map_pair(cerl:c_atom(K), Form) || {Form, K} <- zip(Forms, maps:keys(Map))],
            DomainMap = cerl:c_map(Entries),
            {ok, cerl:c_tuple([cerl:c_atom(product), DomainMap])}
    end;

abstract_form(Env, TypeDefs, {tagged, Tag, Domain}) ->
    error:map(abstract_form(Env, TypeDefs, Domain), fun(DomainForm) ->
        cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), DomainForm]) end);

abstract_form(_, _, {variable, Tag}) -> 
    {ok, cerl:c_var(Tag)};

abstract_form(Env, TypeDefs, {type, Tag}) -> 
    case maps:get(Tag, Env) of
        {_, {type, Tag}} -> {ok, cerl:c_atom(Tag)};
        {_, Domain} -> {ok, abstract_form(Env, TypeDefs, Domain)}
    end;

abstract_form(_, _, {recur, Tag}) ->
    BranchFun = cerl:c_fun([], cerl:c_apply(cerl:c_atom(domain), [cerl:c_atom(Tag)])),
    cerl:c_tuple([cerl:c_atom(recur), BranchFun]);

abstract_form(Env, TypeDefs, {application, Expr, Args} = Current) ->
    CompiledArgs = error:flatten([abstract_form(Env, TypeDefs, A) || A <- Args]),
    case {Expr, CompiledArgs} of

        {_, {error, E}} -> {error, E};

        % type argument function e.g.: `type FunctorPair f t -> { a: t, b: f(t) }`
        {{variable, Tag}, {ok, ArgForms}} -> {ok, unsafe_call_form(cerl:c_var(Tag), ArgForms)};

        % type function e.g.: type IntOption -> Option(Int) 
        {{type, Tag}, {ok, ArgForms}} ->
            case {maps:get(Tag, Env), maps:is_key(Tag, TypeDefs)} of

                {_, false} -> {error, [{{nonexistent_type_def, Tag}, {typegen, Current}}]};

                {{Vars, _}, _} when length(Vars) =:= length(Args) -> 
                    io:format("Compiling application of Tag: ~p with Args: ~p~n", [Tag, ArgForms]),
                    {ok, cerl:c_apply(cerl:c_fname(Tag, length(Args)), ArgForms)};

                {{Vars, _}, _} ->                     
                    {error, [{{wrong_number_of_arguments, Tag, length(Args), length(Vars)}, {typegen, Current}}]}
            end;

        % type recursion e.g.: List a -> Nil | Cons: { head: a, tail: List(a) }
        {{recur, Tag}, {ok, ArgForms}} ->
            case {maps:get(Tag, Env), maps:is_key(Tag, TypeDefs)} of

                {_, false} -> {error, [{{nonexistent_type_def, Tag}, {typegen, Current}}]};

                {{Vars, _}, _} when length(Vars) =:= length(Args) -> 
                    BranchFun = cerl:c_fun([], unsafe_call_form(cerl:c_atom(Tag), ArgForms)),
                    {ok, cerl:c_tuple([cerl:c_atom(recur), BranchFun])};

                {{Vars, _}, _} ->                     
                    {error, [{{wrong_number_of_arguments, Tag, length(Args), length(Vars)}, {typegen, Current}}]}
            end
    end;

abstract_form(_, _, Key) -> {ok, cerl:c_atom(Key)}.

order(Args, Vars) ->
    FlatVars = lists:flatten(Vars),
    [A || A <- Args, lists:member(A, FlatVars)].

group_by(F, L) -> dict:to_list(lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ])).

unsafe_call_form(NameForm, ArgForms) ->
    cerl:c_apply(
      cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(element), 
                  [cerl:c_int(3), cerl:c_apply(cerl:c_fname(domain, 1), [NameForm])]),
      ArgForms).

-ifdef(TEST).

run(Code, RunAsserts) ->
    {ok, _, {TypeAST, _}} = kind:get_AST(Code),
    io:format("TypeAST: ~p~n", [TypeAST]),
    case typer:load("test", TypeAST) of
        {error, Errs} -> RunAsserts({error, Errs});
        {ok, TypeMod} ->
            io:format("TypeMod: ~p~n", [TypeMod]),
            RunAsserts(TypeMod),
            true = code:soft_purge(TypeMod),
            true = code:delete(TypeMod)
    end.

sum_type_boolean_test() ->
    Code = "type Boolean -> True | False",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])},
                         Actual = Mod:domain('Boolean'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'Boolean'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

product_type_test() ->
    Code = "type P -> {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {product, #{a => 'P/A', b => 'P/B'}},
                         Actual = Mod:domain('P'),
                         ?assertEqual(none, domain:diff(Expected, Actual)),
                         Actual2 = Mod:'P'(),
                         ?assertEqual(none, domain:diff(Expected, Actual2))
                 end,
    run(Code, RunAsserts).

tagged_type_test() ->
    Code = "type P -> K: {a: A, b: B}",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'P/K', {product, #{a => 'P/A', b => 'P/B'}}},
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
                         {f, 'Id', DomainFun} = Mod:domain('Id'),
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

tagged_product_subset_test() ->
    Code = "type Time -> {hour: (Hour: Int), minute: (Minute: Int), second: (Second: Int)}",
    RunAsserts = fun(Mod) ->
                         Actual = Mod:domain('Time/Minute'),
                         Expected = {tagged, 'Time/Minute', 'Time/Int'},
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

sum_var_test() ->
    Code = "type Boolean -> True | False\n"
           "type Option a -> a | None",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['Boolean/True',
                                                          'Boolean/False',
                                                          'Option/None'])},
                         Actual = Mod:'Option'({sum, ordsets:from_list(['Boolean/True', 'Boolean/False'])}),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

buried_var_test() ->
    Code = "type Buried a -> Surface | Bottom: { var: a }\n"
           "type Hidden -> Treasure",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Buried/Bottom', 
                                     {product, #{var => 'Hidden/Treasure'}}},
                         {f, 'Buried/Bottom', DomainFun} = Mod:domain('Buried/Bottom'),
                         Actual = DomainFun('Hidden/Treasure'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

var_order_test() ->
    Code = "type Order a b c -> T: (C: c | B: b | A: a)\n"
           "type Args -> A | B | C",
    RunAsserts = fun(Mod) ->
                         Expected = {tagged, 'Order/T', 
                                     {sum, ordsets:from_list([
                                                           {tagged, 'Order/C', 'Args/C'},
                                                           {tagged, 'Order/A', 'Args/A'},
                                                           {tagged, 'Order/B', 'Args/B'}])}},
                         {f, 'Order/T', DomainFun} = Mod:domain('Order/T'),
                         Actual = DomainFun('Args/A', 'Args/B', 'Args/C'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

application_top_level_f_test() ->
    Code = "type Option a -> a | None\n"
           "type BlahOption -> Option(blah)",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['BlahOption/blah', 'Option/None'])},
                         Actual = Mod:domain('BlahOption'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

application_wrong_number_of_args_test() ->
    Code = "type Option a -> a | None\n"
           "type BlahOption -> Option(blip, blup)",
    RunAsserts = fun({error, [{{ActualType, ActualTag, ActualGivenNumber, ActualExpectedNumber}, _}]}) ->
                         ExpectedType = wrong_number_of_arguments,
                         ExpectedTag = 'Option',
                         ExpectedGivenNumber = 2,
                         ExpectedExpectedNumber = 1,
                         ?assertEqual(ExpectedType, ActualType),
                         ?assertEqual(ExpectedTag, ActualTag),
                         ?assertEqual(ExpectedGivenNumber, ActualGivenNumber),
                         ?assertEqual(ExpectedExpectedNumber, ActualExpectedNumber)
                 end,
    run(Code, RunAsserts).

application_inner_level_f_test() ->
    Code = "type Option a -> P: {a: a} | None | O: P(a)\n",
    RunAsserts = fun({error, [{{ActualType, ActualTag}, _}]}) ->
                         ExpectedType = nonexistent_type_def,
                         ExpectedTag = 'Option/P',
                         ?assertEqual(ExpectedType, ActualType),
                         ?assertEqual(ExpectedTag, ActualTag)
                 end,
    run(Code, RunAsserts).

application_first_order_type_test() ->
    Code = "type Args -> Arg1 | Arg2\n"
           "type Option a -> None | a\n"
           "type AnyOption f a -> f(a)",
    RunAsserts = fun(Mod) ->
                         Expected = {sum, ordsets:from_list(['Option/None', 'Args/Arg1'])},
                         {f, 'AnyOption', DomainFun} = Mod:domain('AnyOption'),
                         Actual = DomainFun('Option', 'Args/Arg1'),
                         ?assertEqual(none, domain:diff(Expected, Actual))
                 end,
    run(Code, RunAsserts).

recursion_top_level_f_test() ->
    Code = "type List a -> Nil | Cons: {head: a, tail: List(a)}",
    RunAsserts = fun(Mod) ->
                         {f, 'List', DomainFun} = Mod:domain('List'),
                         Actual = DomainFun('List/Nil'),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, Actual),
                         {_, [_, {_, _, {recur, RecurFun}}]} = Actual,
                         ?assertMatch({product, _}, RecurFun()),
                         {product, ProductMap} = RecurFun(),
                         ?assertEqual('List/Nil', maps:get(head, ProductMap)),
                         ?assertMatch({sum, ['List/Nil', {tagged, 'List/Cons', {recur, _}}]}, maps:get(tail, ProductMap))
                 end,
    run(Code, RunAsserts).


-endif.
