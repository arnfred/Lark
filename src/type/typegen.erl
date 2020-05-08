-module(typegen).
-import(lists, [unzip/1, unzip3/1]).
-export([gen/2]).

-include_lib("eunit/include/eunit.hrl").

gen(Module, AST) when is_list(AST) ->
    {TypeDefs, Domains} = unzip([gen_type_def(Type) || Type <- AST]),
    DomainFun = gen_domain(lists:flatten(Domains)),
    {Exports, _} = unzip([DomainFun | TypeDefs]),
    {ok, cerl:c_module(cerl:c_atom(Module), Exports, [], [DomainFun | TypeDefs])}.

gen_domain([]) -> {cerl:c_fname(domain, 1), cerl:c_fun([cerl:c_var(a)],cerl:c_var(a))};
gen_domain(Domains) ->
    Clauses = [cerl:c_clause(cerl:c_atom(Tag), Form) || {Tag, Form} <- Domains],
    Arg = cerl:c_var(type),
    Body = cerl:c_case(Arg, Clauses),
    Name = cerl:c_fname(domain, 1),
    {Name, cerl:c_fun([Arg], Body)}.

gen_type_def({type_def, _, Name, Args, Expr}) ->
    {_, _, ArgsForm} = unzip3([gen_type(A) || A <- Args]),
    {ExprEnv, _, ExprForm} = gen_type(Expr),
    FName = cerl:c_fname(Name, length(Args)),
    TypeDef = {FName, cerl:c_fun(ArgsForm, ExprForm)},
    {TypeDef, ExprEnv}.

gen_type({variable, _, S, Tag}) -> {[], [S], cerl:c_var(Tag)};

gen_type({qualified_symbol, L, Symbols}) -> gen_type({type, L, Symbols});

gen_type({pair, _, Symbol, Expr}) ->
    {ExprEnv, Vars, ExprForm} = gen_type(Expr),
    Tag = gen_tag(Symbol),
    TaggedForm = domain_form({tagged, Tag, ExprForm}),
    {[{Tag, ExprForm} | ExprEnv], Vars, maybe_lambda(Vars, TaggedForm)}.

gen_tag({type, _, Symbols}) -> 
    list_to_atom(lists:flatten([atom_to_list(A) || A <- lists:join('/', Symbols)]));

gen_tag({symbol, _, S}) -> S;

gen_tag({variable, _, _, Tag}) -> Tag.

domain_form({Symbol, Tag, Domain}) -> cerl:c_tuple(cerl:c_atom(Symbol), gen_type(Tag), Domain);
domain_form({Symbol, Domain}) -> cerl:c_tuple(cerl:c_atom(Symbol), Domain).

maybe_lambda([], Form) -> Form;
maybe_lambda(Vars, Form) -> cerl:c_fun([cerl:c_var(V) || V <- Vars], Form).

-ifdef(TEST).

run(Code, RunAsserts) ->
    {ok, TypeAST, _} = kind:get_AST(Code),
    io:format("TypeAST: ~p~n", [TypeAST]),
    {ok, TypeMod} = typer:load("test", TypeAST),
    io:format("TypeMod: ~p~n", [TypeMod]),
    RunAsserts(TypeMod),
    true = code:soft_purge(TypeMod),
    true = code:delete(TypeMod).

type_identity_test() ->
    Code = "type Id a -> a",
    RunAsserts = fun(Mod) -> ?assertEqual(Mod:'Id'('Int'), 'Int') end,
    run(Code, RunAsserts).

-endif.
