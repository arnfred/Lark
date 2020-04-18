-module(tagger).
-export([tag/1]).

-include_lib("eunit/include/eunit.hrl").

tag(AST) -> 
    {EnvList, Definitions} = lists:unzip([tag(#{}, Definition) || Definition <- AST]),
    {ok, slurp(EnvList), Definitions}.

tag(Env, {def, Line, Args, Body}) ->
    {ArgsEnv, TaggedArgs} = tag(Env, Args),
    {BodyEnv, TaggedBody} = tag(ArgsEnv, Body),
    {BodyEnv, {def, Line, TaggedArgs, TaggedBody}};

tag(Env, {type, Line, Args, Body}) ->
    {ArgsEnv, TaggedArgs} = tag(Env, Args),
    {BodyEnv, TaggedBody} = tag(ArgsEnv, Body),
    {BodyEnv, {type, Line, TaggedArgs, TaggedBody}};

tag(Env, {clauses, Line, Clauses}) ->
    {EnvList, TaggedClauses} = lists:unzip([tag(Env, Clause) || Clause <- Clauses]),
    {slurp(EnvList), {clauses, Line, TaggedClauses}};

tag(Env, {clause, Line, Patterns, Expr}) ->
    {EnvList, TaggedPatterns} = lists:unzip([tag(Env, Pattern) || Pattern <- Patterns]),
    {NewEnv, TaggedBody} = tag(slurp(EnvList), Expr),
    {NewEnv, {clause, Line, TaggedPatterns, TaggedBody}};

tag(Env, {application, Line, Name, Args}) ->
    {NameEnv, TaggedName} = tag(Env, Name),
    {NewEnv, TaggedArgs} = tag(NameEnv, Args),
    {NewEnv, {application, Line, TaggedName, TaggedArgs}};

tag(Env, {match, Line, Expr, Clauses}) ->
    {ExprEnv, TaggedExpr} = tag(Env, Expr),
    {ClausesEnv, TaggedClauses} = tag(ExprEnv, Clauses),
    {ClausesEnv, {match, Line, TaggedExpr, TaggedClauses}};

tag(Env, {tuple, Line, Expressions}) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                {NewEnv, TaggedExpr} = tag(EnvAcc, Expr),
                {maps:merge(EnvAcc, NewEnv), [TaggedExpr | ExprAcc]} 
        end,
    {EnvExpr, TaggedExpressions} = lists:foldl(F, {Env, []}, Expressions),
    {EnvExpr, {tuple, Line, lists:reverse(TaggedExpressions)}};

tag(Env, {symbol, Line, S}) -> 
    Tagged = maps:get(S, Env, symbol:id(S)),
    {maps:put(S, Tagged, Env), {symbol, Line, S, Tagged}};

tag(Env, {type_symbol, Line, S}) -> 
    Tagged = maps:get(S, Env, symbol:id(S)),
    {maps:put(S, Tagged, Env), {type_symbol, Line, S, Tagged}};

tag(Env, {qualified_symbol, Symbols}) -> {Env, {qualified_symbol, Symbols}};

tag(Env, Elements) when is_list(Elements) ->
    {EnvList, Tagged} = lists:unzip([tag(Env, E) || E <- Elements]),
    {slurp(EnvList), Tagged}.

slurp(MapList) -> maps:from_list(lists:flatten([maps:to_list(M) || M <- MapList])).

-ifdef(TEST).

tag_AST(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    tag(AST).

identity_function_test() ->
    {ok, Tags, [{def, _, [Name | Args], Body}]} = tag_AST("def id a -> a"),
    ?assertEqual(maps:is_key(id, Tags), true),
    ?assertEqual(maps:is_key(a, Tags), true),
    TaggedName = maps:get(id, Tags),
    TaggedArg = maps:get(a, Tags),
    {symbol, _, a, TaggedArg} = Body,
    [{symbol, _, a, TaggedArg}] = Args,
    {symbol, _, id, TaggedName} = Name.

pattern_match1_test() ->
    Code = 
        "def not a\n"
        " | b -> b",
    {ok, Tags, [{def, _, _, {clauses, _, [Clause]}}]} = tag_AST(Code),
    TaggedArg = maps:get(b, Tags),
    {clause, _, [{symbol, _, b, TaggedArg}], {symbol, _, b, TaggedArg}} = Clause.

pattern_match2_test() ->
    Code = 
        "def not a\n"
        " | b -> a",
    {ok, Tags, [{def, _, _, {clauses, _, [Clause]}}]} = tag_AST(Code),
    TaggedArg = maps:get(a, Tags),
    {clause, _, _, {symbol, _, a, TaggedArg}} = Clause.

tuple_test() ->
    Code = "def not a -> (a, a)",
    {ok, Tags, [{def, _, _, {tuple, _, [L1, L2]}}]} = tag_AST(Code),
    TaggedArg = maps:get(a, Tags),
    {symbol, _, a, TaggedArg} = L1,
    {symbol, _, a, TaggedArg} = L2.
    
anonymous_function_test() ->
    Code = 
        "def blap a -> a.blip(b -> b\n"
        "                     _ -> a)",
    {ok, Tags, [{def, _, DefArgs, {application, _, _, Args}}]} = tag_AST(Code),
    TaggedA = maps:get(a, Tags),
    TaggedB = maps:get(b, Tags),
    [_, {symbol, _, a, TaggedA}] = DefArgs,
    [_, {clauses, _, [Clause1, Clause2]}] = Args,
    {clause, _, [{symbol, _, b, TaggedB}], {symbol, _, b, TaggedB}} = Clause1,
    {clause, _, _, {symbol, _, a, TaggedA}} = Clause2.
    
-endif.
