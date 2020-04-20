-module(tagger).
-export([tag/1]).

-include_lib("eunit/include/eunit.hrl").

tag(AST) -> 
    {TypeEnv, TaggedTypes} = tag_types(AST),
    {DefEnv, TaggedDefs} = tag_defs(TypeEnv, AST),
    {ok, {TypeEnv, DefEnv}, {TaggedTypes, TaggedDefs}}.

tag_types(AST) ->
    Types = lists:filter(fun(A) -> element(1,A) =:= type end, AST),
    TagType = fun(Path, {symbol, Line, T}) -> {qualified_symbol, Line, lists:reverse([T | Path])} end,
    InitEnv = maps:from_list([{Name, TagType([], {symbol, Line, Name})} || {type, Line, Name, _, _} <- Types]),
    tag(InitEnv, Types, [], TagType).

tag_defs(TypeEnv, AST) ->
    Defs = lists:filter(fun(A) -> element(1,A) =:= def end, AST),
    TagDef = fun({def, Line, Name, Args, _}) -> {symbol, Line, Name, {Name, length(Args)}} end,
    DefEnv = maps:from_list([{element(3, D), TagDef(D)} || D  <- Defs]),
    TagVal = fun(_, {symbol, Line, S}) -> case maps:get(S, TypeEnv, undefined) of
                                               undefined -> {symbol, Line, S, symbol:id(S)};
                                               Type -> Type
                                          end end,
    tag(DefEnv, Defs, [], TagVal).

tag(Env, {def, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    {ArgsEnv, TaggedArgs} = tag(#{}, Args, NewPath, TagFun),
    {BodyEnv, TaggedBody} = tag(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun),
    {BodyEnv, {def, Line, Name, TaggedArgs, TaggedBody}};

tag(Env, {clauses, Line, Clauses}, Path, TagFun) ->
    {NewEnv, TaggedClauses} = tag(Env, Clauses, Path, TagFun),
    {NewEnv, {clauses, Line, TaggedClauses}};

tag(Env, {clause, Line, Patterns, Expr}, Path, TagFun) ->
    {PatternEnv, TaggedPatterns} = tag(#{}, Patterns, Path, TagFun),
    NewEnv = maps:merge(Env, PatternEnv),
    {BodyEnv, TaggedBody} = tag(NewEnv, Expr, Path, TagFun),
    {BodyEnv, {clause, Line, TaggedPatterns, TaggedBody}};

tag(Env, {application, Line, Name, Args}, Path, TagFun) ->
    NewPath = [Name | Path],
    {NameEnv, TaggedName} = tag(Env, Name, NewPath, TagFun),
    {NewEnv, TaggedArgs} = tag(NameEnv, Args, NewPath, TagFun),
    {NewEnv, {application, Line, TaggedName, TaggedArgs}};

tag(Env, {match, Line, Expr, Clauses}, Path, TagFun) ->
    {ExprEnv, TaggedExpr} = tag(Env, Expr, Path, TagFun),
    {ClausesEnv, TaggedClauses} = tag(ExprEnv, Clauses, Path, TagFun),
    {ClausesEnv, {match, Line, TaggedExpr, TaggedClauses}};

tag(Env, {tuple, Line, Expressions}, Path, TagFun) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                {NewEnv, TaggedExpr} = tag(EnvAcc, Expr, Path, TagFun),
                {maps:merge(EnvAcc, NewEnv), [TaggedExpr | ExprAcc]} 
        end,
    {EnvExpr, TaggedExpressions} = lists:foldl(F, {Env, []}, Expressions),
    {EnvExpr, {tuple, Line, lists:reverse(TaggedExpressions)}};

tag(Env, {type, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    {BodyEnv, TaggedBody} = tag(Env, Body, NewPath, TagFun),
    {BodyEnv, {type, Line, Name, Args, TaggedBody}};

tag(Env, {sum, Line, Types}, Path, TagFun) ->
    {SumEnv, TaggedTypes} = tag(Env, Types, Path, TagFun),
    {SumEnv, {sum, Line, TaggedTypes}};

tag(Env, Elements, Path, TagFun) when is_list(Elements) ->
    {EnvList, Tagged} = lists:unzip([tag(Env, E, Path, TagFun) || E <- Elements]),
    {slurp(EnvList), Tagged};

tag(Env, {qualified_symbol, _, _} = QS, _, _) -> {Env, QS};

tag(Env, {symbol, _, S} = Symbol, Path, TagFun) ->
    NewSymbol = maps:get(S, Env, TagFun(Path, Symbol)),
    NewEnv = maps:put(S, NewSymbol, Env),
    {NewEnv, NewSymbol}.

slurp(MapList) -> lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, MapList).

-ifdef(TEST).

tag_AST(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    {ok, _, {_, Tagged}} = tag(AST),
    Tagged.

identity_function_test() ->
    [{def, _, _, Args, Body}] = tag_AST("def id a -> a"),
    {symbol, _, a, TaggedArg} = Body,
    [{symbol, _, a, TaggedArg}] = Args.

pattern_match1_test() ->
    Code = 
        "def not a\n"
        " | b -> b",
    [{def, _, _, _, [Clause]}] = tag_AST(Code),
    {clause, _, [{symbol, _, b, TaggedArg}], {symbol, _, b, TaggedArg}} = Clause.

pattern_match2_test() ->
    Code = 
        "def not a\n"
        " | b -> a",
    [{def, _, _, Args, [Clause]}] = tag_AST(Code),
    [{symbol, _, a, TaggedArg}] = Args,
    {clause, _, _, {symbol, _, a, TaggedArg}} = Clause.

pattern_match3_test() ->
    Code = 
        "def not a\n"
        " | a -> a",
    Tagged = tag_AST(Code),
    [{def, _, _, Args, [Clause]}] = Tagged,
    [{symbol, _, a, TaggedDefArg}] = Args,
    {clause, _, [{symbol, _, a, TaggedArg}], {symbol, _, a, TaggedArg}} = Clause,
    ?assertNotEqual(TaggedDefArg, TaggedArg).

tuple_test() ->
    Code = "def not a -> (a, a)",
    [{def, _, _, Args, {tuple, _, [L1, L2]}}] = tag_AST(Code),
    [{symbol, _, a, TaggedArg}] = Args,
    {symbol, _, a, TaggedArg} = L1,
    {symbol, _, a, TaggedArg} = L2.
    
anonymous_function_test() ->
    Code = 
        "def blap a -> a.blip(b -> b\n"
        "                     _ -> a)",
    Tagged = tag_AST(Code),
    [{def, _, _, DefArgs, {application, _, _, Args}}] = Tagged,
    [_, {clauses, _, [Clause1, Clause2]}] = Args,
    [{symbol, _, a, TaggedA}] = DefArgs,
    {clause, _, [{symbol, _, b, TaggedB}], {symbol, _, b, TaggedB}} = Clause1,
    {clause, _, _, {symbol, _, a, TaggedA}} = Clause2.

-endif.
