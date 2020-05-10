-module(tagger).
-export([tag/1]).

-include_lib("eunit/include/eunit.hrl").

tag(AST) -> 
    {TypeEnv, TaggedTypes} = tag_types(AST),
    {DefEnv, TaggedDefs} = tag_defs(TypeEnv, AST),
    {ok, {TypeEnv, DefEnv}, {TaggedTypes, TaggedDefs}}.

tag_types(AST) ->
    Types = lists:filter(fun(A) -> element(1,A) =:= type_def end, AST),
    TagType = fun(Path, {symbol, Line, T}) -> {type, Line, lists:reverse([T | Path])} end,
    InitEnv = maps:from_list([{Name, TagType([], {symbol, Line, Name})} || {type_def, Line, Name, _, _} <- Types]),
    map(InitEnv, Types, [], TagType).

tag_defs(TypeEnv, AST) ->
    Defs = lists:filter(fun(A) -> element(1,A) =:= def end, AST),
    TagDef = fun({def, Line, Name, Args, _}) -> {variable, Line, Name, {Name, length(Args)}} end,
    DefEnv = maps:from_list([{element(3, D), TagDef(D)} || D  <- Defs]),
    TagVal = fun(Path, {symbol, Line, S}) -> 
                     TypePath = lists:reverse([S | Path]),
                     case maps:get(S, TypeEnv, undefined) of
                         undefined -> {variable, Line, S, symbol:id(TypePath)};
                         Type -> Type
                     end 
             end,
    map(DefEnv, Defs, [], TagVal).

tag(Env, {def, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    {ArgsEnv, TaggedArgs} = map(#{}, Args, NewPath, TagFun),
    {BodyEnv, TaggedBody} = case Body of
                                _ when is_list(Body) -> map(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun);
                                _ -> tag(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun)
                            end,
    {BodyEnv, {def, Line, Name, TaggedArgs, TaggedBody}};

tag(Env, {lambda, Line, Clauses}, Path, TagFun) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, TagFun),
    {NewEnv, {lambda, Line, TaggedClauses}};

tag(Env, {clauses, Line, Clauses}, Path, TagFun) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, TagFun),
    {NewEnv, {clauses, Line, TaggedClauses}};

tag(Env, {clause, Line, Patterns, Expr}, Path, TagFun) ->
    {PatternEnv, TaggedPatterns} = map(#{}, Patterns, Path, TagFun),
    NewEnv = maps:merge(Env, PatternEnv),
    {BodyEnv, TaggedBody} = tag(NewEnv, Expr, Path, TagFun),
    {BodyEnv, {clause, Line, TaggedPatterns, TaggedBody}};

%% Notes on application:
%% Without knowing the type of `Name`, we can't know what the symbol 
%% resolves to. It might resolve to one of following:
%%  1. A function defined for a constraint. We can only know by finding the
%%     type of the noun. Example: `map` on list
%%  2. An accessor function for a type product (which strictly speaking 
%%     can be seen as a case of the above)
%%  3. A global or local non-type definition
%%
%%  In order to correctly assign the symbol we need to either:
%%   1. Be able to narrow down the domain to one possible option
%%   2. Generate a pattern match which would choose between options
tag(Env, {application, Line, Name, Args}, Path, TagFun) ->
    {NameEnv, TaggedName} = tag(Env, Name, Path, TagFun),
    {NewEnv, TaggedArgs} = map(NameEnv, Args, Path, TagFun),
    {NewEnv, {application, Line, TaggedName, TaggedArgs}};

tag(Env, {match, Line, Expr, Clauses}, Path, TagFun) ->
    {ExprEnv, TaggedExpr} = tag(Env, Expr, Path, TagFun),
    {ClausesEnv, TaggedClauses} = map(ExprEnv, Clauses, Path, TagFun),
    {ClausesEnv, {match, Line, TaggedExpr, TaggedClauses}};

tag(Env, {tuple, Line, Expressions}, Path, TagFun) ->
    {EnvExpr, TaggedExpressions} = fold(Env, Expressions, Path, TagFun),
    {EnvExpr, {tuple, Line, TaggedExpressions}};

tag(Env, {dict, Line, Expressions}, Path, TagFun) ->
    {EnvExpr, TaggedExpressions} = map(Env, Expressions, Path, TagFun),
    {EnvExpr, {dict, Line, TaggedExpressions}};

tag(Env, {type_def, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    MakeVar = fun(L, S) -> {variable, L, S, symbol:id(lists:reverse([S | NewPath]))} end,
    TaggedArgs = [MakeVar(L, S) || {symbol, L, S} <- Args],
    ArgEnv = maps:from_list([{S, Arg} || {_, _, S, _} = Arg <- TaggedArgs]),
    {BodyEnv, TaggedBody} = tag(maps:merge(Env, ArgEnv), Body, NewPath, TagFun),
    {BodyEnv, {type_def, Line, Name, TaggedArgs, TaggedBody}};

%% We deliberately don't include the KeyEnv in the environment that is
%% returned. This is because the key of the pair in turn becomes an accessor
%% function, but we want local definitions and assignments to have precedence
%% over type product accessor functions.
tag(Env, {pair, Line, Key, Value}, Path, TagFun) ->
    {_, TaggedKey} = tag(Env, Key, Path, TagFun),
    {ValueEnv, TaggedValue} = tag(Env, Value, Path, TagFun),
    {ValueEnv, {pair, Line, TaggedKey, TaggedValue}};

tag(Env, {qualified_symbol, _, _} = QS, _, _) -> {Env, QS};

tag(Env, {symbol, _, S} = Symbol, Path, TagFun) ->
    NewSymbol = maps:get(S, Env, TagFun(Path, Symbol)),
    NewEnv = maps:put(S, NewSymbol, Env),
    {NewEnv, NewSymbol}.

map(Env, Elements, Path, TagFun) when is_list(Elements) ->
    {EnvList, Tagged} = lists:unzip([tag(Env, E, Path, TagFun) || E <- Elements]),
    NewEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, EnvList),
    {NewEnv, Tagged}.

fold(Env, Elements, Path, TagFun) when is_list(Elements) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                {NewEnv, TaggedExpr} = tag(EnvAcc, Expr, Path, TagFun),
                {maps:merge(EnvAcc, NewEnv), [TaggedExpr | ExprAcc]} 
        end,
    {NewEnv, TaggedElements} = lists:foldl(F, {Env, []}, Elements),
    {NewEnv, lists:reverse(TaggedElements)}.

-ifdef(TEST).

tag_AST(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    {ok, _, Tagged} = tag(AST),
    Tagged.

identity_function_test() ->
    {_, [{def, _, _, Args, Body}]} = tag_AST("def id a -> a"),
    {variable, _, a, TaggedArg} = Body,
    [{variable, _, a, TaggedBody}] = Args,
    ?assertEqual(TaggedArg, TaggedBody).

pattern_match1_test() ->
    Code = 
        "def not a\n"
        " | b -> b",
    {_, [{def, _, _, _, [Clause]}]} = tag_AST(Code),
    {clause, _, [{variable, _, b, TaggedArg}], {variable, _, b, TaggedArg}} = Clause.

pattern_match2_test() ->
    Code = 
        "def not a\n"
        " | b -> a",
    {_, [{def, _, _, Args, [Clause]}]} = tag_AST(Code),
    [{variable, _, a, TaggedArg}] = Args,
    {clause, _, _, {variable, _, a, TaggedArg}} = Clause.

pattern_match3_test() ->
    Code = 
        "def not a\n"
        " | a -> a",
    {_, Tagged} = tag_AST(Code),
    [{def, _, _, Args, [Clause]}] = Tagged,
    [{variable, _, a, TaggedDefArg}] = Args,
    {clause, _, [{variable, _, a, TaggedArg}], {variable, _, a, TaggedArg}} = Clause,
    ?assertNotEqual(TaggedDefArg, TaggedArg).

tuple_test() ->
    Code = "def not a -> (a, a)",
    {_, [{def, _, _, Args, {tuple, _, [L1, L2]}}]} = tag_AST(Code),
    [{variable, _, a, TaggedArg}] = Args,
    {variable, _, a, TaggedArg} = L1,
    {variable, _, a, TaggedArg} = L2.
    
anonymous_function_test() ->
    Code = 
        "def blap a -> a.blip(b -> b\n"
        "                     _ -> a)",
    {_, Tagged} = tag_AST(Code),
    [{def, _, _, DefArgs, {application, _, _, Args}}] = Tagged,
    [_, {lambda, _, [Clause1, Clause2]}] = Args,
    [{variable, _, a, TaggedA}] = DefArgs,
    {clause, _, [{variable, _, b, TaggedB}], {variable, _, b, TaggedB}} = Clause1,
    {clause, _, _, {variable, _, a, TaggedA}} = Clause2.

simple_sum_type_test() ->
    Code =
        "type Boolean -> True | False\n"
        "def blah a\n"
        " | True -> False",
    {Typed, Tagged} = tag_AST(Code),
    [{type_def, _, _, _, {tuple, _, [{_, _, True}, {_, _, False}]}}] = Typed,
    ?assertEqual(True, ['Boolean','True']),
    ?assertEqual(False, ['Boolean', 'False']),
    [{def, _, _, _, [Clause]}] = Tagged,
    {clause, _, [{_, _, TrueClause}], {_, _, FalseExpr}} = Clause,
    ?assertEqual(TrueClause, ['Boolean', 'True']),
    ?assertEqual(FalseExpr, ['Boolean', 'False']).

complex_sum_syntax_test() ->
    Code =
        "\n"
        "type Animal -> (Cat | Dog\n"
        "                Parrot | Seagull\n"
        "                Brontosaurus)",
    {Typed, _} = tag_AST(Code),
    [{type_def, _, _, _, {tuple, _, [{_, _, Cat}, 
                                     {_, _, Dog},
                                     {_, _, Parrot},
                                     {_, _, Seagull},
                                     {_, _, Brontosaurus}]}}] = Typed,
    ?assertEqual(Cat, ['Animal','Cat']),
    ?assertEqual(Dog, ['Animal','Dog']),
    ?assertEqual(Parrot, ['Animal','Parrot']),
    ?assertEqual(Seagull, ['Animal','Seagull']),
    ?assertEqual(Brontosaurus, ['Animal','Brontosaurus']).

simple_product_type_test() ->
    Code =
        "type Monkey -> Monkey: { food: Banana, plant: Trees }",
    {Typed, _} = tag_AST(Code),
    Expected = [{type_def, 1, 'Monkey', [],
                 {pair, 1,
                  {type, 1, ['Monkey']},
                  {dict, 1,
                   [{pair,1,
                     {type,1,['Monkey', 'food']},
                     {type,1,['Monkey', 'Banana']}},
                    {pair,1,
                     {type,1,['Monkey', 'plant']},
                     {type,1,['Monkey', 'Trees']}}]}}}],
    ?assertEqual(Expected, Typed).

complex_type_test() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    {Typed, _} = tag_AST(Code),
    ?assertEqual([{type_def,1,'BooleanList',[],
                   {tuple,1,
                    [{pair,1,
                      {type,1,['BooleanList','Cons']},
                      {dict,1,
                       [{pair,1,
                         {type,1,['BooleanList',value]},
                         {tuple,1,
                          [{type,1,['BooleanList','True']},
                           {type,1,['BooleanList','False']}]}},
                        {pair,2,
                         {type,2,['BooleanList',cons]},
                         {type,1,['BooleanList']}}]}},
                     {type,3,['BooleanList','Nil']}]}}], Typed).

product_key_not_propagated_test() ->
    Code =
        "type Blip -> { blup: blyp }\n"
        "def blap -> blup",
    {Typed, Tagged} = tag_AST(Code),
    [{type_def, 1, 'Blip', [], {dict, _, [{pair, 1, {type, _, BlupKey}, _}]}}] = Typed,
    [{def, 2, 'blap', [], {variable, 2, 'blup', BlupTag}}] = Tagged,
    ?assertNotEqual(BlupKey, BlupTag).


-endif.
