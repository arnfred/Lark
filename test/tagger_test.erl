-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").

tag_AST(Code) ->
    {ok, Tokens, _} = lexer:string(Code),
    io:format("Tokens are ~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("AST is ~p~n", [AST]),
    {ok, _, Tagged} = tagger:tag(AST),
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

dict_pair_test() ->
    Code = "def f -> {a: b}",
    {_, Tagged} = tag_AST(Code),
    [{def, _, _, [], {dict, _, [{pair, _, Key, _}]}}] = Tagged,
    ?assertMatch({variable, 1, a, _}, Key).

dict_value_test() ->
    Code = "def f -> {a}",
    {_, Tagged} = tag_AST(Code),
    [{def, _, _, [], {dict, _, [Key]}}] = Tagged,
    ?assertMatch({variable, 1, a, _}, Key).


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
    ?assertMatch([{type_def, 1, 'Monkey', [],
                 {pair, 1,
                  {type, 1, ['Monkey']},
                  {dict, 1,
                   [{pair,1,
                     {variable,1,food,_},
                     {type,1,['Monkey', 'Banana']}},
                    {pair,1,
                     {variable,1,plant,_},
                     {type,1,['Monkey', 'Trees']}}]}}}], Typed).

complex_type_test() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    {Typed, _} = tag_AST(Code),
    ?assertMatch([{type_def,1,'BooleanList',[],
                   {tuple,1,
                    [{pair,1,
                      {type,1,['BooleanList','Cons']},
                      {dict,1,
                       [{pair,1,
                         {variable,1,value,_},
                         {tuple,1,
                          [{type,1,['BooleanList','True']},
                           {type,1,['BooleanList','False']}]}},
                        {pair,2,
                         {variable,2,cons,_},
                         {type,1,['BooleanList']}}]}},
                     {type,3,['BooleanList','Nil']}]}}], Typed).

product_key_not_propagated_test() ->
    Code =
        "type Blip -> { blup: blyp }\n"
        "def blap -> blup",
    {Typed, Tagged} = tag_AST(Code),
    [{type_def, 1, 'Blip', [], {dict, _, [{pair, 1, {variable, 1, 'blup', BlupKey}, _}]}}] = Typed,
    [{def, 2, 'blap', [], {variable, 2, 'blup', BlupTag}}] = Tagged,
    ?assertNotEqual(BlupKey, BlupTag).

pattern_product_key_propagated_test() ->
    Code = "def test a\n"
           " | {b, c} -> b(c)",
    {_, Tagged} = tag_AST(Code),
    ?assertMatch([{def,1,test,
                   [{variable,1,a,_}],
                   [{clause,2,
                     [{dict,2,
                       [{variable,2,b,B},
                        {variable,2,c,C}]}],
                     {application,2,
                      {variable,2,b,B},
                      [{variable,2,c,C}]}}]}],
                 Tagged).
