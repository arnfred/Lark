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
    ?assertMatch({variable, _, a, _}, Key).

dict_value_test() ->
    Code = "def f -> {a}",
    {_, Tagged} = tag_AST(Code),
    [{def, _, _, [], {dict, _, [Key]}}] = Tagged,
    ?assertMatch({variable, _, a, _}, Key).


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
    ?assertMatch([{type_def, _, 'Monkey', [],
                 {pair, _,
                  {type, _, ['Monkey']},
                  {dict, _,
                   [{pair,_,
                     {variable,_,food,_},
                     {type,_,['Monkey', 'Banana']}},
                    {pair,_,
                     {variable,_,plant,_},
                     {type,_,['Monkey', 'Trees']}}]}}}], Typed).

complex_type_test() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    {Typed, _} = tag_AST(Code),
    ?assertMatch([{type_def,_,'BooleanList',[],
                   {tuple,_,
                    [{pair,_,
                      {type,_,['BooleanList','Cons']},
                      {dict,_,
                       [{pair,_,
                         {variable,_,value,_},
                         {tuple,_,
                          [{type,_,['BooleanList','True']},
                           {type,_,['BooleanList','False']}]}},
                        {pair,_,
                         {variable,_,cons,_},
                         {type,_,['BooleanList']}}]}},
                     {type,_,['BooleanList','Nil']}]}}], Typed).

product_key_not_propagated_test() ->
    Code =
        "type Blip -> { blup: blyp }\n"
        "def blap -> blup",
    {Typed, Tagged} = tag_AST(Code),
    [{type_def, _, 'Blip', [], {dict, _, [{pair, _, {variable, _, 'blup', BlupKey}, _}]}}] = Typed,
    [{def, _, 'blap', [], {variable, _, 'blup', BlupTag}}] = Tagged,
    ?assertNotEqual(BlupKey, BlupTag).

pattern_product_key_propagated_test() ->
    Code = "def test a\n"
           " | {b, c} -> b(c)",
    {_, Tagged} = tag_AST(Code),
    ?assertMatch([{def,_,test,
                   [{variable,_,a,_}],
                   [{clause,_,
                     [{dict,_,
                       [{variable,_,b,B},
                        {variable,_,c,C}]}],
                     {application,_,
                      {variable,_,b,B},
                      [{variable,_,c,C}]}}]}],
                 Tagged).
