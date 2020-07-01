-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

identity_function_test() ->
    Code = "def id a -> a",
    ?assertMatch({ok, {_,
                       [{def, _, id, [{variable, _, a, A}],
                         {variable, _, a, A}}]}},
                 kind:get_AST(Code)).

pattern_match1_test() ->
    Code = 
        "def not a\n"
        " | b -> b",
    ?assertMatch({ok, {_,
                       [{def, _, 'not', [{variable, _, a, _A}],
                         [{clause, _, [{variable, _, b, B}], {variable, _, b, B}}]}]}},
                 kind:get_AST(Code)).

pattern_match2_test() ->
    Code = 
        "def not a\n"
        " | b -> a",
    ?assertMatch({ok, {_,
                       [{def, _, 'not', [{variable, _, a, A}],
                         [{clause, _, [{variable, _, b, _B}], {variable, _, a, A}}]}]}},
                 kind:get_AST(Code)).

tuple_test() ->
    Code = "def not a -> (a, a)",
    ?assertMatch({ok, {_,
                       [{def, _, 'not', [{variable, _, a, A}],
                         {seq, _, {variable, _, a, A}, {variable, _, a, A}}}]}},
                 kind:get_AST(Code)).
    
anonymous_function_test() ->
    Code = 
        "def blip a -> a\n"
        "def blap a -> a.blip(b -> b\n"
        "                     _ -> a)",
    ?assertMatch({ok, {_,
                       [{def, _, blip,
                         [{variable, _, a, A1}],
                         {variable, _, a, A1}},
                        {def, _, blap,
                         [{variable, _, a, A2}],
                         {application, _,
                          {variable, _, blip, {blip, 1}},
                          [{variable, _, a, A2},
                           {lambda, _,
                            [{clause, _, [{variable, _, b, B}], {variable, _, b, B}},
                             {clause, _, [{variable, _, '_', _}], {variable, _, a, A2}}]}]}}]}},
                 kind:get_AST(Code)).

dict_pair_test() ->
    Code = "def f b -> {a: b}",
    ?assertMatch({ok, {_,
                       [{def, _, _, [{variable, _, b, B}],
                         {dict, _,
                          [{pair, _,
                            {key, _, a},
                            {variable, _, b, B}}]}}]}}, kind:get_AST(Code)).


dict_value_test() ->
    Code = "def f d a -> d: {a}",
    ?assertMatch({ok, {_,
                       [{def, _, _,
                         [{variable, _, d, D},
                          {variable, _, a, _A}],
                         {pair,_,
                          {variable, _, d, D},
                          {dict, _,
                           [{key, _, a}]}}}]}}, kind:get_AST(Code)).


simple_sum_type_test() ->
    Code =
        "type Boolean -> True | False\n"
        "def blah a\n"
        " | Boolean/True -> Boolean/False",
    ?assertMatch({ok, {_, [{type_def, _, 'Boolean', [],
                            {tuple, _,
                             [{type, _, 'True', ['Boolean', 'True']},
                              {type, _, 'False', ['Boolean', 'False']}]}},
                           {def, _, blah, [{variable, _, a, _}],
                            [{clause, _,
                              [{type, _, 'True', ['Boolean', 'True']}],
                              {type, _, 'False', ['Boolean', 'False']}}]}]}},
                 kind:get_AST(Code)).

complex_sum_syntax_test() ->
    Code =
        "\n"
        "type Animal -> (Cat | Dog |\n"
        "                Parrot | Seagull\n"
        "                Brontosaurus)",
    ?assertMatch({ok, {_, [{type_def, _, 'Animal', [],
                            {tuple, _,
                             [{type, _, 'Cat', ['Animal', 'Cat']},
                              {type, _, 'Dog', ['Animal', 'Dog']},
                              {type, _, 'Parrot', ['Animal', 'Parrot']},
                              {type, _, 'Seagull', ['Animal', 'Seagull']},
                              {type, _, 'Brontosaurus', ['Animal', 'Brontosaurus']}]}}]}},
                 kind:get_AST(Code)).

simple_product_type_test() ->
    Code =
        "type Monkey -> Monkey: { food: Banana, plant: Trees }",
    ?assertMatch({ok, {_, [{type_def, _, 'Monkey', [],
                            {pair, _,
                             {type, _, 'Monkey', ['Monkey']},
                             {dict, _,
                              [{pair,_,
                                {key,_,food},
                                {type,_,'Banana',['Monkey', 'Banana']}},
                               {pair,_,
                                {key,_,plant},
                                {type,_,'Trees',['Monkey', 'Trees']}}]}}}]}}, kind:get_AST(Code)).

complex_type_test() ->
    Code =
        %"type BooleanList -> (Cons: BooleanList\n"
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    ?assertMatch({ok, {_, [{type_def,_,'BooleanList',[],
                           {tuple,_,
                            [{pair,_,
                              {type,_,'Cons',['BooleanList','Cons']},
                              {dict,_,
                               [{pair,_,
                                 {key,_,value},
                                 {tuple,_,
                                  [{type,_,'True',['BooleanList','True']},
                                   {type,_,'False',['BooleanList','False']}]}},
                                {pair,_,
                                 {key,_,cons},
                                 {type,_,'BooleanList',['BooleanList']}}]}},
                             {type,_,'Nil',['BooleanList','Nil']}]}}]}}, kind:get_AST(Code)).


product_key_not_propagated_test() ->
    Code =
        "type Blip -> { blup: Blyp }\n"
        "def blap -> blup",
    ?errorMatch({undefined_variable, blup}, kind:get_AST(Code)).

pattern_product_key_propagated_test() ->
    Code = "def test a\n"
           " | {b, c} -> b(c)",
    Tagged = kind:get_AST(Code),
    ?assertMatch({ok, {#{a := {variable, _, a, A},
                         b := {variable, _, b, B},
                         c := {variable, _, c, C}},
                       [{def,_,test,
                         [{variable,_,a,A}],
                         [{clause,_,
                           [{dict,_,
                             [{variable,_,b,B},
                              {variable,_,c,C}]}],
                           {application,_,
                            {variable,_,b,B},
                            [{variable,_,c,C}]}}]}]}}, Tagged).

undefined_type_test() ->
    Code = "def test -> T",
    ?errorMatch({undefined_type, 'T'}, kind:get_AST(Code)).

undefined_variable_test() ->
    Code = "def test -> a",
    ?errorMatch({undefined_variable, a}, kind:get_AST(Code)).

symbol_already_defined_test() ->
    Code = "def test a\n"
           " | a -> a",
    ?errorMatch({symbol_in_pattern_already_defined, a}, kind:get_AST(Code)).

undefined_qualified_type_test() ->
    Code = "def test -> T/T",
    ?errorMatch({undefined_type, 'T/T'}, kind:get_AST(Code)).

nested_def_test_() ->
    Code = "def test a -> (def f b -> a,\n"
           "               f(a))",
    ?_assertMatch({ok, {_,
                        [{def, _, test, [{variable, _, a, A1}],
                          {'let', _, {variable, _, f, F},
                           {def, _, f, [{variable, _, b, _B}], {variable, _, a, _A2}},
                           {application, _, {variable, _, f, F},
                            [{variable, _, a, A1}]}}}]}}, kind:get_AST(Code)).

nested_type_test_() ->
    Code = "def match a f -> f(a)\n"
           "def test a -> (type T -> A | B,\n"
           "               a.match(T -> a\n"
           "                       T/A -> T/B))",
    ?_assertMatch({ok, {_,
                        [_,
                         {def, _, test, [{variable, _, a, _A}],
                          {let_type, _,
                           {type_def, _, 'T', [],
                            {tuple, _, [{type, _, 'A', ['T', 'A']},
                                        {type, _, 'B', ['T', 'B']}]}},
                           {application, _, {variable, _, match, {match, 2}},
                            [{variable, _, a, _A},
                             {lambda, _,
                              [{clause, _, [{type, _, 'T', ['T']}],
                                {variable, _, a, _A}},
                               {clause, _, [{type, _, 'A', ['T', 'A']}],
                                {type, _, 'B', ['T', 'B']}}]}]}}}]}},
                  kind:get_AST(Code)).

type_already_defined_error_test() ->
    Code = "type B -> A | B\n"
           "def test -> (type B -> C | B/A,\n"
           "             B/C)",
    ?errorMatch({type_already_defined, 'B'}, kind:get_AST(Code)).

type_variable_test() ->
    Code = "type F a -> a",
    ?assertMatch({ok, {_,
                       [{type_def, _, 'F', [{variable, _, 'a', A}],
                         {variable, _, 'a', A}}]}}, kind:get_AST(Code)).
