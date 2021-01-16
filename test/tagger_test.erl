-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

get_AST(Code) ->
    case parser:parse([{text, Code}], #{import_kind_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, [{ast, _, _, _, Defs} | _]} -> {ok, maps:values(Defs)}
    end.


identity_function_test_() ->
    Code = "def id a -> a",
    ?test({ok, [{def, _, id,
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A}],
                           {variable, _, a, A}}]}}]},
                 get_AST(Code)).

pattern_match1_test_() ->
    Code = 
        "def not\n"
        " | b -> b",
    ?test({ok, [{def, _, 'not',
                        {'fun', _,
                         [{clause, _, [{variable, _, b, B}], {variable, _, b, B}}]}}]},
                 get_AST(Code)).

tuple_test_() ->
    Code = "def not a -> (a, a)",
    ?test({ok, [{def, _, 'not',
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A}],
                           {seq, _, {variable, _, a, A}, {variable, _, a, A}}}]}}]},
                 get_AST(Code)).
    
anonymous_function_test_() ->
    Code = 
        "def blip a -> a\n"
        "def blap a -> a.blip(| b -> b\n"
        "                     | _ -> a)",
    ?test({ok, [{def, _, blap,
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A2}],
                           {application, _,
                            {variable, _, blip, {blip, 1}},
                            [{variable, _, a, A2},
                             {'fun', _,
                              [{clause, _, [{variable, _, b, B}], {variable, _, b, B}},
                               {clause, _, [{variable, _, '_', _}], {variable, _, a, A2}}]}]}}]}},
                       {def, _, blip,
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A1}],
                           {variable, _, a, A1}}]}}]},
                 get_AST(Code)).

dict_pair_test_() ->
    Code = "def f b -> {a: b}",
    ?test({ok, [{def, _, f,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, b, B}],
                    {dict, _,
                     [{pair, _,
                       {key, _, a},
                       {variable, _, b, B}}]}}]}}]}, get_AST(Code)).


dict_value_test_() ->
    Code = "def f d a -> d: {a}",
    ?test({ok, [{def, _, f,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, d, D},
                     {variable, _, a, _A}],
                    {pair,_,
                     {variable, _, d, D},
                     {dict, _,
                      [{key, _, a}]}}}]}}]}, get_AST(Code)).


simple_sum_type_test_() ->
    Code =
        "type Boolean -> (True | False)\n"
        "def blah\n"
        " | Boolean/True -> Boolean/False",
    ?test({ok, [{type_def, _, 'Boolean',
                 {sum, _,
                  [{type, _, 'True', ['Boolean', 'True']},
                   {type, _, 'False', ['Boolean', 'False']}]}},
                {def, _, blah,
                 {'fun', _,
                  [{clause, _,
                    [{type, _, 'True', ['Boolean', 'True']}],
                    {type, _, 'False', ['Boolean', 'False']}}]}}]},
          get_AST(Code)).

complex_sum_syntax_test_() ->
    Code =
        "\n"
        "type Animal -> (Cat | Dog |\n"
        "                Parrot | Seagull\n"
        "                Brontosaurus)",
    ?test({ok, [{type_def, _, 'Animal',
                        {sum, _,
                         [{type, _, 'Cat', ['Animal', 'Cat']},
                          {type, _, 'Dog', ['Animal', 'Dog']},
                          {type, _, 'Parrot', ['Animal', 'Parrot']},
                          {type, _, 'Seagull', ['Animal', 'Seagull']},
                          {type, _, 'Brontosaurus', ['Animal', 'Brontosaurus']}]}}]},
                 get_AST(Code)).

simple_product_type_test_() ->
    Code =
        "type Monkey -> Monkey: { food: Banana, plant: Trees }",
    ?test({ok, [{type_def, _, 'Monkey',
                        {pair, _,
                         {type, _, 'Monkey', ['Monkey']},
                         {dict, _,
                          [{pair,_,
                            {key,_,food},
                            {type,_,'Banana',['Monkey', 'Banana']}},
                           {pair,_,
                            {key,_,plant},
                            {type,_,'Trees',['Monkey', 'Trees']}}]}}}]}, get_AST(Code)).

complex_type_test_() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    ?test({ok, [{type_def,_,'BooleanList',
                        {sum,_,
                         [{pair,_,
                           {type,_,'Cons',['BooleanList','Cons']},
                           {dict,_,
                            [{pair,_,
                              {key,_,value},
                              {sum,_,
                               [{type,_,'True',['BooleanList','True']},
                                {type,_,'False',['BooleanList','False']}]}},
                             {pair,_,
                              {key,_,cons},
                              {type,_,'BooleanList',['BooleanList']}}]}},
                          {type,_,'Nil',['BooleanList','Nil']}]}}]}, get_AST(Code)).


product_key_not_propagated_test_() ->
    Code =
        "type Blip -> { blup: Blyp }\n"
        "def blap -> blup",
    ?testError({undefined_variable, blup}, get_AST(Code)).

pattern_product_key_propagated_test_() ->
    Code = "def test\n"
           " | {b, c} -> b(c)",
    Tagged = get_AST(Code),
    ?test({ok, [{def, _, test,
                 {'fun', _,
                  [{clause,_,
                    [{dict,_,
                      [{variable,_,b, _B},
                       {variable,_,c, _C}]}],
                    {application,_,
                     {variable,_,b, _B},
                     [{variable,_,c, _C}]}}]}}]}, Tagged).

undefined_type_test_() ->
    Code = "def test -> T",
    ?testError({undefined_symbol, type, 'T'}, get_AST(Code)).

undefined_variable_test_() ->
    Code = "def test -> a",
    ?testError({undefined_variable, a}, get_AST(Code)).

undefined_qualified_symbol_test_() ->
    Code = "def test -> T/T",
    ?testError({undefined_symbol, 'T/T'}, get_AST(Code)).

nested_def_test_() ->
    Code = "def test a -> (def f b -> a,\n"
           "               f(a))",
    ?test({ok, [{def, _, test,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, a, A1}],
                    {'let', _, {variable, _, f, F},
                     {def, _, f,
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, b, _B}],
                         {variable, _, a, _A2}}]}},
                     {application, _, {variable, _, f, F},
                      [{variable, _, a, A1}]}}}]}}]}, get_AST(Code)).

nested_type_test_() ->
    Code = "def match a f -> f(a)\n"
           "def test a -> (type T -> (A | B),\n"
           "               a.match(\n"
           "                 | T -> a\n"
           "                 | T/A -> T/B))",
    ?test({ok, [_,
                {def, _, test,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, a, _A}],
                    {let_type, _,
                     {type_def, _, 'T',
                      {sum, _, [{type, _, 'A', ['T', 'A']},
                                {type, _, 'B', ['T', 'B']}]}},
                     {application, _, {variable, _, match, {match, 2}},
                      [{variable, _, a, _A},
                       {'fun', _,
                        [{clause, _, [{type, _, 'T', ['T']}],
                          {variable, _, a, _A}},
                         {clause, _, [{type, _, 'A', ['T', 'A']}],
                          {type, _, 'B', ['T', 'B']}}]}]}}}]}}]},
          get_AST(Code)).

type_already_defined_error_test_() ->
    Code = "type B -> (A | C)\n"
           "def test -> (type B -> (C | B/A),\n"
           "             B/C)",
    ?testError({type_already_defined, 'B'}, get_AST(Code)).

type_variable_test_() ->
    Code = "type F a -> a",
    ?test({ok, [{type_def, _, 'F',
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, 'a', A}],
                    {variable, _, 'a', A}}]}}]}, get_AST(Code)).
