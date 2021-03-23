-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

tag(Code) ->
    case parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, [{module, _, _, _, _, Defs} | _]} -> Defs
    end.


identity_function_test_() ->
    Code = "def id a -> a",
    Defs = tag(Code),
    ?test(#{id := {def, _, id,
                   {'fun', _,
                    [{clause, _,
                      [{variable, _, a, A}],
                      {variable, _, a, A}}]}}}, Defs).

pattern_match1_test_() ->
    Code = 
        "def not\n"
        " | b -> b",
    Defs = tag(Code),
    ?test(#{'not' := {def, _, 'not',
                      {'fun', _,
                       [{clause, _, [{variable, _, b, B}], {variable, _, b, B}}]}}}, Defs).

tuple_test_() ->
    Code = "def not a -> (a, a)",
    Defs = tag(Code),
    ?test(#{'not' := {def, _, 'not',
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, a, A}],
                         {seq, _, {variable, _, a, A}, {variable, _, a, A}}}]}}}, Defs).
    
anonymous_function_test_() ->
    Code = 
        "def blip a -> a\n"
        "def blap a -> a.blip(| b -> b\n"
        "                     | _ -> a)",
    Defs = tag(Code),
    [?test(#{blap := {def, _, blap,
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, a, A2}],
                         {application, _,
                          {variable, _, blip, {blip, 1}},
                          [{variable, _, a, A2},
                           {'fun', _,
                            [{clause, _, [{variable, _, b, B}], {variable, _, b, B}},
                             {clause, _, [{variable, _, '_', _}], {variable, _, a, A2}}]}]}}]}}}, Defs),
     ?test(#{blip := {def, _, blip,
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, a, A1}],
                         {variable, _, a, A1}}]}}}, Defs)].

dict_pair_test_() ->
    Code = "def f b -> {a: b}",
    Defs = tag(Code),
    ?test(#{f := {def, _, f,
                  {'fun', _,
                   [{clause, _,
                     [{variable, _, b, B}],
                     {dict, _,
                      [{pair, _,
                        {key, _, a},
                        {variable, _, b, B}}]}}]}}}, Defs).


dict_value_test_() ->
    Code = "def f d a -> d: {a}",
    Defs = tag(Code),
    ?test(#{f := {def, _, f,
                  {'fun', _,
                   [{clause, _,
                     [{variable, _, d, D},
                      {variable, _, a, _A}],
                     {pair,_,
                      {variable, _, d, D},
                      {dict, _,
                       [{key, _, a}]}}}]}}}, Defs).


simple_sum_type_test_() ->
    Code =
        "type Boolean -> (True | False)\n"
        "def blah\n"
        " | Boolean/True -> Boolean/False",
    Defs = tag(Code),
    [?test(#{'Boolean' := {type_def, _, 'Boolean',
                           {sum, _,
                            [{type, _, 'True', ['Boolean', 'True']},
                             {type, _, 'False', ['Boolean', 'False']}]}}}, Defs),
     ?test(#{'Boolean/False' := {type_def, _, 'False', {type, _, 'False', ['Boolean', 'False']}}}, Defs),
     ?test(#{'Boolean/True' := {type_def, _, 'True', {type, _, 'True', ['Boolean', 'True']}}}, Defs),
     ?test(#{blah := {def, _, blah,
                      {'fun', _,
                       [{clause, _,
                         [{type, _, 'True', ['Boolean', 'True']}],
                         {type, _, 'False', ['Boolean', 'False']}}]}}}, Defs)].

complex_sum_syntax_test_() ->
    Code =
        "\n"
        "type Animal -> (Cat | Dog |\n"
        "                Parrot | Seagull\n"
        "                Brontosaurus)",
    Defs = tag(Code),
    [?test(#{'Animal' := {type_def, _, 'Animal',
                          {sum, _,
                           [{type, _, 'Cat', ['Animal', 'Cat']},
                            {type, _, 'Dog', ['Animal', 'Dog']},
                            {type, _, 'Parrot', ['Animal', 'Parrot']},
                            {type, _, 'Seagull', ['Animal', 'Seagull']},
                            {type, _, 'Brontosaurus', ['Animal', 'Brontosaurus']}]}}}, Defs),
     ?test(#{'Animal/Brontosaurus' := {type_def, _, 'Brontosaurus',
                                       {type, _, 'Brontosaurus', ['Animal', 'Brontosaurus']}}}, Defs),
     ?test(#{'Animal/Cat' := {type_def, _, 'Cat',
                                       {type, _, 'Cat', ['Animal', 'Cat']}}}, Defs),
     ?test(#{'Animal/Dog' := {type_def, _, 'Dog',
                                       {type, _, 'Dog', ['Animal', 'Dog']}}}, Defs),
     ?test(#{'Animal/Parrot' := {type_def, _, 'Parrot',
                                       {type, _, 'Parrot', ['Animal', 'Parrot']}}}, Defs),
     ?test(#{'Animal/Seagull' := {type_def, _, 'Seagull',
                                       {type, _, 'Seagull', ['Animal', 'Seagull']}}}, Defs)].

simple_product_type_test_() ->
    Code =
        "type Monkey -> Monkey: { food: Banana, plant: Trees }",
    Defs = tag(Code),
    [?test(#{'Monkey' := {type_def, _, 'Monkey',
                          {tagged, _, ['Monkey'],
                           {dict, _,
                            [{pair,_,
                              {key,_,food},
                              {type,_,'Banana', ['Monkey', 'Banana']}},
                             {pair,_,
                              {key,_,plant},
                              {type,_,'Trees',['Monkey', 'Trees']}}]}}}}, Defs),
     ?test(#{'Monkey/Banana' := {type_def, _, 'Banana', _}}, Defs),
     ?test(#{'Monkey/Trees' := {type_def, _, 'Trees', _}}, Defs)].

complex_type_test_() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    Defs = tag(Code),
    [?test(#{'BooleanList' := {type_def,_,'BooleanList',
                               {sum,_,
                                [{tagged,_,['BooleanList','Cons'],
                                  {dict,_,
                                   [{pair,_,
                                     {key,_,value},
                                     {sum,_,
                                      [{type,_,'True',['BooleanList','True']},
                                       {type,_,'False',['BooleanList','False']}]}},
                                    {pair,_,
                                     {key,_,cons},
                                     {type,_,'BooleanList',['BooleanList']}}]}},
                                 {type,_,'Nil',['BooleanList','Nil']}]}}}, Defs),
     ?test(#{'BooleanList/True' := {type_def, _, 'True', _}}, Defs),
     ?test(#{'BooleanList/False' := {type_def, _, 'False', _}}, Defs)].



product_key_not_propagated_test_() ->
    Code =
        "type Blip -> { blup: Blyp }\n"
        "def blap -> blup",
    ?testError({undefined_symbol, blup}, tag(Code)).

pattern_product_key_propagated_test_() ->
    Code = "def test\n"
           " | {b, c} -> b(c)",
    Tagged = tag(Code),
    ?test(#{test := {def, _, test,
                     {'fun', _,
                      [{clause,_,
                        [{dict,_,
                          [{variable,_,b, _B},
                           {variable,_,c, _C}]}],
                        {application,_,
                         {variable,_,b, _B},
                         [{variable,_,c, _C}]}}]}}}, Tagged).

undefined_variable_test_() ->
    Code = "def test -> a",
    ?testError({undefined_symbol, a}, tag(Code)).

undefined_qualified_symbol_test_() ->
    Code = "def test -> T/T",
    ?testError({undefined_symbol, 'T/T'}, tag(Code)).

val_test_() ->
    Code = "def test a -> (val f = | b -> a,\n"
           "               f(a))",
    ?test(#{test := {def, _, test,
                     {'fun', _,
                      [{clause, _,
                        [{variable, _, a, A1}],
                        {'let', _, {variable, _, f, F},
                         {'fun', _,
                          [{clause, _,
                            [{variable, _, b, _B}],
                            {variable, _, a, A1}}]},
                         {application, _, {variable, _, f, F},
                          [{variable, _, a, A1}]}}}]}}}, tag(Code)).

local_import_conflict_test_() ->
    Code = "type T -> (A | B)
            import T/_",
    Defs = tag(Code),
    [?test(#{'T' := {type_def, _, 'T', {sum, _,
                                        [{type, _, 'A', ['T', 'A']},
                                         {type, _, 'B', ['T', 'B']}]}}}, Defs),
     ?test(#{'T/A' := {type_def, _, 'A', {type, _, 'A', ['T', 'A']}}}, Defs),
     ?test(#{'T/B' := {type_def, _, 'B', {type, _, 'B', ['T', 'B']}}}, Defs)].

type_variable_test_() ->
    Code = "type F a -> a",
    Defs = tag(Code),

    ?test(#{'F' := {type_def, _, 'F',
                    {'fun', _,
                     [{clause, _,
                       [{variable, _, 'a', A}],
                       {variable, _, 'a', A}}]}}}, Defs).

tag_sub_module_test_() ->
    Code = "type List a -> (Nil | Cons: { head: a, tail: List(a) })",
    {ok, Parsed} = parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}),
    ModMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Parsed]),
    % Should result in `source_test_code` module and `source_test_code_List` module.
    % We're interested in the latter.
    #{'source/test_code' := {module, _, _, _, _, #{'List/Cons' := Cons}}} = ModMap,
    ?test({type_def, _, 'Cons',
                {'fun', _,
                 [{clause, _,
                   [{pair, _, {variable, _, _, Sub1}, {variable, _, a, _}},
                    {pair, _, {variable, _, _, Sub2}, {application, _,
                                                       {type, _, 'List', ['List']},
                                                       [{type, _, any, _}]}}],
                   {tagged, _, ['List', 'Cons'],
                    {dict, _, [{pair, _, {key, _, head}, {variable, _, _, Sub1}},
                               {pair, _, {key, _, tail}, {variable, _, _, Sub2}}]}}}]}}, Cons).
