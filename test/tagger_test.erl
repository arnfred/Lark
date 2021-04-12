-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

tag(Code) ->
    case parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, Modules} ->
            ModuleMap = maps:from_list([{module:path(M), M} || M <- Modules]),
            case maps:get([source, test_code], ModuleMap) of
                {module, _, _, _, _, Defs} -> Defs
            end
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
                        {keyword, _, a},
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
                       [{keyword, _, a}]}}}]}}}, Defs).


simple_sum_type_test_() ->
    Code =
        "def boolean -> (True | False)\n"
        "def blah\n"
        " | boolean/True -> boolean/False",
    Defs = tag(Code),
    [?test(#{'boolean' := {def, _, 'boolean',
                           {sum, _,
                            [{keyword, _, [source, test_code, 'boolean'], 'True'},
                             {keyword, _, [source, test_code, 'boolean'], 'False'}]}}}, Defs),
     ?test(#{'boolean/False' := {keyword, _, [source, test_code, 'boolean'], 'False'}}, Defs),
     ?test(#{'boolean/True' := {keyword, _, [source, test_code, 'boolean'], 'True'}}, Defs),
     ?test(#{blah := {def, _, blah,
                      {'fun', _,
                       [{clause, _,
                         [{keyword, _, [source, test_code, 'boolean'], 'True'}],
                         {keyword, _, [source, test_code, 'boolean'], 'False'}}]}}}, Defs)].

complex_sum_syntax_test_() ->
    Code =
        "\n"
        "def animal -> (Cat | Dog |\n"
        "               Parrot | Seagull\n"
        "               Brontosaurus)",
    Defs = tag(Code),
    [?test(#{'animal' := {def, _, 'animal',
                          {sum, _,
                           [{keyword, _, [source, test_code, 'animal'], 'Cat'},
                            {keyword, _, [source, test_code, 'animal'], 'Dog'},
                            {keyword, _, [source, test_code, 'animal'], 'Parrot'},
                            {keyword, _, [source, test_code, 'animal'], 'Seagull'},
                            {keyword, _, [source, test_code, 'animal'], 'Brontosaurus'}]}}}, Defs),
     ?test(#{'animal/Brontosaurus' := {keyword, _, [source, test_code, 'animal'], 'Brontosaurus'}}, Defs),
     ?test(#{'animal/Cat' := {keyword, _, [source, test_code, 'animal'], 'Cat'}}, Defs),
     ?test(#{'animal/Dog' := {keyword, _, [source, test_code, 'animal'], 'Dog'}}, Defs),
     ?test(#{'animal/Parrot' := {keyword, _, [source, test_code, 'animal'], 'Parrot'}}, Defs),
     ?test(#{'animal/Seagull' := {keyword, _, [source, test_code, 'animal'], 'Seagull'}}, Defs)].

simple_product_type_test_() ->
    Code =
        "def monkey -> Monkey: { food: Banana, plant: Trees }",
    Defs = tag(Code),
    [?test(#{'monkey' := {def, _, 'monkey',
                          {tagged, _, [monkey, 'Monkey'],
                           {dict, _,
                            [{pair,_,
                              {keyword,_,food},
                              {keyword,_,[source, test_code, 'monkey'], 'Banana'}},
                             {pair,_,
                              {keyword,_,plant},
                              {keyword,_,[source, test_code, 'monkey'], 'Trees'}}]}}}}, Defs),
     ?test(#{'monkey/Banana' := {keyword, _, _, 'Banana'}}, Defs),
     ?test(#{'monkey/Trees' := {keyword, _, _, 'Trees'}}, Defs)].

complex_type_test_() ->
    Code =
        "def booleanList -> (Cons: { value: (True | False)\n"
        "                             cons: booleanList }\n"
        "                     Nil)",
    Defs = tag(Code),
    [?test(#{'booleanList' := {def,_,'booleanList',
                               {sum,_,
                                [{tagged,_,['booleanList','Cons'],
                                  {dict,_,
                                   [{pair,_,
                                     {keyword,_,value},
                                     {sum,_,
                                      [{keyword,_,[source, test_code, 'booleanList'],'True'},
                                       {keyword,_,[source, test_code, 'booleanList'],'False'}]}},
                                    {pair,_,
                                     {keyword,_,cons},
                                     {variable,_,'booleanList',{'booleanList', 0}}}]}},
                                 {keyword,_,[source, test_code, 'booleanList'],'Nil'}]}}}, Defs),
     ?test(#{'booleanList/True' := {keyword, _, _, 'True'}}, Defs),
     ?test(#{'booleanList/False' := {keyword, _, _, 'False'}}, Defs)].



product_key_not_propagated_test_() ->
    Code =
        "def blip -> { blup: Blyp }\n"
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
    Code = "def t -> (A | B)
            import t/_",
    Defs = tag(Code),
    [?test(#{'t' := {def, _, 't', {sum, _,
                                   [{keyword, _, [source, test_code, 't'], 'A'},
                                    {keyword, _, [source, test_code, 't'], 'B'}]}}}, Defs),
     ?test(#{'t/A' := {keyword, _, [source, test_code, 't'], 'A'}}, Defs),
     ?test(#{'t/B' := {keyword, _, [source, test_code, 't'], 'B'}}, Defs)].

type_variable_test_() ->
    Code = "def f a -> a",
    Defs = tag(Code),

    ?test(#{'f' := {def, _, 'f',
                    {'fun', _,
                     [{clause, _,
                       [{variable, _, 'a', A}],
                       {variable, _, 'a', A}}]}}}, Defs).

tag_sub_module_test_() ->
    Code = "def list a -> (Nil | Cons: { head: a, tail: list(a) })",
    {ok, Parsed} = parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}),
    ModMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Parsed]),
    % Should result in `source_test_code` module and `source_test_code_List` module.
    % We're interested in the latter.
    #{'source/test_code' := {module, _, _, _, _, #{'list/Cons' := Cons}}} = ModMap,
    ?test({def, _, 'Cons',
           {'fun', _,
            [{clause, _,
              [{pair, _, {variable, _, _, Sub1}, {variable, _, a, _}},
               {pair, _, {variable, _, _, Sub2}, {application, _,
                                                  {variable, _, 'list', _},
                                                  [{keyword, _, '_'}]}}],
              {tagged, _, [source, test_code, 'list', 'Cons'],
               {dict, _, [{pair, _, {keyword, _, head}, {variable, _, _, Sub1}},
                          {pair, _, {keyword, _, tail}, {variable, _, _, Sub2}}]}}}]}}, Cons).

local_constant_test_() ->
    Code = "def t -> A
            import t/A
            def s -> A
            def r -> s/A",
    Defs = tag(Code),
    [?test(#{'s' := {def, _, 's', {keyword, _, [source, test_code, 't'], 'A'}}}, Defs),
     ?test(#{'r' := {def, _, 'r', {keyword, _, [source, test_code, 't'], 'A'}}}, Defs)].
