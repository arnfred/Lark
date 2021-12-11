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
        "def not
             b -> b",
    Defs = tag(Code),
    ?test(#{'not' := {def, _, 'not',
                      {'fun', _,
                       [{clause, _, [{variable, _, b, B}], {variable, _, b, B}}]}}}, Defs).

tuple_test_() ->
    Code = "def not a -> (a
                          a)",
    Defs = tag(Code),
    ?test(#{'not' := {def, _, 'not',
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, a, A}],
                         {seq, _, {variable, _, a, A}, {variable, _, a, A}}}]}}}, Defs).
    
anonymous_function_test_() ->
    Code = 
        "def blip a -> a
         def blap a -> a.blip(b -> b,
                              _ -> a)",
    Defs = tag(Code),
    [?test(#{blap := {def, _, blap,
                      {'fun', _,
                       [{clause, _,
                         [{variable, _, a, A2}],
                         {qualified_application, _, [source, test_code], blip,
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
    Code = "def f d a -> {a}",
    Defs = tag(Code),
    ?test(#{f := {def, _, f,
                  {'fun', _,
                   [{clause, _,
                     [{variable, _, d, _D},
                      {variable, _, a, _A}],
                     {dict, _,
                      [{keyword, _, a}]}}]}}}, Defs).


simple_sum_type_test_() ->
    Code =
        "def boolean -> (True | False)
         def blah
             boolean/True -> boolean/False",
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
        "def animal -> Cat | Dog |
                       Parrot | Seagull |
                       Brontosaurus",
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
        "def t -> Banana | Trees | (Monkey: 'any')
         def monkey (t/Monkey: { food: t/Banana, plant: t/Trees }) -> t/Banana",
    Defs = tag(Code),
    [?test(#{'monkey' := {def,_,
                          monkey,
                          {'fun',_,
                           [{clause,_,
                             [{tagged,_,
                               [source, test_code, t, 'Monkey'],
                               {dict,_,
                                [{pair,_,
                                  {keyword,_,
                                   food},
                                  {keyword,_,
                                   [source,test_code,t],
                                   'Banana'}},
                                 {pair,_,
                                  {keyword,_,
                                   plant},
                                  {keyword,_,
                                   [source,test_code,t],
                                   'Trees'}}]}}],
                             {keyword,_,
                              [source,test_code,t],
                              'Banana'}}]}}}, Defs),
     ?test(#{'t/Banana' := {keyword, _, _, 'Banana'}}, Defs),
     ?test(#{'t/Trees' := {keyword, _, _, 'Trees'}}, Defs)].

complex_type_test_() ->
    Code =
        "def booleanList -> (Cons: { value: True | False,
                                      cons: booleanList }) |
                            Nil",
    Defs = tag(Code),
    [?test(#{'booleanList' := {def,_,'booleanList',
                               {sum,_,
                                [{tagged,_,[source, test_code, 'booleanList','Cons'],
                                  {dict,_,
                                   [{pair,_,
                                     {keyword,_,value},
                                     {sum,_,
                                      [{keyword,_,[source, test_code, 'booleanList'],'True'},
                                       {keyword,_,[source, test_code, 'booleanList'],'False'}]}},
                                    {pair,_,
                                     {keyword,_,cons},
                                     {qualified_symbol,_,[source, test_code], 'booleanList'}}]}},
                                 {keyword,_,[source, test_code, 'booleanList'],'Nil'}]}}}, Defs),
     ?test(#{'booleanList/True' := {keyword, _, _, 'True'}}, Defs),
     ?test(#{'booleanList/False' := {keyword, _, _, 'False'}}, Defs)].



product_key_not_propagated_test_() ->
    Code =
        "def blip -> { blup: Blyp }\n"
        "def blap -> blup",
    ?testError({undefined_symbol, blup}, tag(Code)).

pattern_product_key_propagated_test_() ->
    Code = "def test
                {b, c} -> b(c)",
    Tagged = tag(Code),
    ?test(#{test := {def, _, test,
                     {'fun', _,
                      [{clause,_,
                        [{dict,_,
                          [{variable,_,b, B},
                           {variable,_,c, C}]}],
                        {application,_,
                         {variable,_,b, B},
                         [{variable,_,c, C}]}}]}}}, Tagged).

undefined_variable_test_() ->
    Code = "def test -> a",
    ?testError({undefined_symbol, a}, tag(Code)).

undefined_qualified_symbol_test_() ->
    Code = "def test -> T/T",
    ?testError({undefined_symbol, 'T/T'}, tag(Code)).

val_test_() ->
    Code = "def test a -> (val f = (fn b -> a)
                           f(a))",
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

val_seq_test_() ->
    Code = "def test a -> (val f = (fn b -> a)
                           f(a)
                           f(a))",
    ?test(#{test := {def, _, test,
                     {'fun', _,
                      [{clause, _,
                        [{variable, _, a, A1}],
                        {'let', _, {variable, _, f, F},
                         {'fun', _,
                          [{clause, _,
                            [{variable, _, b, _B}],
                            {variable, _, a, A1}}]},
                         {seq, _,
                          {application, _, {variable, _, f, F}, [{variable, _, a, A1}]},
                          {application, _, {variable, _, f, F}, [{variable, _, a, A1}]}}}}]}}},
          tag(Code)).
parens_variable_test_() ->
    Code = "def f (((a))) -> (((a)))",
    Defs = tag(Code),
    ?test(#{'f' := {def, _, 'f',
                    {'fun', _,
                     [{clause, _,
                       [{variable, _, 'a', A}],
                       {variable, _, 'a', A}}]}}}, Defs).

tag_sub_module_test_() ->
    Code = "def list a -> Nil | (Cons: { head: a, tail: list(a) })",
    {ok, Parsed} = parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}),
    ModMap = maps:from_list([{module:kind_name(Path), Mod} || {module, _, Path, _, _, _} = Mod <- Parsed]),
    % Should result in `source_test_code` module and `source_test_code_List` module.
    % We're interested in the latter.
    #{'source/test_code' := {module, _, _, _, _, #{'list/Cons' := Cons}}} = ModMap,
    ?test({def, _, 'Cons',
           {'fun', _,
            [{clause, _,
              [{pair, _, {variable, _, _, Sub1}, {variable, _, a, _}},
               {pair, _, {variable, _, _, Sub2}, {qualified_application, _, [source, test_code], list,
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

tagged_type_test_() ->
    Code = "def tagged-type -> (T: 4 | 5)
            import tagged-type/_
            def t (T: 4) -> 4",
    Defs = tag(Code),
    [?test(#{'t' := {def, _, 't', {'fun', _,
                                   [{clause, _,
                                    [{tagged, _, [source, test_code, 'tagged-type', 'T'], {value, _, integer, 4}}],
                                    {value, _, integer, 4}}]}}}, Defs)].

module_keyword_test_() ->
    Code = "module m (export {boolean}
                      def boolean -> (True | False))
            import m/boolean/_
            def f False -> True",
    Defs = tag(Code),
    [?test(#{'f' := {def, _, 'f', {'fun', _,
                                   [{clause, _,
                                    [{keyword, _, [m, boolean], 'False'}],
                                    {keyword, _, [m, boolean], 'True'}}]}}}, Defs)].

def_tag_test_() ->
    Code = "def d a -> {a: a}
            def f (b: d(T)) -> b",
    Defs = tag(Code),
    [?test(#{'f' := {def, _, 'f', {'fun', _,
                                   [{clause, _,
                                     [{pair, _,
                                       {variable, _, b, B},
                                       {qualified_application, _, [source, test_code], d,
                                        [{keyword, _, _, 'T'}]}}],
                                     {variable, _, b, B}}]}}}, Defs)].

pattern_application_test_() ->
    Code = "def d -> X | Y
            def t -> (val f = d
                      val q = (fn f() -> d/X)
                      q(d/Y))",
    Defs = tag(Code),
    [?test(#{'t' := {def, _, 't',
                     {'let', _, {variable, _, f, F},
                                {qualified_symbol, _, [source, test_code], d},
                                {'let', _, {variable, _, q, Q},
                                           {'fun', _, [{clause, _, [{application, _, {variable, _, f, F}, []}],
                                                                   {keyword, _, [source, test_code, d], 'X'}}]},
                                           {application, _, {variable, _, q, Q},
                                                            [{keyword, _, [source, test_code, d], 'Y'}]}}}}},
           Defs)].

nested_module_import_conflict_test_() ->
    Code = "module test (export {boolean}
                         def boolean -> True | False)
            module test2 (import test
                          def t test/boolean -> test/boolean/True)",
    {ok, Modules} = parser:parse([{text, test_code, Code}], #{include_kind_libraries => false}),
    ModuleMap = maps:from_list([{module:path(M), M} || M <- Modules]),
    {module, _, _, _, _, Defs} = maps:get([test2], ModuleMap),
    [?test(#{'t' := {def, _, 't', {'fun', _,
                                   [{clause, _, [{qualified_symbol, _, [test], boolean}],
                                     {keyword, _, [test, boolean], 'True'}}]}}},
           Defs)].

beam_symbol_test_() ->
    Code = "import beam/rand
            def t -> rand/uniform",
    Defs = tag(Code),
    [?test(#{'t' := {def, _, 't',
                     {beam_symbol, _, [rand], uniform}}}, Defs)].
