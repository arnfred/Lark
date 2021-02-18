-module(tagger_test).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

tag(Code) ->
    case parser:parse([{text, Code}], #{import_kind_libraries => false}) of
        {error, Errs} -> {error, Errs};
        {ok, [{module, _, _, _, _, Defs} | _]} -> {ok, maps:values(Defs)}
    end.


identity_function_test_() ->
    Code = "def id a -> a",
    ?test({ok, [{def, _, id,
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A}],
                           {variable, _, a, A}}]}}]},
                 tag(Code)).

pattern_match1_test_() ->
    Code = 
        "def not\n"
        " | b -> b",
    ?test({ok, [{def, _, 'not',
                        {'fun', _,
                         [{clause, _, [{variable, _, b, B}], {variable, _, b, B}}]}}]},
                 tag(Code)).

tuple_test_() ->
    Code = "def not a -> (a, a)",
    ?test({ok, [{def, _, 'not',
                        {'fun', _,
                         [{clause, _,
                           [{variable, _, a, A}],
                           {seq, _, {variable, _, a, A}, {variable, _, a, A}}}]}}]},
                 tag(Code)).
    
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
                 tag(Code)).

dict_pair_test_() ->
    Code = "def f b -> {a: b}",
    ?test({ok, [{def, _, f,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, b, B}],
                    {dict, _,
                     [{pair, _,
                       {key, _, a},
                       {variable, _, b, B}}]}}]}}]}, tag(Code)).


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
                      [{key, _, a}]}}}]}}]}, tag(Code)).


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
                    [{qualified_symbol, _, [Root, 'Boolean'], 'True'}],
                    {qualified_symbol, _, [Root, 'Boolean'], 'False'}}]}}]},
          tag(Code)).

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
                 tag(Code)).

simple_product_type_test_() ->
    Code =
        "type Monkey -> Monkey: { food: Banana, plant: Trees }",
    ?test({ok, [{type_def, _, 'Monkey',
                        {tagged, _, ['Monkey'],
                         {dict, _,
                          [{pair,_,
                            {key,_,food},
                            {type,_,'Banana',['Monkey', 'Banana']}},
                           {pair,_,
                            {key,_,plant},
                            {type,_,'Trees',['Monkey', 'Trees']}}]}}}]}, tag(Code)).

complex_type_test_() ->
    Code =
        "type BooleanList -> (Cons: { value: (True | False)\n"
        "                             cons: BooleanList }\n"
        "                     Nil)",
    ?test({ok, [{type_def,_,'BooleanList',
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
                          {type,_,'Nil',['BooleanList','Nil']}]}}]}, tag(Code)).


product_key_not_propagated_test_() ->
    Code =
        "type Blip -> { blup: Blyp }\n"
        "def blap -> blup",
    ?testError({undefined_symbol, blup}, tag(Code)).

pattern_product_key_propagated_test_() ->
    Code = "def test\n"
           " | {b, c} -> b(c)",
    Tagged = tag(Code),
    ?test({ok, [{def, _, test,
                 {'fun', _,
                  [{clause,_,
                    [{dict,_,
                      [{variable,_,b, _B},
                       {variable,_,c, _C}]}],
                    {application,_,
                     {variable,_,b, _B},
                     [{variable,_,c, _C}]}}]}}]}, Tagged).

undefined_variable_test_() ->
    Code = "def test -> a",
    ?testError({undefined_symbol, a}, tag(Code)).

undefined_qualified_symbol_test_() ->
    Code = "def test -> T/T",
    ?testError({undefined_symbol, 'T/T'}, tag(Code)).

val_test_() ->
    Code = "def test a -> (val f = | b -> a,\n"
           "               f(a))",
    ?test({ok, [{def, _, test,
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, a, A1}],
                    {'let', _, {variable, _, f, F},
                     {'fun', _,
                      [{clause, _,
                        [{variable, _, b, _B}],
                        {variable, _, a, A1}}]},
                     {application, _, {variable, _, f, F},
                      [{variable, _, a, A1}]}}}]}}]}, tag(Code)).

local_import_conflict_test_() ->
    Code = "type T -> (A | B)
            import T/_",
    ?test({ok, [{type_def, _, 'T', {sum, _,
                                    [{qualified_symbol, _, [_,'T'], 'A'},
                                     {qualified_symbol, _, [_,'T'], 'B'}]}}]}, tag(Code)).

type_variable_test_() ->
    Code = "type F a -> a",
    ?test({ok, [{type_def, _, 'F',
                 {'fun', _,
                  [{clause, _,
                    [{variable, _, 'a', A}],
                    {variable, _, 'a', A}}]}}]}, tag(Code)).

tag_sub_module_test_() ->
    "I'm seeing some problems with type defs in sub modules and it's likely
     that the issue I'm seeing belongs in the `tagged_gen` module, but it's
     easier to design a test for it here",
    Code = "type List a -> (Nil | Cons: { head: a, tail: List(a) })",
    Parsed = parser:parse([{text, Code}], #{import_kind_libraries => false}),
    {ok, Modules} = Parsed,
    % Should result in `no_file_xxxxx` module and `no_file_xxxxx_List` module.
    % We're interested in the latter.
    [#{'Cons' := Cons}] = [Defs || {module, _, Path, _, _, Defs} <- Modules, lists:last(Path) =:= 'List'],
    ?test({type_def, _, 'Cons',
                {'fun', _,
                 [{clause, _,
                   [{pair, _, {variable, _, _, Sub1}, {variable, _, a, _}},
                    {pair, _, {variable, _, _, Sub2}, {application, _,
                                                       {qualified_symbol, _, _, 'List'},
                                                       [{type, _, any, _}]}}],
                   {tagged, _, ['List', 'Cons'],
                    {dict, _, [{pair, _, {key, _, head}, {variable, _, _, Sub1}},
                               {pair, _, {key, _, tail}, {variable, _, _, Sub2}}]}}}]}}, Cons).
