-module(ast_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

pre_tagger(Term) -> 
    TermType = element(1, Term),
    Context = element(2, Term),
    [TermType | maps:get(tagger, Context, [])].

post_tagger(_, _, TermF) ->
    Tag = fun(TermType, Ctx, S, T) -> {#{S => T}, {TermType, Ctx, S, T}} end,
    case TermF() of
        {error, Errs}                       -> {error, Errs};
        {ok, {Env, {type_symbol, C, S}}}    -> Tag(type, C, S, maps:get(S, Env, symbol:id(S)));
        {ok, {Env, {symbol, C, S}}}         -> Tag(var, C, S, maps:get(S, Env, symbol:id(S)));
        {ok, {Env, Term}}                   -> {Env, Term}
    end.

pair_test_() ->
    InputEnv = #{test_symbol => symbsymb,
                 'T' => typtyp},
    Input = {pair, #{},
             {symbol, #{}, test_symbol},
             {type_symbol, #{}, 'T'}},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, InputEnv, Input),
    ?_assertMatch({ok,
                   {#{},
                    {pair, #{}, {var, #{}, test_symbol, symbsymb}, {type, #{}, 'T', typtyp}}}}, Output).
pair_undefined_symbol_error_test_() ->
    InputEnv = #{},
    Input = {pair, #{},
             {symbol, #{}, test_symbol},
             {type_symbol, #{}, 'T'}},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, InputEnv, Input),
    ?_errorMatch({undefined_symbol_in_expression, 'test_symbol'},
                 {undefined_symbol_in_expression, 'T'},
                 Output). 

val_application_test_() ->
    InputEnv = #{'T' => typtyp,
                 test_symbol => symbsymb},
    Input = {val, #{}, 
             {symbol, #{}, val_symbol}, 
             {application, #{},
              {symbol, #{}, test_symbol},
              [{type_symbol, #{}, 'T'}]}},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, InputEnv, Input),
    ?_assertMatch({ok,
                    {#{val_symbol := _V},
                     {val, #{},
                      {var, #{}, val_symbol, _V},
                      {application, #{}, 
                       {var, #{}, test_symbol, _TS},
                       [{type, #{}, 'T', _T}]}}}}, Output).

symbol_already_defined_error_test_() ->
    InputEnv = #{val_symbol => valval},
    Input = {val, #{}, 
             {symbol, #{}, val_symbol}, 
             {symbol, #{}, val_symbol}},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, InputEnv, Input),
    ?_errorMatch({symbol_in_pattern_already_defined, val_symbol}, Output).

tuple_assignment_lookup_test_() ->
    InputEnv = #{type_symbol => typtyp},
    Input = {tuple, #{},
             [{val, #{}, 
               {symbol, #{}, key_symbol},
               {type_symbol, #{}, type_symbol}},
              {val, #{},
               {symbol, #{}, val_symbol},
               {dict, #{}, [{pair, #{},
                             {symbol, #{}, key_symbol},
                             {type_symbol, #{}, type_symbol}}]}},
              {lookup, #{},
               {symbol, #{}, val_symbol},
               [{symbol, #{}, key_symbol}]}]},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, InputEnv, Input),
    ?_assertMatch({ok,
                    {#{val_symbol := _S,
                       type_symbol := _T,
                       key_symbol := _K},
                     {tuple, _, 
                      [{val, _,
                        {var, _, key_symbol, _K1},
                        {type, _, type_symbol, _T1}},
                       {val, _,
                        {var, _, val_symbol, _S},
                        {dict, _,
                         [{pair, _,
                           {var, _, key_symbol, _K2},
                           {type, _, type_symbol, _T2}}]}},
                       {lookup, _,
                        {var, _, val_symbol, _S},
                        [{var, _, key_symbol, _K3}]}]}}}, Output).

type_def_test_() ->
    InputEnv = #{'T' => typtyp,
                 key => keykey},
    Input = {type_def, #{}, test_type,
             [{symbol, #{}, type_var}],
             {pair, #{},
              {type_symbol, #{}, 'T'},
              {dict, #{},
               [{pair, #{},
                 {symbol, #{}, key},
                 {symbol, #{}, type_var}}]}}},
    Output = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, InputEnv, Input),
    ?_assertMatch({ok,
                   {#{type_var := V, 'T' := T}, % Key is not present because it's not part of env
                    {type_def, _, test_type,
                     [{var, _, type_var, V}],
                     {pair, _,
                      {type, _, 'T', T},
                      {dict, _,
                       [{pair, _,
                         {var, _, key, keykey},
                         {var, _, type_var, V}}]}}}}}, Output).

unrecognized_term_error_test_() ->
    Input = {blip_blop, #{}, blap},
    Actual = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, #{}, Input),
    ?_errorMatch({unrecognized_term, blip_blop, expr}, Actual).

illegal_dict_element_error_test_() ->
    Input = {dict, #{},
             [{val, #{},
               {symbol, #{}, symb},
               {dict, #{}, []}}]},
    Actual = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, #{}, Input),
    ?_errorMatch({illegal_dict_element, val, expr}, Actual).

illegal_dict_symbol_error_test_() ->
    Input = {dict, #{},
             [{symbol, #{}, symb}]},
    Actual = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, expr, #{}, Input),
    ?_errorMatch({illegal_dict_element, symb, expr}, Actual).

lambda_clause_test_() ->
    Input = {def, #{}, test_fun,
             [{symbol, #{}, fun_var}],
             [{clause, #{},
               [{dict, #{}, [{symbol, #{}, dict_elem}]}],
               {lambda, #{},
                [{clause, #{},
                  [{symbol, #{}, lambda_var}],
                  {application, #{},
                   {symbol, #{}, lambda_var},
                   [{symbol, #{}, dict_elem}]}}]}}]},
    Actual = ast:traverse(tagger, fun pre_tagger/1, fun post_tagger/3, Input),
    ?_assertMatch({ok,
                   {#{fun_var := _F,
                      dict_elem := _D,
                      lambda_var := _L},
                    {def, _, test_fun,
                     [{var, _, fun_var, _F}],
                     [{clause, _,
                       [{dict, _, [{var, _, dict_elem, _D}]}],
                       {lambda, _,
                        [{clause, _,
                          [{var, _, lambda_var, _L}],
                          {application, _,
                           {var, _, lambda_var, _L},
                           [{var, _, dict_elem, _D}]}}]}}]}}}, Actual).
    
