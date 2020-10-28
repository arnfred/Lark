-module(ast_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/error.hrl").

pre_tagger(_, Scope, {symbol, Ctx, T, S}) -> 
    {ok, maps:get(S, Scope, {T, Ctx, S, symbol:id(S)})};
pre_tagger(_, _, Term) -> {ok, Term}.

post_tagger(pattern, _Env, {variable, _, Name, _} = Term) -> 
    {ok, Name, Term};
post_tagger(pattern, _Env, {type, _, Name, _} = Term) -> {ok, Name, Term};
post_tagger(_Type, _Env, _Term) -> ok.


pair_test_() ->
    Scope = #{test_symbol => {variable, #{}, test_symbol, symbsymb}},
    Input = {pair, #{},
             {symbol, #{}, variable, test_symbol},
             {symbol, #{}, type, 'T'}},
    Output = ast:traverse_term(expr, fun pre_tagger/3, fun post_tagger/3, Scope, Input),
    ?_assertMatch({ok,
                   {#{},
                    {pair, #{}, 
                     {variable, #{}, test_symbol, symbsymb}, 
                     {type, #{}, 'T', _}}}}, Output).

val_application_test_() ->
    Scope = #{'T' => {type, #{}, 'T', typtyp},
              test_symbol => {variable, #{}, test_symbol, symbsymb}},
    Input = {val, #{}, 
             {symbol, #{}, variable, val_symbol}, 
             {application, #{},
              {symbol, #{}, variable, test_symbol},
              [{symbol, #{}, type, 'T'}]}},
    Output = ast:traverse_term(expr, fun pre_tagger/3, fun post_tagger/3, Scope, Input),
    ?_assertMatch({ok,
                    {#{val_symbol := {variable, #{}, val_symbol, _V}},
                     {val, #{},
                      {variable, #{}, val_symbol, _V},
                      {application, #{}, 
                       {variable, #{}, test_symbol, _TS},
                       [{type, #{}, 'T', _T}]}}}}, Output).

let_assignment_lookup_test_() ->
    Scope = #{},
    Input = {'let', #{},
             {symbol, #{}, variable, key_symbol},
             {symbol, #{}, type, symbol},
             {'let', #{},
              {symbol, #{}, variable, val_symbol},
              {dict, #{}, [{pair, #{},
                             {symbol, #{}, variable, key_symbol},
                             {symbol, #{}, type, symbol}}]},
              {lookup, #{},
               {symbol, #{}, variable, val_symbol},
               [{symbol, #{}, variable, key_symbol}]}}},
    Output = ast:traverse_term(expr, fun pre_tagger/3, fun post_tagger/3, Scope, Input),
    ?_assertMatch({ok,
                   {#{val_symbol := {variable, #{}, val_symbol, _S},
                      key_symbol := {variable, #{}, key_symbol, _K}},
                    {'let', _, 
                     {variable, _, key_symbol, _K1},
                     {type, _, symbol, _T1},
                     {'let', _,
                      {variable, _, val_symbol, _S},
                      {dict, _,
                       [{pair, _,
                         {variable, _, key_symbol, _K1},
                         {type, _, symbol, _T2}}]},
                      {lookup, _,
                       {variable, _, val_symbol, _S},
                       [{variable, _, key_symbol, _K3}]}}}}}, Output).
