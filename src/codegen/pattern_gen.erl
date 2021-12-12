-module(pattern_gen).
-export([gen_pattern/3]).


% Pattern of shape: a
gen_pattern(pattern, _, {variable, _, _, _} = Term) -> {ok, [cerl:c_var(symbol:tag(Term))]};

% Pattern of shape: T
gen_pattern(pattern, _Scope, {keyword, _Ctx, _Path, _Val} = Term) ->
    Tag = symbol:tag(Term),
    {ok, [cerl:c_atom(Tag)]};

% Key like 'k' in '{k: a}'
gen_pattern(pattern, _, {key, _, _} = Term) -> {ok, [cerl:c_atom(symbol:tag(Term))]};

% Pattern like '{a, k: b}'
gen_pattern(pattern, _, {dict, _, ElemList}) ->
    {ok, [cerl:c_map_pattern(Elems) || 
          Elems <- utils:combinations(ElemList)]};

% Pattern of variable or pair inside product
%gen_pattern(pattern, _, {dict_pair, _, Keys, Vals}) ->
%    {ok, [cerl:c_map_pair_exact(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'a: T' or 'T {a, B: T}'
% (the latter is a lookup, but get translated to a pair before reaching this
% state of the typegen
gen_pattern(pattern, _, {pair, _, Keys, Vals}) ->
    {ok, [cerl:c_alias(K, V) || K <- Keys, V <- Vals]};

% Pattern of shape: 'T: S'
gen_pattern(pattern, _, {tagged, _, _, Vals} = Term) ->
    Tag = symbol:tag(Term),
    {ok, [cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), V]) || V <- Vals]};

% Pattern of shape: 'A | B'
gen_pattern(pattern, _, {sum, _, ElemList}) ->
    {ok, [E || Elems <- utils:combinations(ElemList), E <- Elems]};

% Pattern of shape: '[1, 2]'
gen_pattern(pattern, _, {list, _, ElemList}) ->
    Ls = [cerl:make_list(Elems) || Elems <- utils:combinations(ElemList)],
    Ts = [cerl:c_tuple(Elems) || Elems <- utils:combinations(ElemList)],
    {ok, Ls ++ Ts};

% Pattern of shape: f(A)
gen_pattern(pattern, _Scope, {application, Ctx, _, _}) ->
    error:format({local_pattern_application}, {pattern_gen, Ctx});

% Pattern of shape: `"string"`, `'atom'` or `3.14`
gen_pattern(pattern, _, {value, _, Type, Val}) -> 
    Bitstr = fun(V) -> cerl:c_bitstr(cerl:abstract(V), cerl:c_atom(undefined), cerl:c_atom(undefined), cerl:c_atom(utf8), cerl:abstract([unsigned, big])) end,
    case Type of
        string  -> {ok, [cerl:c_binary([Bitstr(V) || V <- unicode:characters_to_list(Val, utf8)])]};
        _       -> {ok, [cerl:abstract(Val)]}
    end.
