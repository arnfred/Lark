-module(pattern_gen).
-export([gen_pattern/2]).


% Pattern of shape: a
gen_pattern(_, {variable, _, _, _} = Term) -> {ok, cerl:c_var(symbol:tag(Term))};

% Pattern of shape: T
gen_pattern(_Scope, {keyword, _Ctx, _Path, _Val} = Term) ->
    Tag = symbol:tag(Term),
    {ok, cerl:c_atom(Tag)};

% Key like 'k' in '{k: a}'
gen_pattern(_, {keyword, _, _} = Term) -> skip;

% Pattern like '{a, k: b}'
gen_pattern(_, {dict, _, Elems}) -> {ok, cerl:c_map_pattern(Elems)};

% Pattern of variable or pair inside product
gen_pattern(_, {pair, _, {keyword, _, _} = K, Val}) ->
    Key = cerl:c_atom(symbol:tag(K)),
    {ok, cerl:c_map_pair_exact(Key, Val)};

% Pattern of shape: 'a: T' or 'T {a, B: T}'
% (the latter is a lookup, but get translated to a pair before reaching this
% state of the typegen
gen_pattern(_, {pair, _, Key, Val}) -> {ok, cerl:c_alias(Key, Val)};

% Pattern of shape: 'T: S'
gen_pattern(_, {tagged, _, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    {ok, cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Val])};

% Pattern of shape: '[1, 2]'
gen_pattern(_, {list, _, Elems}) -> {ok, cerl:make_list(Elems)};

% Pattern of shape: '#(1, 2)'
gen_pattern(_, {tuple, _, Elems}) -> {ok, cerl:c_tuple(Elems)};

% Pattern of shape: f(A)
gen_pattern(_Scope, {application, Ctx, _, _}) ->
    error:format({local_pattern_application}, {pattern_gen, Ctx});

% Pattern of shape: `"string"`, `'atom'` or `3.14`
gen_pattern(_, {value, _, Type, Val}) ->
    Bitstr = fun(V) -> cerl:c_bitstr(cerl:abstract(V), cerl:c_atom(undefined), cerl:c_atom(undefined), cerl:c_atom(utf8), cerl:abstract([unsigned, big])) end,
    case Type of
        string  -> {ok, cerl:c_binary([Bitstr(V) || V <- unicode:characters_to_list(Val, utf8)])};
        _       -> {ok, cerl:abstract(Val)}
    end.
