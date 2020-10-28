-module(utils).
-export([combinations/1, duplicates/2, group_by/2, group_by/3, unique/1, merge/1, pivot/1, domain_to_term/2, gen_tag/1, print_core/1]).

% All combinations of list elements:
% [[a, b, c], [1, 2]] -> [[a, 1], [a, 2], [b, 1], [b, 2], [c, 1], [c, 2]]
combinations(L) -> 
    Rs = lists:foldl(fun(Es, Accs) -> [[E | Acc] || E <- Es, Acc <- Accs] end, [[]], L),
    [lists:reverse(R) || R <- Rs].

pivot([[]]) -> [[]];
pivot(L) ->
    Rs = lists:foldl(fun(Es, Accs) -> [[E | Acc] || {E, Acc} <- lists:zip(Es, Accs)] end, [[] || _ <- hd(L)], L),
    [lists:reverse(R) || R <- Rs].

% Pull out all duplicates from a list of elements using a key function
duplicates(Elements, GetKey) ->
    F = fun(Elem, {Duplicates, Seen}) -> 
                Key = GetKey(Elem),
                case maps:is_key(Key, Seen) of
                    true    -> {[{Elem, maps:get(Key, Seen)} | Duplicates], Seen};
                    false   -> {Duplicates, maps:put(Key, Elem, Seen)}
                end end,
    {Duplicates, _} = lists:foldl(F, {[], #{}}, Elements),
    Duplicates.

% Group items in list by key
% Example result: [(K1, [V1, V2]), (K2, [V3])]
group_by(KeyF, L) -> group_by(KeyF, fun(X) -> X end, L).
group_by(KeyF, ValF, L) -> 
	Dict = lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {KeyF(X), ValF(X)} || X <- L ]),
	dict:to_list(Dict).

% Keep unique items in list in an order-sensitive way
unique(L) -> 
    {Out, _} = lists:foldl(fun(Elem, {Out, Seen}) -> 
                                      case ordsets:is_element(Elem, Seen) of
                                          true -> {Out, Seen};
                                          false -> {[Elem | Out], ordsets:add_element(Elem, Seen)}
                                      end end, {[], ordsets:new()}, L),
    lists:reverse(Out).

% Merge a list of maps
merge(Maps) when is_list(Maps) ->
    lists:foldl(fun maps:merge/2, #{}, Maps).

% Convert between domains and their AST terms
domain_to_term({sum, Elems}, Ctx) -> 
    {sum, Ctx, [domain_to_term(E, Ctx) || E <- ordsets:to_list(Elems)]};
domain_to_term(A, Ctx) when is_atom(A) -> {type, Ctx, A, [A]};
domain_to_term(Elems, Ctx) when is_map(Elems) -> 
    {dict, Ctx, [{dict_pair, Ctx, K, domain_to_term(V, Ctx)} || {K, V} <- maps:to_list(Elems)]};
domain_to_term(Elems, Ctx) when is_list(Elems) -> 
    {list, Ctx, [domain_to_term(V, Ctx) || {_, V} <- Elems]};
domain_to_term({tagged, Tag, Domain}, Ctx) ->
    {tagged, Ctx, Tag, domain_to_term(Domain, Ctx)};
domain_to_term({recur, _F}, Ctx) ->
    {variable, Ctx, '_', '_'}.

gen_tag(F) -> 
    {name, Tag} = erlang:fun_info(F, name),
    Tag.

print_core(Name) ->
    {ok, FileContents} = file:read_file(Name),
    Core = unicode:characters_to_list(FileContents, utf8),
    {ok, Scanned, _} = core_scan:string(Core),
    io:format("~tp~n", [core_parse:parse(Scanned)]).

