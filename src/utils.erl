-module(utils).
-export([combinations/1, duplicates/2, group_by/2, group_by/3, unique/1, merge/1, pivot/1,
         domain_to_term/2, gen_tag/1, print_core/1, get_arity/1, get_arity/3, set/1, mapfun/2, mapfun/3,
         function/2, get_arities/2, get_min_arity/2, get_max_arity/2]).

-include_lib("eunit/include/eunit.hrl").

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
domain_to_term(A, Ctx) when is_float(A) -> {value, Ctx, float, A};
domain_to_term(A, Ctx) when is_integer(A) -> {value, Ctx, integer, A};
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

get_arity({def, _, _, {'fun', _, [{clause, _, Patterns, _} | _]}}) -> length(Patterns);
get_arity({def, _, _, _})                                          -> 0;
get_arity({keyword, _, _, _})                                      -> 0;
get_arity(Fun) when is_function(Fun) -> proplists:get_value(arity, erlang:fun_info(Fun)).

get_arity(Path, Name, ModuleMap) ->
    {module, _, Path, _, _, Defs} = maps:get(Path, ModuleMap),
    case maps:get(Name, Defs) of
        {link, _, LinkPath, LinkName}   -> get_arity(LinkPath, LinkName, ModuleMap);
        _                               -> get_arity(maps:get(Name, Defs))
    end.

get_arities(Module, Name) ->
    case erlang:module_loaded(Module) of
        false   -> [];
        true    -> proplists:get_all_values(Name, erlang:get_module_info(Module, exports))
    end.
get_min_arity(Module, Name) ->
    lists:foldl(fun min/2, none, get_arities(Module, Name)).
get_max_arity(Module, Name) ->
    lists:foldl(fun max/2, 0, get_arities(Module, Name)).

set(Elems) -> lists:foldl(fun ordsets:add_element/2, ordsets:new(), Elems).

mapfun(Mapper, Fun) -> 
    function(get_arity(Fun), fun(Args) -> Mapper(erlang:apply(Fun, Args)) end).

mapfun(Mapper, Fun1, Fun2) -> 
    function(get_arity(Fun1),
             fun(Args) -> Mapper(erlang:apply(Fun1, Args), erlang:apply(Fun2, Args)) end).

function(Arity, Body) ->
    case Arity of
        0 -> fun() -> Body([]) end;
        1 -> fun(A1) -> Body([A1]) end;
        2 -> fun(A1, A2) -> Body([A1, A2]) end;
        3 -> fun(A1, A2, A3) -> Body([A1, A2, A3]) end;
        4 -> fun(A1, A2, A3, A4) -> Body([A1, A2, A3, A4]) end;
        5 -> fun(A1, A2, A3, A4, A5) -> Body([A1, A2, A3, A4, A5]) end;
        6 -> fun(A1, A2, A3, A4, A5, A6) -> Body([A1, A2, A3, A4, A5, A6]) end;
        7 -> fun(A1, A2, A3, A4, A5, A6, A7) -> Body([A1, A2, A3, A4, A5, A6, A7]) end;
        8 -> fun(A1, A2, A3, A4, A5, A6, A7, A8) -> Body([A1, A2, A3, A4, A5, A6, A7, A8]) end;
        9 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9]) end;
        10 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]) end;
        11 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]) end;
        12 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]) end;
        13 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]) end;
        14 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]) end;
        15 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]) end;
        16 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]) end;
        17 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]) end;
        18 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]) end;
        19 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]) end;
        20 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]) end;
        21 -> fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) -> Body([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]) end
    end.


