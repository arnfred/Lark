-module(diff).
-export([diff/3]).

diff(_, {recur, D}, {recur, D}) -> none;
diff(Path, {recur, OldF}, {recur, NewF}) -> 
    OldTag = utils:gen_tag(OldF),
    NewTag = utils:gen_tag(NewF),
    case lists:member(OldTag, Path) or lists:member(NewTag, Path) of
        true -> none;
        false -> diff([OldTag | [NewTag | Path]], OldF(), NewF())
    end;
diff(Path, {recur, OldF}, New) -> 
    Tag = utils:gen_tag(OldF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff([Tag | Path], OldF(), New)
    end;
diff(Path, Old, {recur, NewF}) -> 
    Tag = utils:gen_tag(NewF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff([Tag | Path], Old, NewF())
    end;
diff(_, Same, Same) -> none;
diff(Path, {sum, Old}, {sum, New}) -> case diff_set(Path, Old, New) of
                                          none  -> none;
                                          Diff  -> {sum, Diff}
                                      end;
diff(Path, {Type, Old}, {Type, New}) -> 
    case diff(Path, Old, New) of
        none -> none;
        Diff -> {Type, Diff}
    end;

diff(Path, {tagged, Tag, Old}, {tagged, Tag, New}) ->
    case diff(Path, Old, New) of
        none -> none;
        Diff -> {tagged, Tag, Diff}
    end;

diff(_, OldF, NewF) when is_function(OldF), is_function(NewF) ->
    OldTag = utils:gen_tag(OldF),
    NewTag = utils:gen_tag(NewF),
    case OldTag =:= NewTag of
        true    -> none;
        false   -> #{old => OldF,
                     new => NewF}
    end;

diff(Path, Old, New) when is_map(Old), is_map(New) -> 
    Keys = ordsets:to_list(ordsets:from_list(maps:keys(Old) ++ maps:keys(New))),
    OnlyInOld = [Key || Key <- Keys, not maps:is_key(Key, New)],
    OnlyInNew = [Key || Key <- Keys, not maps:is_key(Key, Old)],
    Domains = [{Key, diff(Path, maps:get(Key, Old), maps:get(Key, New))} || 
               Key <- Keys, maps:is_key(Key, Old) andalso maps:is_key(Key, New)],
    DiffDomains = [{K, D} || {K, D} <- Domains, not(D =:= none)],
    case {OnlyInOld, OnlyInNew, DiffDomains} of
        {[], [], []} -> none;
        _ -> #{only_in_old => OnlyInOld, 
               only_in_new => OnlyInNew, 
               diff => DiffDomains}
    end;

diff(Path, L1, L2) when is_list(L1), is_list(L2) ->
    case length(L1) =:= length(L2) of
        true    ->
            Diffs = [diff(Path, E1, E2) || {E1, E2} <- lists:zip(L1, L2)],
            case lists:all(fun(E) -> E =:= none end, Diffs) of
                true    -> none;
                false   -> Diffs
            end;
        false   ->
            MinLength = min(length(L1), length(L2)),
            MaxLength = max(length(L1), length(L2)),
            L1Init = lists:sublist(L1, MinLength),
            L2Init = lists:sublist(L2, MinLength),
            L1Tail = lists:sublist(L1, MinLength + 1, MaxLength - MinLength),
            L2Tail = lists:sublist(L2, MinLength + 1, MaxLength - MinLength),
            #{only_in_old => L1Tail,
              only_in_new => L2Tail,
              diff => diff(Path, L1Init, L2Init)}
    end;

diff(_, Old, New) -> #{old => Old,
                       new => New}.

diff_set(Path, Old, New) ->
    InOld = ordsets:subtract(Old, New),
    InNew = ordsets:subtract(New, Old),
    Matched = [{O, N} || O <- InOld, N <- InNew, D <- [diff(Path, O, N)], D =:= none],
    {MatchedOld, MatchedNew} = lists:unzip(Matched),
    OnlyInOld = ordsets:to_list(ordsets:subtract(InOld, ordsets:from_list(MatchedOld))),
    OnlyInNew = ordsets:to_list(ordsets:subtract(InNew, ordsets:from_list(MatchedNew))),
    case length(OnlyInOld) =:= 0 andalso length(OnlyInNew) =:= 0 of
        true -> none;
        false -> #{only_in_old => OnlyInOld,
                   only_in_new => OnlyInNew}
    end.
