-module(diff).
-export([diff/3]).

diff(_, {recur, D}, {recur, D}) -> none;
diff(Path, {recur, OldF}, {recur, NewF}) -> 
    OldTag = gen_tag(OldF),
    NewTag = gen_tag(NewF),
    case lists:member(OldTag, Path) or lists:member(NewTag, Path) of
        true -> none;
        false -> diff([OldTag | [NewTag | Path]], OldF(), NewF())
    end;
diff(Path, {recur, OldF}, New) -> 
    Tag = gen_tag(OldF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff([Tag | Path], OldF(), New)
    end;
diff(Path, Old, {recur, NewF}) -> 
    Tag = gen_tag(NewF),
    case lists:member(Tag, Path) of
        true -> none;
        false -> diff([Tag | Path], Old, NewF())
    end;
diff(_, Same, Same) -> none;
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

diff(_, {f, Name, _}, {f, Name, _}) -> none;
diff(_, {f, _, _} = Old, {f, _, _} = New) -> #{old => Old,
                                               new => New};

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

diff(Path, Old, New) -> 
    case ordsets:is_set(Old) andalso ordsets:is_set(New) of
        true -> diff_set(Path, Old, New);
        false -> #{old => Old,
                   new => New}
    end.

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

gen_tag(F) -> 
    {name, Tag} = erlang:fun_info(F, name),
    Tag.
