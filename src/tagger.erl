-module(tagger).
-export([tag/1]).

tag(AST) -> 
    {TypeEnv, TaggedTypes} = tag_types(AST),
    {DefEnv, TaggedDefs} = tag_defs(TypeEnv, AST),
    {ok, {TypeEnv, DefEnv}, {TaggedTypes, TaggedDefs}}.

tag_types(AST) ->
    Types = lists:filter(fun(A) -> element(1,A) =:= type_def end, AST),
    TagType = fun(Path, {symbol, Line, T}) -> {type, Line, lists:reverse([T | Path])} end,
    InitEnv = maps:from_list([{Name, TagType([], {symbol, Line, Name})} || 
                              {type_def, Line, Name, _, _} <- Types]),
    map(InitEnv, Types, [], TagType).

tag_defs(TypeEnv, AST) ->
    Defs = lists:filter(fun(A) -> element(1,A) =:= def end, AST),
    TagDef = fun({def, Line, Name, Args, _}) -> {variable, Line, Name, {Name, length(Args)}} end,
    DefEnv = maps:from_list([{element(3, D), TagDef(D)} || D  <- Defs]),
    TagVal = fun(Path, {symbol, Line, S}) -> 
                     TypePath = lists:reverse([S | Path]),
                     case maps:get(S, TypeEnv, undefined) of
                         undefined -> {variable, Line, S, symbol:id(TypePath)};
                         Type -> Type
                     end 
             end,
    map(DefEnv, Defs, [], TagVal).

tag(Env, {def, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    {ArgsEnv, TaggedArgs} = map(#{}, Args, NewPath, TagFun),
    {BodyEnv, TaggedBody} = case Body of
                                _ when is_list(Body) -> map(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun);
                                _ -> tag(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun)
                            end,
    {BodyEnv, {def, Line, Name, TaggedArgs, TaggedBody}};

tag(Env, {lambda, Line, Clauses}, Path, TagFun) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, TagFun),
    {NewEnv, {lambda, Line, TaggedClauses}};

tag(Env, {clauses, Line, Clauses}, Path, TagFun) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, TagFun),
    {NewEnv, {clauses, Line, TaggedClauses}};

tag(Env, {clause, Line, Patterns, Expr}, Path, TagFun) ->
    {PatternEnv, TaggedPatterns} = map(#{}, Patterns, Path, TagFun),
    NewEnv = maps:merge(Env, PatternEnv),
    {BodyEnv, TaggedBody} = tag(NewEnv, Expr, Path, TagFun),
    {BodyEnv, {clause, Line, TaggedPatterns, TaggedBody}};

%% Notes on application:
%% Without knowing the type of `Name`, we can't know what the symbol 
%% resolves to. It might resolve to one of following:
%%  1. A function defined for a constraint. We can only know by finding the
%%     type of the noun. Example: `map` on list
%%  2. An accessor function for a type product (which strictly speaking 
%%     can be seen as a case of the above)
%%  3. A global or local non-type definition
%%
%%  In order to correctly assign the symbol we need to either:
%%   1. Be able to narrow down the domain to one possible option
%%   2. Generate a pattern match which would choose between options
tag(Env, {application, Line, Name, Args}, Path, TagFun) ->
    {NameEnv, TaggedName} = tag(Env, Name, Path, TagFun),
    {NewEnv, TaggedArgs} = map(NameEnv, Args, Path, TagFun),
    {NewEnv, {application, Line, TaggedName, TaggedArgs}};

tag(Env, {lookup, Line, Var, Elems}, Path, TagFun) ->
    {VarEnv, TaggedVar} = tag(Env, Var, Path, TagFun),
    io:format("Env: ~p~nVarEnv: ~p~n", [Env, VarEnv]),
    {NewEnv, TaggedElems} = map(VarEnv, Elems, Path, TagFun),
    {NewEnv, {lookup, Line, TaggedVar, TaggedElems}};

tag(Env, {val, Line, Pattern, Expr}, Path, TagFun) ->
    {PatternEnv, TaggedPattern} = tag(#{}, Pattern, Path, TagFun),
    {_, TaggedExpr} = tag(Env, Expr, Path, TagFun),
    NewEnv = maps:merge(Env, PatternEnv),
    {NewEnv, {val, Line, TaggedPattern, TaggedExpr}};

tag(Env, {match, Line, Expr, Clauses}, Path, TagFun) ->
    {ExprEnv, TaggedExpr} = tag(Env, Expr, Path, TagFun),
    {ClausesEnv, TaggedClauses} = map(ExprEnv, Clauses, Path, TagFun),
    {ClausesEnv, {match, Line, TaggedExpr, TaggedClauses}};

tag(Env, {tuple, Line, Expressions}, Path, TagFun) ->
    {EnvExpr, TaggedExpressions} = fold(Env, Expressions, Path, TagFun),
    {EnvExpr, {tuple, Line, TaggedExpressions}};

tag(Env, {dict, Line, Expressions}, Path, TagFun) ->
    {EnvExpr, TaggedExpressions} = map_dict(Env, Expressions, Path, TagFun),
    {EnvExpr, {dict, Line, TaggedExpressions}};

tag(Env, {type_def, Line, Name, Args, Body}, Path, TagFun) ->
    NewPath = [Name | Path],
    MakeVar = fun(L, S) -> {variable, L, S, symbol:id(lists:reverse([S | NewPath]))} end,
    TaggedArgs = [MakeVar(L, S) || {symbol, L, S} <- Args],
    ArgsEnv = maps:from_list([{S, Arg} || {_, _, S, _} = Arg <- TaggedArgs]),
    {BodyEnv, TaggedBody} = case Body of
                                _ when is_list(Body) -> map(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun);
                                _ -> tag(maps:merge(Env, ArgsEnv), Body, NewPath, TagFun)
                            end,
    {BodyEnv, {type_def, Line, Name, TaggedArgs, TaggedBody}};

tag(Env, {pair, Line, Key, Value}, Path, TagFun) ->
    {KeyEnv, TaggedKey} = tag(Env, Key, Path, TagFun),
    {ValueEnv, TaggedValue} = tag(KeyEnv, Value, Path, TagFun),
    {ValueEnv, {pair, Line, TaggedKey, TaggedValue}};

tag(Env, {qualified_symbol, _, _} = QS, _, _) -> {Env, QS};

tag(Env, {symbol, _, S} = Symbol, Path, TagFun) ->
    NewSymbol = maps:get(S, Env, TagFun(Path, Symbol)),
    NewEnv = maps:put(S, NewSymbol, Env),
    {NewEnv, NewSymbol}.

%% When we map a dictionary _in_ a pattern, we include the keys in the
%% environment because they get mapped to variables
map_dict(Env, Elements, Path, TagFun) when map_size(Env) == 0 -> % Env is empty for patterns
    Tag = fun({pair, Line, Key, Val}) ->
                  {KeyEnv, TaggedKey} = tag(Env, Key, Path, TagFun),
                  {ValEnv, TaggedVal} = tag(Env, Val, Path, TagFun),
                  {maps:merge(KeyEnv, ValEnv), {pair, Line, TaggedKey, TaggedVal}};
             ({symbol, _, _} = S) -> tag(Env, S, Path, TagFun) end,
    {EnvList, Tagged} = lists:unzip([Tag(E) || E <- Elements]),
    NewEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, EnvList),
    {NewEnv, Tagged};

%% When we map a dictionary that _isn't_ in a pattern, we deliberately don't
%% include the KeyEnv in the environment that is returned. This is because the
%% key of the pair in turn becomes an accessor function, but we want local
%% definitions and assignments to have precedence over type product accessor
%% functions.
map_dict(Env, Elements, Path, TagFun) when is_list(Elements) ->
    TagKey = fun(_, Symbol) -> {key, element(2, Symbol), symbol:name(Symbol)} end,
    Tag = fun({pair, Line, Key, Val}) ->
                  {_, TaggedKey} = tag(Env, Key, Path, TagKey),
                  {ValEnv, TaggedVal} = tag(Env, Val, Path, TagFun),
                  {ValEnv, {pair, Line, TaggedKey, TaggedVal}};
             ({symbol, _, _} = S) -> 
                  {_, TaggedSymbol} = tag(Env, S, Path, TagKey),
                  {Env, TaggedSymbol}
          end,
    {EnvList, Tagged} = lists:unzip([Tag(E) || E <- Elements]),
    NewEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, EnvList),
    {NewEnv, Tagged}.



map(Env, Elements, Path, TagFun) when is_list(Elements) ->
    {EnvList, Tagged} = lists:unzip([tag(Env, E, Path, TagFun) || E <- Elements]),
    NewEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, EnvList),
    {NewEnv, Tagged}.

fold(Env, Elements, Path, TagFun) when is_list(Elements) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                {NewEnv, TaggedExpr} = tag(EnvAcc, Expr, Path, TagFun),
                {maps:merge(EnvAcc, NewEnv), [TaggedExpr | ExprAcc]} 
        end,
    {NewEnv, TaggedElements} = lists:foldl(F, {Env, []}, Elements),
    {NewEnv, lists:reverse(TaggedElements)}.
