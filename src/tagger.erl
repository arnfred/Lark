-module(tagger).
-export([tag/1]).

tag(AST) -> 
    {TypeEnv, TaggedTypes} = tag_types(AST),
    {DefEnv, TaggedDefs} = tag_defs(TypeEnv, AST),
    {ok, {TypeEnv, DefEnv}, {TaggedTypes, TaggedDefs}}.

tag_symbol(Path, {symbol, Ctx, variable, S}) ->
    VarPath = lists:reverse([S | Path]),
    {variable, Ctx, S, symbol:id(VarPath)};
tag_symbol(Path, {symbol, Ctx, type, T}) ->
    {type, Ctx, lists:reverse([T | Path])}.

tag_types(AST) ->
    Types = lists:filter(fun(A) -> element(1,A) =:= type_def end, AST),
    InitEnv = maps:from_list([{Name, tag_symbol([], {symbol, Ctx, type, Name})} || 
                              {type_def, Ctx, Name, _, _} <- Types]),
    map(InitEnv, Types, [], false).

tag_defs(TypeEnv, AST) ->
    Defs = lists:filter(fun(A) -> element(1,A) =:= def end, AST),
    TagDef = fun({def, Ctx, Name, Args, _}) -> {variable, Ctx, Name, {Name, length(Args)}} end,
    DefEnv = maps:from_list([{element(3, D), TagDef(D)} || D  <- Defs]),
    Env = maps:merge(TypeEnv, DefEnv),
    io:format("Env: ~p~n", [Env]),
    map(Env, Defs, [], false).

tag(Env, {def, Ctx, Name, Args, Body}, Path, IsPattern) ->
    NewPath = [Name | Path],
    {ArgsEnv, TaggedArgs} = map(Env, Args, NewPath, IsPattern),
    {BodyEnv, TaggedBody} = case Body of
                                _ when is_list(Body) -> map(maps:merge(Env, ArgsEnv), Body, NewPath, IsPattern);
                                _ -> tag(maps:merge(Env, ArgsEnv), Body, NewPath, IsPattern)
                            end,
    {BodyEnv, {def, Ctx, Name, TaggedArgs, TaggedBody}};

tag(Env, {clauses, Ctx, Clauses}, Path, IsPattern) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, IsPattern),
    {NewEnv, {clauses, Ctx, TaggedClauses}};

tag(Env, {clause, Ctx, Patterns, Expr}, Path, _) ->
    {PatternEnv, TaggedPatterns} = map(Env, Patterns, Path, true),
    NewEnv = maps:merge(Env, PatternEnv),
    {BodyEnv, TaggedBody} = tag(NewEnv, Expr, Path, false),
    {BodyEnv, {clause, Ctx, TaggedPatterns, TaggedBody}};

tag(Env, {lambda, Ctx, Clauses}, Path, IsPattern) ->
    {NewEnv, TaggedClauses} = map(Env, Clauses, Path, IsPattern),
    {NewEnv, {lambda, Ctx, TaggedClauses}};

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
tag(Env, {application, Ctx, Name, Args}, Path, IsPattern) ->
    {NameEnv, TaggedName} = tag(Env, Name, Path, IsPattern),
    {NewEnv, TaggedArgs} = map(NameEnv, Args, Path, IsPattern),
    {NewEnv, {application, Ctx, TaggedName, TaggedArgs}};

tag(Env, {lookup, Ctx, Var, Elems}, Path, IsPattern) ->
    {VarEnv, TaggedVar} = tag(Env, Var, Path, IsPattern),
    {NewEnv, TaggedElems} = map(VarEnv, Elems, Path, IsPattern),
    {NewEnv, {lookup, Ctx, TaggedVar, TaggedElems}};

tag(Env, {val, Ctx, Pattern, Expr}, Path, IsPattern) ->
    {PatternEnv, TaggedPattern} = tag(Env, Pattern, Path, true),
    {_, TaggedExpr} = tag(Env, Expr, Path, IsPattern),
    NewEnv = maps:merge(Env, PatternEnv),
    {NewEnv, {val, Ctx, TaggedPattern, TaggedExpr}};

tag(Env, {match, Ctx, Expr, Clauses}, Path, IsPattern) ->
    {ExprEnv, TaggedExpr} = tag(Env, Expr, Path, IsPattern),
    {ClausesEnv, TaggedClauses} = map(ExprEnv, Clauses, Path, true),
    {ClausesEnv, {match, Ctx, TaggedExpr, TaggedClauses}};

tag(Env, {tuple, Ctx, Expressions}, Path, IsPattern) ->
    {EnvExpr, TaggedExpressions} = fold(Env, Expressions, Path, IsPattern),
    {EnvExpr, {tuple, Ctx, TaggedExpressions}};

tag(Env, {'let', Ctx, Pattern, Expr, Term}, Path, IsPattern) ->
    {PatternEnv, TaggedPattern} = tag(Env, Pattern, Path, true),
    {_, TaggedExpr} = tag(Env, Expr, Path, IsPattern),
    NewEnv = maps:merge(Env, PatternEnv),
    {TermEnv, TaggedTerm} = tag(NewEnv, Term, Path, IsPattern),
    {TermEnv, {'let', Ctx, TaggedPattern, TaggedExpr, TaggedTerm}};

tag(Env, {seq, Ctx, Expr1, Expr2}, Path, IsPattern) ->
    {Expr1Env, TaggedExpr1} = tag(Env, Expr1, Path, IsPattern),
    NewEnv = maps:merge(Env, Expr1Env),
    {Expr2Env, TaggedExpr2} = tag(NewEnv, Expr2, Path, IsPattern),
    {Expr2Env, {seq, Ctx, TaggedExpr1, TaggedExpr2}};

tag(Env, {dict, Ctx, Expressions}, Path, IsPattern) ->
    Tag = fun({pair, L, Key, Val}) ->
                  {KeyEnv, TaggedKey} = tag(Env, Key, Path, true),
                  {ValEnv, TaggedVal} = tag(Env, Val, Path, true),
                  {KeyEnv, ValEnv, {pair, L, TaggedKey, TaggedVal}};
             ({symbol, _, _, _} = S) -> 
                  {KeyEnv, TaggedSymbol} = tag(Env, S, Path, true),
                  {KeyEnv, #{}, TaggedSymbol}
          end,
    {KeyEnvList, ValEnvList, Tagged} = lists:unzip3([Tag(E) || E <- Expressions]),
    KeyEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, KeyEnvList),
    ValEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, ValEnvList),

    % For patterns we include the environment of the keys, for non-patterns we
    % don't. 
    %
    % This is because in a pattern a key in a dictionary is a variable that may
    % be used in the body of code following the expression.  For non-patterns,
    % a key is not a variable and it shouldn't clutter the name space.
    case IsPattern of
        true -> {maps:merge(KeyEnv, ValEnv), {dict, Ctx, Tagged}};
        false -> {ValEnv, {dict, Ctx, Tagged}}
    end;

tag(Env, {type_def, Ctx, Name, Args, Body}, Path, IsPattern) ->
    NewPath = [Name | Path],
    MakeVar = fun(L, S) -> {variable, L, S, symbol:id(lists:reverse([S | NewPath]))} end,
    TaggedArgs = [MakeVar(L, S) || {symbol, L, _, S} <- Args],
    ArgsEnv = maps:from_list([{S, Arg} || {_, _, S, _} = Arg <- TaggedArgs]),
    {BodyEnv, TaggedBody} = case Body of
                                _ when is_list(Body) -> map(maps:merge(Env, ArgsEnv), Body, NewPath, IsPattern);
                                _ -> tag(maps:merge(Env, ArgsEnv), Body, NewPath, IsPattern)
                            end,
    {BodyEnv, {type_def, Ctx, Name, TaggedArgs, TaggedBody}};

tag(Env, {pair, Ctx, Key, Value}, Path, IsPattern) ->
    {KeyEnv, TaggedKey} = tag(Env, Key, Path, IsPattern),
    {ValueEnv, TaggedValue} = tag(KeyEnv, Value, Path, IsPattern),
    {ValueEnv, {pair, Ctx, TaggedKey, TaggedValue}};

tag(Env, {qualified_type, Ctx, Symbols} = QT, _, _) ->
    {Module, Types} = lists:splitwith(fun({T, _}) -> T == variable end, Symbols),
    case Module of
        [] -> {Env, {type, Ctx, [T || {type, T} <- Types]}};
        _ -> {Env, QT}
    end;

tag(Env, {qualified_variable, Ctx, Symbols}, _, _) -> 
    {Env, {qualified_symbol, Ctx, [S || {variable, S} <- Symbols]}};

tag(Env, {key, _, _} = Key, _, _) -> {Env, Key};

tag(Env, {symbol, _, _, S} = Symbol, Path, _) ->
    NewSymbol = maps:get(S, Env, tag_symbol(Path, Symbol)),
    NewEnv = maps:put(S, NewSymbol, Env),
    {NewEnv, NewSymbol}.


map(Env, Elements, Path, IsPattern) when is_list(Elements) ->
    {EnvList, Tagged} = lists:unzip([tag(Env, E, Path, IsPattern) || E <- Elements]),
    NewEnv = lists:foldl(fun(M1, M2) -> maps:merge(M1, M2) end, #{}, EnvList),
    {NewEnv, Tagged}.

fold(Env, Elements, Path, IsPattern) when is_list(Elements) ->
    F = fun(Expr, {EnvAcc, ExprAcc}) -> 
                {NewEnv, TaggedExpr} = tag(EnvAcc, Expr, Path, IsPattern),
                {maps:merge(EnvAcc, NewEnv), [TaggedExpr | ExprAcc]} 
        end,
    {NewEnv, TaggedElements} = lists:foldl(F, {Env, []}, Elements),
    {NewEnv, lists:reverse(TaggedElements)}.
