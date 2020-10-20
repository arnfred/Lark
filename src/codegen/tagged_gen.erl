-module(tagged_gen).
-export([gen/2]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

gen(TypesEnv, {tagged, _, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    case is_literal(TypesEnv, Val) of
        true    ->
            % For literals we generate a function with no arguments that return the domain
            case gen_expr(TypesEnv, Term) of
                {error, Errs}   -> {error, Errs};
                {ok, Body}      -> {ok, [{cerl:c_fname(Tag, 0), cerl:c_fun([], Body)}]}
            end;
        false   ->
            % For non literals, we replace the non-literal components with function arguments
            case gen_tagged_fun(TypesEnv, Val) of
                {error, Errs}           -> {error, Errs};
                {ok, ConstructorList}   ->
                    Constructors = lists:flatten(ConstructorList),
                    Tagged = fun(Body) -> cerl:c_tuple([cerl:c_atom(tagged), cerl:c_atom(Tag), Body]) end,
                    {ok, [{cerl:c_fname(Tag, length(Args)), cerl:c_fun(Args, Tagged(Body))} || 
                          {Args, Body} <- Constructors]}
            end
    end.

gen_tagged_fun(TypesEnv, {list, _, _} = Term) -> 
    case gen_tagged(TypesEnv, Term) of
        {error, Errs}   -> {error, Errs};
        {ok, FunList}    -> {ok, [{Args, Body} || {Args, _, Body} <- lists:flatten(FunList)]}
    end;

gen_tagged_fun(TypesEnv, {dict, _, _} = Term) -> 
    case gen_tagged(TypesEnv, Term) of
        {error, Errs}   -> {error, Errs};
        {ok, FunList}    -> {ok, [{Args, Body} || {Args, _, Body} <- lists:flatten(FunList)]}
    end;

gen_tagged_fun(TypesEnv, {sum, _, _} = Term) -> 
    case gen_tagged(TypesEnv, Term) of
        {error, Errs}   -> {error, Errs};
        {ok, FunList}    ->
            Funs = lists:flatten(FunList),
            FunsByArity = utils:group_by(fun({Args, _, _}) -> length(Args) end, Funs),
            MakeClause = fun(Patterns, Body) -> cerl:c_clause(Patterns, Body) end,
            MakeFun = fun([{Args, _, FirstBody} | _] = Fs) -> 
                              Clauses = [MakeClause(Patterns, Body) || {_, PatternList, Body} <- Fs,
                                                                       Patterns <- PatternList],
                              Body = case length(Args) of
                                         0 -> FirstBody;
                                         1 -> cerl:c_case(hd(Args), Clauses);
                                         N -> cerl:c_case(cerl:c_values(Args), Clauses)
                                     end,
                              {Args, Body}
                      end,
            {ok, [MakeFun(Fs) || {_Arity, Fs} <- FunsByArity]}
    end;

gen_tagged_fun(_, Term) ->
    Var = cerl:c_var(symbol:id(substituted)),
    {ok, [{[Var], Var}]}.


gen_tagged(TypesEnv, Term) -> gen_tagged(arg(1), TypesEnv, Term).

gen_tagged(_, TypesEnv, {dict, Ctx, Elems}) ->
    case substitute_domains(TypesEnv, Elems) of
        {error, Errs}                       -> {error, Errs};
        {ok, {Args, PatternList, Terms}}    ->
            Patterns = utils:combinations(PatternList),
            ArgForms = [cerl:c_var(A) || A <- lists:flatten(Args)],
            ArgTerm = {dict, Ctx, Terms},
            case gen_expr(TypesEnv, ArgTerm) of
                {error, Errs}   -> {error, Errs};
                {ok, Body}      -> {ok, [{ArgForms, Patterns, Body}]}
            end
    end;

gen_tagged(_, TypesEnv, {list, Ctx, Elems}) ->
    case substitute_domains(TypesEnv, Elems) of
        {error, Errs}                       -> {error, Errs};
        {ok, {Args, PatternList, Terms}}    ->
            Patterns = utils:combinations(PatternList),
            ArgForms = [cerl:c_var(A) || A <- lists:flatten(Args)],
            ArgTerm = {list, Ctx, Terms},
            case gen_expr(TypesEnv, ArgTerm) of
                {error, Errs}   -> {error, Errs};
                {ok, Body}      -> {ok, [{ArgForms, Patterns, Body}]}
            end
    end;

gen_tagged(Var, TypesEnv, {sum, _, Elems}) ->
    error:collect([gen_tagged(Var, TypesEnv, E) || E <- Elems]);

gen_tagged(Var, TypesEnv, Term) ->
    case is_literal(TypesEnv, Term) of
        true -> gen_tagged_literal(TypesEnv, Term);
        false ->
            case gen_pattern(TypesEnv, Term) of
                {error, Errs}       -> {error, Errs};
                {ok, Patterns}   ->
                    {ok, [{[cerl:c_var(Var)], [Patterns], cerl:c_var(Var)}]}
            end
    end.


gen_tagged_literal(TypesEnv, Literal) ->
    case gen_expr(TypesEnv, Literal) of
        {error, Errs}   -> {error, Errs};
        {ok, Body}      -> {ok, [{[], [], Body}]}
    end.


substitute_domains(TypesEnv, Elems) -> substitute_domains(TypesEnv, Elems, 1, [], [], []).

substitute_domains(_, [], _, Args, Patterns, Terms) ->
    {ok, {lists:reverse(Args),
          lists:reverse(Patterns),
          lists:reverse(Terms)}};

substitute_domains(TypesEnv, [{dict_pair, Ctx, Key, Val} = Pair | Rest], N, Args, Patterns, Terms) ->
    case is_literal(TypesEnv, Pair) of
        true -> substitute_domains(TypesEnv, Rest, N, Args, Patterns, [Pair | Terms]);
        false ->
            ValCtx = element(2, Val),
            Arg = list_to_atom("substituted_" ++ integer_to_list(N)),
            ArgPair = {dict_pair, Ctx, Key, {variable, ValCtx, symbol:tag(Key), Arg}},
            case gen_pattern(TypesEnv, Val) of
                {error, Errs}           -> {error, Errs};
                {ok, PatternList}  -> 
                    substitute_domains(TypesEnv, Rest, N+1, [Arg | Args], [PatternList | Patterns], [ArgPair | Terms])
            end
    end;

substitute_domains(TypesEnv, [Term | Rest], N, Args, Patterns, NewTerms) ->
    case is_literal(TypesEnv, Term) of
        true -> substitute_domains(TypesEnv, Rest, N, Args, Patterns, [Term | NewTerms]);
        false ->
            Ctx = element(2, Term),
            Arg = list_to_atom("substituted_" ++ integer_to_list(N)),
            Var = {variable, Ctx, Arg, Arg},
            case gen_pattern(TypesEnv, Term) of
                {error, Errs}           -> {error, Errs};
                {ok, PatternList}  -> 
                    substitute_domains(TypesEnv, Rest, N+1, [Arg | Args], [PatternList | Patterns], [Var | NewTerms])
            end
    end.

arg(N) -> list_to_atom("substituted_" ++ integer_to_list(N)).

gen_expr(TypesEnv, Term) ->
    case ast:traverse_term(expr, fun code_gen:pre_gen/3, fun code_gen:gen/3, TypesEnv, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, Forms}}    -> {ok, Forms}
    end.

gen_pattern(TypesEnv, PatternTerm) ->
    Pre = fun(_, _, {application, Ctx, _, _}) -> {variable, Ctx, '_', symbol:id('_')};
             (Type, Scope, Term) -> code_gen:pre_gen(Type, Scope, Term) end,
    case ast:traverse_term(pattern, Pre, fun code_gen:gen/3, TypesEnv, PatternTerm) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, Forms}}    -> {ok, Forms}
    end.
    
is_literal(_TypesEnv, {value, _, _, _})                         -> true;
is_literal(_TypesEnv, {variable, _, _, _})                      -> false;
is_literal(_TypesEnv, {qualified_variable, _, _, _})            -> false;
is_literal(_TypesEnv, {application, _, _, _})                   -> false;
is_literal(_TypesEnv, {recursive_type_application, _, _, _})    -> false;
is_literal(_TypesEnv, {recursive_type, _, _, _})                -> false;
is_literal(_TypesEnv, {sum, _, _})                              -> false;
is_literal(TypesEnv, {list, _, Elems})                  -> lists:all(fun(E) -> is_literal(TypesEnv, E) end, Elems);
is_literal(TypesEnv, {dict, _, Elems})                  -> lists:all(fun(E) -> is_literal(TypesEnv, E) end, Elems);
is_literal(TypesEnv, {tuple, _, Elems})                 -> lists:all(fun(E) -> is_literal(TypesEnv, E) end, Elems);
is_literal(TypesEnv, {pair, _, _, Val})                 -> is_literal(TypesEnv, Val);
is_literal(TypesEnv, {dict_pair, _, _, Val})            -> is_literal(TypesEnv, Val);
is_literal(TypesEnv, {tagged, _, _, Val})               -> is_literal(TypesEnv, Val);
is_literal(TypesEnv, {type, _, _, Path})                -> not(maps:is_key(Path, TypesEnv));
is_literal(TypesEnv, {qualified_type, Ctx, ModulePath, Name}) -> 
    ModuleName = module:beam_name(ModulePath),
    case erlang:function_exported(ModuleName, Name, 0) of
        false   -> false;
        true    -> Domain = erlang:apply(ModuleName, Name, []),
                   is_literal(TypesEnv, utils:domain_to_term(Domain, Ctx))
    end.
            



-ifdef(TEST).

-define(setup(Term, Tests), {setup, fun() -> load(Term, #{}) end, fun clean/1, Tests}).
-define(setup(Term, TypesEnv, Tests), {setup, fun() -> load(Term, TypesEnv) end, fun clean/1, Tests}).

load(Term, TypesEnv) ->
    case gen(TypesEnv, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, Defs}  ->
            {Exports, _} = lists:unzip(Defs),
            ModuleForm = cerl:c_module(cerl:c_atom(tagged), Exports, [], Defs),
            case compile:forms(ModuleForm, [report, verbose, from_core]) of
                error -> error:format({compilation_error}, {tagged_gen});
                {error, Err} -> error:format({compilation_error, Err}, {tagged_gen});
                {ok, Name, Bin} ->
                    code:load_binary(Name, "tagged.beam", Bin),
                    {ok, {Defs, tagged}}
            end
    end.

clean({error, _})   -> noop;
clean({ok, _})      -> true = code:soft_purge(tagged),
                       true = code:delete(tagged).

product_single_var_test_() ->
    {"A product with a single element should create one constructor which
      called with a value produces the product with that value",
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k}, {variable, #{}, v, v}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k := 'Boolean/True'}}, tagged:t('Boolean/True'))]
            end)}.

product_literal_test_() ->
    {"literals should not be replaced by a variable in the constructor",
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k}, {value, #{}, interger, 32}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k := 32}}, tagged:t())]
            end)}.
     
product_nested_literal_test_() ->
    {"nested literals should not be replaced by a variable in the constructor",
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k},
                {list, #{}, [{value, #{}, interger, 32}]}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k := [32]}}, tagged:t())]
            end)}.

product_mixed_var_and_literal_test_() ->
    {"For a product with mixed values and literals the constructor only
      replaces the values and only takes the number of args to do so",
     ?setup({tagged, #{}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
               {dict_pair, #{}, {key, #{}, k2}, {variable, #{}, v, v}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32,
                                         k2 := 'Boolean/True'}}, tagged:t('Boolean/True'))]
            end)}.

sum_of_products_test_() ->
    {"A tag that covers over a sum of products should generate a set of
      function based on the arity of each product.  For products with the same
      arity, they should be differentiated by pattern matching on the input",
     ?setup({tagged, #{}, [t],
             {sum, #{},
              [{dict, #{},
                [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
                 {dict_pair, #{}, {key, #{}, k2}, {type, #{}, 'T', ['T']}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k3}, {type, #{}, 'S', ['S']}},
                 {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {type, #{}, 'S', ['S']}},
                 {dict_pair, #{}, {key, #{}, k6}, {type, #{}, 'T', ['T']}}]}]}},
            #{['T'] => {sum, #{}, [{type, #{}, 'A', ['T', 'A']}, {type, #{}, 'B', ['T', 'B']}]},
              ['S'] => {sum, #{}, [{type, #{}, 'A', ['S', 'A']}, {type, #{}, 'B', ['S', 'B']}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32, k2 := 'T/A'}},
                           tagged:t('T/A')),
                     ?test({tagged, t, #{k4 := 3.14, k3 := 'S/B'}},
                           tagged:t('S/B')),
                     ?test({tagged, t, #{k5 := 'S/A', k6 := 'T/B'}},
                           tagged:t('S/A', 'T/B'))]
            end)}.

nested_sum_of_products_test_() ->
    {"A tag that covers over a nested sum of products should generate a set of
      function based on the arity of each product.  For products with the same
      arity, they should be differentiated by pattern matching on the input",
     ?setup({tagged, #{}, [t],
             {sum, #{},
              [{sum, #{},
                [{dict, #{},
                  [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
                   {dict_pair, #{}, {key, #{}, k2}, {type, #{}, 'T', ['T']}}]},
                 {dict, #{},
                  [{dict_pair, #{}, {key, #{}, k3}, {type, #{}, 'S', ['S']}},
                   {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {type, #{}, 'S', ['S']}},
                 {dict_pair, #{}, {key, #{}, k6}, {type, #{}, 'T', ['T']}}]}]}},
            #{['T'] => {sum, #{}, [{type, #{}, 'A', ['T', 'A']}, {type, #{}, 'B', ['T', 'B']}]},
              ['S'] => {sum, #{}, [{type, #{}, 'A', ['S', 'A']}, {type, #{}, 'B', ['S', 'B']}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32, k2 := 'T/A'}},
                           tagged:t('T/A')),
                     ?test({tagged, t, #{k4 := 3.14, k3 := 'S/B'}},
                           tagged:t('S/B')),
                     ?test({tagged, t, #{k5 := 'S/A', k6 := 'T/B'}},
                           tagged:t('S/A', 'T/B'))]
            end)}.

type_not_in_typesenv_test_() ->
    {"A type not in typeenv is treated as a literal and kept",
     ?setup({tagged, #{args => []}, [t], {type, #{}, 'T', ['T']}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T'}, tagged:t())]
            end)}.

type_in_typeenv_test_() ->
    {"A type in typeenv is treated as a non-literal and replaced",
     ?setup({tagged, #{args => []}, [t], {type, #{}, 'T', ['T']}},
            #{['T'] => {type, #{}, 'T', ['T']}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'S'}, tagged:t('S'))]
            end)}.

list_test_() ->
    {"A list generates a constructor similar to a dict",
     ?setup({tagged, #{args => []}, [t],
             {list, #{},
              [{type, #{}, 'T', ['T']},
               {tagged, #{args => []}, [s], {type, #{}, 'A', ['T', 'A']}}]}},
            #{['T'] => {type, #{}, 'T', ['T']},
              ['T', 'A'] => {type, #{}, 'T/A', ['T', 'A']}},
            fun({ok, _}) ->
                    [?test({tagged, t, ['S', 'P']}, tagged:t('S', 'P'))]
            end)}.

single_val_sum_test_() ->
    {"Don't generate pattern matching for a sum with just one value",
     ?setup({tagged, #{}, [t],
             {sum, #{},
              [{variable, #{}, a, a}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.

zero_val_sum_test_() ->
    {"Don't generate pattern matching for a sum with just one value",
     ?setup({tagged, #{}, [t],
             {sum, #{},
              [{value, #{}, atom, a}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'a'},
                           tagged:t())]
            end)}.

multi_var_sum_test_() ->
    {"A sum with multiple variables should generate clause statements that uses
      the same variable name as the expression",
     ?setup({tagged, #{}, [t],
             {sum, #{},
              [{variable, #{}, a, a},
               {variable, #{}, b, b}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.





-endif.
