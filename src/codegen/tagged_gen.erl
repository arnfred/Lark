-module(tagged_gen).
-export([form/2, term/2]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

form(TypesEnv, {tagged, _, _, Val} = Term) ->
    Tag = symbol:tag(Term),
    case domain:is_literal(TypesEnv, Val) of
        true    ->
            % For literals we generate a function with no arguments that return the domain
            case gen_expr(TypesEnv, Term) of
                {error, Errs}   -> {error, Errs};
                {ok, Body}      -> {ok, {cerl:c_fname(Tag, 0), cerl:c_fun([], Body)}}
            end;
        false   ->
            % For non literals, we replace the non-literal components with function arguments
            Substituted = term(TypesEnv, Term),
            case ast:traverse_term(expr, fun code_gen:pre_gen/3, fun code_gen:gen/3, TypesEnv, Substituted) of
                {error, Errs}       -> {error, Errs};
                {ok, {_, Form}}     -> {ok, {cerl:c_fname(Tag, arity(Substituted)), Form}}
            end
    end.

term(TypesEnv, {tagged, Ctx, Path, Val} = Term) ->
    Tag = symbol:tag(Term),
    case domain:is_literal(TypesEnv, Val) of
        % For literals we generate a function with no arguments that return the domain
        true    -> {type_def, Ctx, Tag, Term};
        % For non literals, we replace the non-literal components with function arguments
        false   -> {type_def, Ctx, Tag, tagged(TypesEnv, Ctx, Path, Val)}
    end.

tagged(TypesEnv, Ctx, Path, {list, ListCtx, Elems}) -> 
    {Args, Patterns, Terms} = substitute_domains(TypesEnv, Elems),
    {'fun', Ctx, [{clause, Ctx, patterns(Args, Patterns, Ctx), {tagged, Ctx, Path, {list, ListCtx, Terms}}}]};

tagged(TypesEnv, Ctx, Path, {dict, DictCtx, Elems}) -> 
    {Args, Patterns, Terms} = substitute_domains(TypesEnv, Elems),
    {'fun', Ctx, [{clause, Ctx, patterns(Args, Patterns, Ctx), {tagged, Ctx, Path, {dict, DictCtx, Terms}}}]};

tagged(_TypesEnv, Ctx, Path, {sum, SumCtx, Elems}) -> 
	Arg = {variable, Ctx, arg(1), arg(1)},
    Defs = [{[Arg], [E], Arg} || E <- Elems],
    MakeClause = fun(Args, Patterns, Body) -> 
						 {clause, SumCtx, patterns(Args, Patterns, Ctx), {tagged, Ctx, Path, Body}}
                 end,
    Clauses = [MakeClause(Args, Patterns, Body) || {Args, Patterns, Body} <- Defs],
    {'fun', Ctx, Clauses};

tagged(_, Ctx, Path, Term) ->
    TermCtx = element(2, Term),
    Var = symbol:id(substituted),
    VarTerm = {variable, TermCtx, Var, Var},
    {'fun', Ctx, [{clause, Ctx, patterns([VarTerm], [Term], Ctx), {tagged, Ctx, Path, VarTerm}}]}.

patterns(Args, Patterns, Ctx) -> [{pair, Ctx, A, P} || {A, P} <- lists:zip(Args, Patterns)].

substitute_domains(TypesEnv, Elems) -> substitute_domains(TypesEnv, Elems, 1, [], [], []).

substitute_domains(_, [], _, Args, Patterns, Terms) ->
    {lists:reverse(Args),
     lists:reverse(Patterns),
     lists:reverse(Terms)};

substitute_domains(TypesEnv, [{dict_pair, Ctx, Key, Val} = Pair | Rest], N, Args, Patterns, Terms) ->
    case domain:is_literal(TypesEnv, Pair) of
        true -> substitute_domains(TypesEnv, Rest, N, Args, Patterns, [Pair | Terms]);
        false ->
            ValCtx = element(2, Val),
            Arg = {variable, ValCtx, symbol:tag(Key), arg(N)},
            ArgPair = {dict_pair, Ctx, Key, Arg},
            substitute_domains(TypesEnv, Rest, N+1, [Arg | Args], [Val | Patterns], [ArgPair | Terms])
    end;

substitute_domains(TypesEnv, [Term | Rest], N, Args, Patterns, NewTerms) ->
    case domain:is_literal(TypesEnv, Term) of
        true -> substitute_domains(TypesEnv, Rest, N, Args, Patterns, [Term | NewTerms]);
        false ->
            Ctx = element(2, Term),
            Arg = {variable, Ctx, arg(N), arg(N)},
            substitute_domains(TypesEnv, Rest, N+1, [Arg | Args], [Term | Patterns], [Arg | NewTerms])
    end.

arg(N) -> list_to_atom("substituted_" ++ integer_to_list(N)).

arity({type_def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> length(Ps);
arity({type_def, _, _, _}) -> 0.

gen_expr(TypesEnv, Term) ->
    case ast:traverse_term(expr, fun code_gen:pre_gen/3, fun code_gen:gen/3, TypesEnv, Term) of
        {error, Errs}       -> {error, Errs};
        {ok, {_, Forms}}    -> {ok, Forms}
    end.


-ifdef(TEST).

-define(setup(Term, Tests), {setup, fun() -> load(Term, #{}) end, fun clean/1, Tests}).
-define(setup(Term, TypesEnv, Tests), {setup, fun() -> load(Term, TypesEnv) end, fun clean/1, Tests}).

load(Term, TypesEnv) ->
    case form(TypesEnv, Term) of
        {error, Errs}       	-> {error, Errs};
        {ok, {Export, _} = Def} ->
            ModuleForm = cerl:c_module(cerl:c_atom(tagged), [Export], [], [Def]),
            case compile:forms(ModuleForm, [report, verbose, from_core]) of
                error -> error:format({compilation_error}, {tagged_gen});
                {error, Err} -> error:format({compilation_error, Err}, {tagged_gen});
                {ok, Name, Bin} ->
                    code:load_binary(Name, "tagged.beam", Bin),
                    {ok, {Def, tagged}}
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

product_multiple_var_test_() ->
    {"A product with a single element should create one constructor which
      called with a value produces the product with that value",
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k1}, {variable, #{}, v1, v1}},
               {dict_pair, #{}, {key, #{}, k2}, {variable, #{}, v2, v2}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 'Boolean/True',
                                         k2 := 'Boolean/False'}}, tagged:t('Boolean/True', 'Boolean/False'))]
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
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
               {dict_pair, #{}, {key, #{}, k2}, {variable, #{}, v, v}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32,
                                         k2 := 'Boolean/True'}}, tagged:t('Boolean/True'))]
            end)}.

sum_of_products_test_() ->
    {"A tag that covers over a sum of products should generate a function of
     arity 1 which pattern matches its input against the sum members",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{dict, #{},
                [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
                 {dict_pair, #{}, {key, #{}, k2}, {variable, #{}, v1, v1}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k3}, {variable, #{}, v2, v2}},
                 {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {variable, #{}, v1, v1}},
                 {dict_pair, #{}, {key, #{}, k6}, {variable, #{}, v2, v2}}]}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32, k2 := 'T/A'}},
                           tagged:t(#{k1 => 32, k2 => 'T/A'})),
                     ?test({tagged, t, #{k4 := 3.14, k3 := 'S/B'}},
                           tagged:t(#{k3 => 'S/B', k4 => 3.14})),
                     ?test({tagged, t, #{k5 := 'S/A', k6 := 'T/B'}},
                           tagged:t(#{k5 => 'S/A', k6 => 'T/B'}))]
            end)}.

nested_sum_of_products_test_() ->
    {"A tag that covers over a nested sum of products should generate a set of
      function based on the arity of each product.  For products with the same
      arity, they should be differentiated by pattern matching on the input",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{sum, #{},
                [{dict, #{},
                  [{dict_pair, #{}, {key, #{}, k1}, {value, #{}, interger, 32}},
                   {dict_pair, #{}, {key, #{}, k2}, {variable, #{}, v1, v1}}]},
                 {dict, #{},
                  [{dict_pair, #{}, {key, #{}, k3}, {variable, #{}, v2, v2}},
                   {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {variable, #{}, v1, v1}},
                 {dict_pair, #{}, {key, #{}, k6}, {variable, #{}, v2, v2}}]}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32, k2 := 'T/A'}},
                           tagged:t(#{k1 => 32, k2 => 'T/A'})),
                     ?test({tagged, t, #{k4 := 3.14, k3 := 'S/B'}},
                           tagged:t(#{k3 => 'S/B', k4 => 3.14})),
                     ?test({tagged, t, #{k5 := 'S/A', k6 := 'T/B'}},
                           tagged:t(#{k5 => 'S/A', k6 => 'T/B'}))]
            end)}.

type_not_in_typesenv_test_() ->
    {"A type not in typeenv is treated as a literal and kept",
     ?setup({tagged, #{args => []}, [t], {type, #{}, 'T', ['T']}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T'}, tagged:t())]
            end)}.

% I can't properly test this because an env has changed to be a call to a
% domain function but I'm not generating domain functions for this test suite
%type_in_typeenv_test_() ->
%    {"A type in typeenv is treated as a non-literal and replaced",
%     ?setup({tagged, #{args => []}, [t], {type, #{}, 'T', ['T']}},
%            #{'T' => {sum, #{}, [{type, #{}, 'A', ['T', 'A']}, {type, #{}, 'B', ['T', 'B']}]}},
%            fun({ok, _}) ->
%                    [?test({tagged, t, 'T/A'}, tagged:t('T/A'))]
%            end)}.

list_test_() ->
    {"A list generates a constructor similar to a dict",
     ?setup({tagged, #{args => []}, [t],
             {list, #{},
              [{variable, #{}, a, a},
			   {variable, #{}, b, b}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, ['A', 'B']}, tagged:t('A', 'B'))]
            end)}.

single_val_sum_test_() ->
    {"Don't generate pattern matching for a sum with just one value",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{variable, #{}, a, a}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.

multi_var_sum_test_() ->
    {"A sum with multiple variables should generate clause statements that uses
      the same variable name as the expression",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{variable, #{}, a, a},
               {variable, #{}, b, b}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.





-endif.
