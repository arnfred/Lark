-module(tagged_gen).
-export([term/1]).

-include_lib("eunit/include/eunit.hrl").
-include("test/macros.hrl").

term({tagged, Ctx, Path, Val} = Term) -> tagged(Path, Ctx, term(symbol:name(Term), Val));
term(Term) -> term(symbol:name(Term), Term).

term(_, {link, _, _} = Term) -> Term;
term(_Tag, {keyword, _, _, _} = Term) -> Term;
term(Tag, Term) ->
    Ctx = element(2, Term),
    {def, Ctx, Tag, sub(Ctx, Term)}.


sub(Ctx, {list, ListCtx, Elems}) ->
    {Args, Patterns, Terms} = substitute_domains(Elems),
    {'fun', Ctx, [{clause, Ctx, patterns(Args, Patterns, Ctx), {list, ListCtx, Terms}}]};

sub(Ctx, {dict, DictCtx, Elems}) ->
    {Args, Patterns, Terms} = substitute_domains(Elems),
    {'fun', Ctx, [{clause, Ctx, patterns(Args, Patterns, Ctx), {dict, DictCtx, Terms}}]};

sub(Ctx, {sum, SumCtx, Elems}) ->
	Arg = {symbol, Ctx, variable, arg(1)},
    Defs = [{[Arg], [E], Arg} || E <- Elems],
    MakeClause = fun(Args, Patterns, Body) -> 
						 {clause, SumCtx, patterns(Args, Patterns, Ctx), Body}
                 end,
    Clauses = [MakeClause(Args, Patterns, Body) || {Args, Patterns, Body} <- Defs],
    {'fun', Ctx, Clauses};

sub(Ctx, Term) ->
    TermCtx = element(2, Term),
    Var = symbol:id(substituted),
    VarTerm = {symbol, TermCtx, variable, Var},
    {'fun', Ctx, [{clause, Ctx, patterns([VarTerm], [Term], Ctx), VarTerm}]}.

tagged(Path, TagCtx, {def, Ctx, Name, Term}) -> {def, Ctx, Name, tagged(Path, TagCtx, Term)};

tagged(Path, TagCtx, {'fun', Ctx, Clauses}) ->
    {'fun', Ctx, [tagged(Path, TagCtx, Clause) || Clause <- Clauses]};

tagged(Path, TagCtx, {clause, Ctx, Patterns, Body}) -> {clause, Ctx, Patterns, {tagged, TagCtx, Path, Body}};

tagged(Path, TagCtx, Term) -> {tagged, TagCtx, Path, Term}.

patterns(Args, Patterns, Ctx) -> [{pair, Ctx, A, sanitize_pattern(P)} || {A, P} <- lists:zip(Args, Patterns)].

substitute_domains(Elems) -> substitute_domains(Elems, 1, [], [], []).

substitute_domains([], _, Args, Patterns, Terms) ->
    {lists:reverse(Args),
     lists:reverse(Patterns),
     lists:reverse(Terms)};

substitute_domains([{pair, Ctx, Key, Val} | Rest], N, Args, Patterns, Terms) ->
    Arg = {symbol, symbol:ctx(Val), variable, arg(N)},
    ArgPair = {pair, Ctx, Key, Arg},
    substitute_domains(Rest, N+1, [Arg | Args], [Val | Patterns], [ArgPair | Terms]);

substitute_domains([Term | Rest], N, Args, Patterns, NewTerms) ->
    Arg = {symbol, symbol:ctx(Term), variable, arg(N)},
    substitute_domains(Rest, N+1, [Arg | Args], [Term | Patterns], [Arg | NewTerms]).

% When we substitute an expression for a variable, we use the expression as a
% pattern guard in the generated function. To give an example, the generated function for `S: Boolean` is:
%
% ```
% def S (substituted_1: Boolean) -> substituted_1
% ```
%
% The pattern might contain variables as part of the original term. In a
% pattern, an unbound variable is just substituted for the `any` domain.
% However, if the pattern contain a function application, the application
% arguments are treated as expressions (and not patterns). If they are not
% defined in other parts of the code, they will cause an error when tagging and
% transpiling the code.
%
% I've had a long think about how best to deal with the presence of these
% variables. The best option, I think is to replace them with a hard-coded
% `any` domain, which is what I've done below.
%
% Before this change, a pattern typechecked in `strict` mode would fail if the
% `any` domain was passed as an argument to a function with constaints on its
% arguments. As part if this commit, I've changed so patterns are almost at
% most typechecked with strictness mode `normal`, which in turn will prevent
% this from happening.
sanitize_pattern(Term) ->
    {ok, {_, NewTerm}} = ast:traverse_term(pattern, fun(_, _, _) -> ok end, fun sanitize_post/3, #{}, Term),
    NewTerm.

sanitize_post(expr, _, {symbol, Ctx, variable, _Name}) -> {ok, {symbol, Ctx, keyword, '_'}};
sanitize_post(expr, _, {symbol, Ctx, keyword, _Name}) -> {ok, {symbol, Ctx, keyword, '_'}};
sanitize_post(_, _, _) -> ok.


arg(N) -> list_to_atom("substituted_" ++ integer_to_list(N)).

-ifdef(TEST).

-define(setup(Term, Tests), {setup, fun() -> load(Term) end, fun clean/1, Tests}).

load(Term) ->
    case term(Term) of
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
              [{dict_pair, #{}, {key, #{}, k}, {symbol, #{}, variable, v}}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k := 'Boolean/True'}}, tagged:t('Boolean/True'))]
            end)}.

product_multiple_var_test_() ->
    {"A product with a single element should create one constructor which
      called with a value produces the product with that value",
     ?setup({tagged, #{args => []}, [t],
             {dict, #{},
              [{dict_pair, #{}, {key, #{}, k1}, {symbol, #{}, variable, v1}},
               {dict_pair, #{}, {key, #{}, k2}, {symbol, #{}, variable, v2}}]}},
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
               {dict_pair, #{}, {key, #{}, k2}, {symbol, #{}, variable, v}}]}},
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
                 {dict_pair, #{}, {key, #{}, k2}, {symbol, #{}, variable, v1}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k3}, {symbol, #{}, variable, v2}},
                 {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {symbol, #{}, variable, v1}},
                 {dict_pair, #{}, {key, #{}, k6}, {symbol, #{}, variable, v2}}]}]}},
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
                   {dict_pair, #{}, {key, #{}, k2}, {symbol, #{}, variable, v1}}]},
                 {dict, #{},
                  [{dict_pair, #{}, {key, #{}, k3}, {symbol, #{}, variable, v2}},
                   {dict_pair, #{}, {key, #{}, k4}, {value, #{}, float, 3.14}}]}]},
               {dict, #{},
                [{dict_pair, #{}, {key, #{}, k5}, {symbol, #{}, variable, v1}},
                 {dict_pair, #{}, {key, #{}, k6}, {symbol, #{}, variable, v2}}]}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, #{k1 := 32, k2 := 'T/A'}},
                           tagged:t(#{k1 => 32, k2 => 'T/A'})),
                     ?test({tagged, t, #{k4 := 3.14, k3 := 'S/B'}},
                           tagged:t(#{k3 => 'S/B', k4 => 3.14})),
                     ?test({tagged, t, #{k5 := 'S/A', k6 := 'T/B'}},
                           tagged:t(#{k5 => 'S/A', k6 => 'T/B'}))]
            end)}.

keyword_not_in_typesenv_test_() ->
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
              [{symbol, #{}, variable, a},
			   {symbol, #{}, variable, b}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, ['A', 'B']}, tagged:t('A', 'B'))]
            end)}.

single_val_sum_test_() ->
    {"Don't generate pattern matching for a sum with just one value",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{symbol, #{}, variable, a}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.

multi_var_sum_test_() ->
    {"A sum with multiple variables should generate clause statements that uses
      the same variable name as the expression",
     ?setup({tagged, #{args => []}, [t],
             {sum, #{},
              [{symbol, #{}, variable, a},
               {symbol, #{}, variable, b}]}},
            fun({ok, _}) ->
                    [?test({tagged, t, 'T/A'},
                           tagged:t('T/A'))]
            end)}.

non_tagged_term_test_() ->
    {"Substitute non-literals for variables in a term which isn't a tagged value",
     ?setup({symbol, #{}, variable, a},
            fun({ok, _}) ->
                    [?test(blah, tagged:a(blah))]
            end)}.




-endif.
