-module(domain_gen).
-export([gen/1]).

-include_lib("eunit/include/eunit.hrl").
gen({ast, _, Modules, _, _} = AST) ->
    error:map(collect_types(AST),
              fun({Types, Defs}) ->
                      ModuleName = symbol:id(domain),
                      % Why is Arity made from `Defs` and not `Types`?
                      % The reason is that we use the map of arities to encode
                      % the env, and we don't want to look up type constants in
                      % the env because it leads to infinite recursion.
                      Arities = maps:map(fun(_, D) -> arity(D) end, Defs),
                      Forms = lists:flatten([gen_def_form(ModuleName, Arities, Term) || Term <- maps:values(Types)]),
                      {ModuleExports, _} = lists:unzip(Forms),
                      RootModule = {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], Forms)},
                      TypeModules = gen_type_modules(ModuleName, Modules, Types),
                      DefModules = [gen_module(M, ModuleName, Arities) || M <- Modules],
                      TypesEnv = maps:from_list([gen_env_elem(Term, ModuleName) || Term <- maps:values(Defs)]),
                      Mods = [RootModule] ++ DefModules ++ TypeModules,
                      {TypesEnv, Mods}
              end).

% Here's the issue: I need to evaluate domains at runtime because type
% functions can be called with values only known at runtime. This is trivial if
% the domain code works with erlang core forms, but that's both cumbersome and
% very fragile to work with (I get vague run-time errors when I don't get the
% forms just right, which take ages to debug).

% Here's the approach. It's maybe a bit dumb, but I think it'll work: 
% when generating a function, we just generate an FNAME and a `cerl:apply` to a
% function in this module containing two parameters: The input args and the
% syntax tree of the function.
%
% * For an anonymous function, this is handled in the function we call with
%   apply like a normal function domain
% * For the domain function in a module I use a macro that takes the term and
%   computes the domain and makes calls to the domain module if necessary
% * For arguments_not_subsets errors, I create a similar function called
%   `typecheck` which works on the same principle.
%
%
gen_def_form(ModuleName, Arities, Term) ->
    Ctx = element(2, Term),
    Tag = symbol:tag(Term),
    Arity = arity(Term),
    Expr = case Term of
               {type_def, _, _, E}  -> E;
               {def, _, _, E}       -> E;
               _                    -> Term
           end,
    Args = [cerl:c_var(symbol:id('arg')) || _ <- lists:seq(1, Arity)],
    StackArg = cerl:c_var(symbol:id('stack')),
    LocalStack = [cerl:c_tuple([cerl:c_atom(Tag), cerl:abstract(Ctx), cerl:make_list(Args)])],
    Stack = cerl:c_case(cerl:c_call(cerl:c_atom(erlang), cerl:c_atom('length'), [StackArg]),
                        [cerl:c_clause([cerl:c_int(0)], cerl:make_list(LocalStack)),
                         cerl:c_clause([cerl:c_var('_')], StackArg)]),
    StrictnessArg = cerl:c_var(symbol:id('strictness')),
    ShortForm = cerl:c_apply(cerl:c_fname(Tag, Arity + 2), [cerl:make_list([]) | [cerl:c_atom(normal) | Args]]),
    NormalForm = cerl:c_apply(cerl:c_fname(Tag, Arity + 2), [cerl:make_list([]) | [StrictnessArg | Args]]),
    LongFormArgs = [StrictnessArg,
                    cerl:abstract(Arities),
                    Stack,
                    cerl:make_list(Args),
                    cerl:c_atom(ModuleName),
                    cerl:abstract(Expr)],
    LongForm = cerl:c_call(cerl:c_atom(typecheck),
                           cerl:c_atom(domain),
                           LongFormArgs),

    [{cerl:c_fname(Tag, Arity), cerl:c_fun(Args, ShortForm)},
     {cerl:c_fname(Tag, Arity + 1), cerl:c_fun([StrictnessArg] ++ Args, NormalForm)},
     {cerl:c_fname(Tag, Arity + 2), cerl:c_fun([StackArg, StrictnessArg] ++ Args, LongForm)}].

gen_env_elem({_, _, Name, _} = Term, ModuleName) ->
    case arity(Term) of
        0       -> Args = [cerl:c_atom(lenient)],
                   {Name, cerl:c_fun([], cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args))};
        Arity   -> Vars = [cerl:c_var(symbol:id('arg')) || _ <- lists:seq(1, Arity)],
                   Args = [cerl:c_atom(lenient) | Vars],
                   {Name, cerl:c_fun(Vars, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Name), Args))}
    end.

gen_call_form(Name, Tag, Arity, ModuleName) ->
    Vars = [cerl:c_var(symbol:id('arg')) || _ <- lists:seq(1, Arity)],
    {cerl:c_fname(Name, length(Vars)), 
     cerl:c_fun(Vars, cerl:c_call(cerl:c_atom(ModuleName), cerl:c_atom(Tag), Vars))}.

collect_types(AST) ->
    case ast:traverse(fun types_post/3, AST) of
        {error, Errs}      -> {error, Errs};
        {ok, {Terms, _}}   ->
            DefMap = maps:from_list([{symbol:tag(T), T} || {TermType, _, _, _} = T <- maps:values(Terms),
                                                           TermType =:= type_def orelse TermType =:= def]),
            F = fun({tagged, _, _, _} = T) -> tagged_gen:term(DefMap, T);
                   (T) -> T end,
            TaggedTerms = [F(T) || T <- maps:values(Terms)],
            Defs = maps:from_list([{symbol:tag(T), T} || {TermType, _, _, _} = T <- TaggedTerms,
                                                         TermType =:= type_def orelse TermType =:= def]),
            Types = maps:merge(maps:from_list([{symbol:tag(T), T} || T <- TaggedTerms]), Defs),
             {ok, {Types, Defs}}
    end.

% We use `Term` for the key of the env to make sure it's unique between type constants
% and type definitions. A mapping of type constants much earlier in the compiler would
% make this code superfluous, but here we are.
types_post(_, _, {type_def, _, _, _} = Term) ->
    {ok, Term, Term};
types_post(_, _, {def, _, _, _} = Term) ->
    {ok, Term, Term};
types_post(expr, _, {tagged, _, _, _} = Term) ->
    {ok, Term, Term};
types_post(_, _, {type, _, _, _} = Term) -> {ok, Term, Term};
types_post(_, _, _) -> ok.

arity({type_def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> length(Ps);
arity({type_def, _, _, _}) -> 0;
arity({def, _, _, {'fun', _, [{clause, _, Ps, _} | _]}}) -> length(Ps);
arity({def, _, _, _}) -> 0;
arity(_) -> 0.

% We generate a module with functions that call to the domain module
gen_module({module, _, Path, Exports}, ParentModule, Arities) ->
    CF = fun(Name, Key, N) -> 
                 gen_call_form(Name, symbol:tag(Key), maps:get(symbol:tag(Key), Arities, 0) + N, ParentModule)
         end,
    CallForms = lists:flatten([[CF(Name, Key, 0),
                                CF(Name, Key, 1),
                                CF(Name, Key, 2)] || {Name, {export, _, Key, _}} <- maps:to_list(Exports)]),
    {ModuleExports, _} = lists:unzip(CallForms),
    ModuleName = module:beam_name(Path ++ [domain]),
    {ModuleName, cerl:c_module(cerl:c_atom(ModuleName), ModuleExports, [], CallForms)}.

gen_type_modules(DomainMod, Modules, Defs) ->
    Types = type_members(Defs),
    [{Name, gen_type_module(DomainMod, Name, Children)} || {module, _, Path, Exports} <- Modules,
                                                           {Parent, Children} <- Types,
                                                           Name <- [module:beam_name(Path ++ [Parent])],
                                                           maps:is_key(Parent, Exports)].

gen_type_module(DomainMod, ModuleName, Children) ->
    Defs = lists:flatten([[gen_call_form(Name, Tag, Arity, DomainMod),
                           gen_call_form(Name, Tag, Arity+1, DomainMod),
                           gen_call_form(Name, Tag, Arity+2, DomainMod)] || {Name, Tag, Arity} <- Children]),
    IsTypeModule = {cerl:c_fname('type-module?', 0), cerl:c_fun([], cerl:c_atom(true))},
    {Exports, _} = lists:unzip([IsTypeModule | Defs]),
    cerl:c_module(cerl:c_atom(ModuleName), Exports, [], [IsTypeModule | Defs]).

type_members(Defs) ->
    TypePaths = [{Parent, {Child, symbol:tag(Term), arity(Term)}} || {Tag, Term} <- maps:to_list(Defs),
                                                                     [Parent, Child] <- [to_path(Tag)]],
    utils:group_by(fun({K, _}) -> K end, fun({_, V}) -> V end, TypePaths).

to_path(Atom) when is_atom(Atom) -> to_path(atom_to_list(Atom), []).
to_path([], Path) -> lists:reverse(Path);
to_path(List, Path) when is_list(List) ->
    {Head, Tail} = lists:splitwith(fun(C) -> not(C == $/) end, List),
    case Tail of
        []          -> to_path([], [list_to_atom(Head) | Path]);
        [_ | Rest]  -> to_path(Rest, [list_to_atom(Head) | Path])
    end.
