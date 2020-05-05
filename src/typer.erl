-module(typer).
-export([type/2]).

type(Types, Defs) -> from_AST(Types, Defs).

from_AST(Types, Defs) -> 
    %{ok, Env} = run_scan(#{}, #{}, Defs),
    %Env.
    {ok, #{}}.

run_scan(Types, OldEnv, Defs) ->
    Env = maps:from_list([{Name, scanner:scan(OldEnv, Types, Def)} || {def, _, Name, _, _} = Def <- Defs]), 
    case domain:diff(OldEnv, Env) of
        none -> {ok, Env};
        {some, _} -> run_scan(Types, Env, Defs)
    end.



%% Next steps:
%% - Tag variables so we can infer constraints on a variable in different
%%   positions of the function body
%% - Maybe implement pattern matching for product types?
%% - Generate function that takes a sum/product type and defines a domain for each
%% - Create function that can infer function constraints, possibly just for
%%   singletons to begin with?  
%% - Create function that can infer variable domains, possibly just for
%%   sum-types and literals to begin with?

%% Types of domains:
%% - Singleton Domain: (`True`, `False`, `"test"`, `42`). Each singleton is a
%%   domain with one element.
%% - Sum domain: `Domain_1 | ... | Domain_i | ... | Domain_n`. It can be
%%   represented as a set of domains
%% - Product domain: `(Domain_1, ..., Domain_i, ..., Domain_n)`. It can
%%   be represented as a tuple of Domains.
%% - Range domain: A range domain covers a Domain of ordered values and a range
%%   within that domain specified by an start and an end. Examples: `N > 0` or `2 to
%%   20000`.
%% - Predicate doman: (Only valid at compile time for literal domains). Example:
%%   `p(x) -> {True, False}`. A function that given a variable returns `True` or
%%   `False`. An example could be a regular expression.
%% - Any: The domain of all values
%% - None: The domain of no values
%% Parametrised Classes/Traits/Kinds can be modelled as product domains where
%% the parameters are the product members.

%% We need to be able to do the following operations on different domains:

%% - Intersection: When a variable `x` goes through pattern match, the domain
%%   of `x` is narrowed from what it was before the pattern match to the
%%   intersection of the domain that of the pattern and the domain of `x`.  This
%%   can be done simply by chaining the membership or subset of the two domains with a
%%   logical `and`. This would produce an Intersection Domain type.

%% - Subset: To see if a variable `x` satisfies a constraint `c`, we can check
%%   at compile time if the domain of `x` is a subset of the domain of `c`:
%%   - Singleton: subset is equality
%%   - Sum domain: There could be a case where a sum domain could consist of
%%     the two cases `N < 0` and `N >= 0`. In this case the entire range of
%%     numbers would be a subset of this sum-domain. However, if we test if the
%%     entire range of numbers is a subset of any of the two sum components, we
%%     would see that it was not. The only way I can see of sorting this out would
%%     be to merge sum-domains so that range sets defined on the same domain
%%     would appear as one. There might still be an issue with numeral literals
%%     as part of the sum-set.
%%   - Product domain: A given domain `D` is a subset if it is also a
%%     product-set and for all sub domain the equivalent sub-domain in `D` exists
%%     and is a subset.
%%   - Range domain: A given domain `D` is subset of range type domain. In this
%%     case `D` is a subset if:
%%     - `D` is a literal and in the range domain
%%     - `D` is a range domain and the start and end are within the start and
%%       end of the Domain
%%   - Intersection domain: Is subset of all member domains.
%%   - Any: Any domain is a subset of Any
%%   - None: No domain is a subset of None
%%   - Predicate domain: (Only valid for literal domains). Iterate through
%%     values of literal domain and check if predicate holds.

%% - Membership: We might want to defer checking a variable to run-time at
%%   which point we need to be able to translate a constraint-domain to a
%%   membership predicate:
%%   - Singleton: membership is equality
%%   - Sum domain: logical `or` of membership of any sub-domain
%%   - Product domain: logical `and` of membership of any equivalent sub-domain
%%   - Range domain: membership of range domain and between range bounds
%%   - Intersection domain: is member of all sub-domains.
%%   - Any: Any value is a member of Any
%%   - None: No values are a member of None

%% Constraints on Domains: Not all domains are created equal and we might be
%% able to provide particular guarantees for some domain types that we can't
%% provide across all domain types:
%%  - Literal Domain: Constrains the domain tree to only contain literals at
%%    the leafs. A literal domain must implement `values` iterating through all
%%    values in the domain.

%% Thoughts:
%% - Defs have *constraints*, variables have *domains*. 
%%   The function `def or b1 b2` accepts values from the domain {True, False} for b1 and b2
%%   Inside the _scope_ of the function body, both variables have the domain of {True, False}.
%% - The typer outputs: 
%%   - Constraints: for each function argument (both global and local) and set of constraints,
%%                  e.g. a set of functions from `Domain -> Boolean`.
%%   - Domains: for each variable in each scope, the domain i.e. the set of all values 
%%              it can possibly take.
%%   - Errors: For each point where a function is applied to values, but the domain 
%%             doesn't satisfy the constraint, an error is produced.
%% - I think gathering up this information could possibly be done in separate passes, and that 
%%   it might be nice to decouple the different pieces of work but it needs to start with 
%%   gathering constraints because they inform what the domains of the function arguments are.
%% - To be able to refer to the function argument constraints and the domains of the function 
%%   body variables, we'll need identifiers for the variables so we can look them up. I suggest
%%   we annotate the AST with variable identifiers in a pass before the typer.
%% - If a variable contains a literal known at compile time, we want to use this information 
%%   to build constraints from. For example the format string in `format` or the length of an 
%%   array. This means we need to generate and compile code that we can invoke at compile time.
%%   For this purpose, we might need to create a separate deriviative module for each file that
%%   is generated and compiled at compile time in order to type-check the rest.
%% - I wonder if most of the type-level code can just be compiled directly at compile-time. For
%%   example, a product-type would produce a set of accessor functions??? Maybe that wouldn't work

