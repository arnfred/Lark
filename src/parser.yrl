Terminals
    def type val 
    symbol type_symbol match_keyword
    integer float string
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    bar implies.

Nonterminals
    definitions definition expression
    assignment function newtype
    match clause patterns pattern
    application callable
    symbols newlines elements
    collection tuple list dict
    literal element separator.

Rootsymbol definitions.


definitions -> definition                       : ['$1'].
definitions -> definition newlines definitions  : ['$1' | '$3'].

newlines -> newline          : '$1'.
newlines -> newline newlines : '$1'.

definition -> assignment   : '$1'.
definition -> function     : '$1'.
definition -> newtype      : '$1'.

assignment -> val symbol assign expression      : {val, line('$1'), unwrap('$2'), '$4'}.
function -> def symbols implies expression      : {def, line('$1'), '$2', {body, '$4'}}.
function -> def symbols separator elements      : {def, line('$1'), '$2', {clauses, '$4'}}.
newtype -> type type_symbol assign elements     : {type, line('$1'), unwrap('$2'), '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

separator -> comma          : '$1'.
separator -> comma newline  : '$1'.
separator -> newline        : '$1'.
separator -> newline bar    : '$2'.

expression -> literal       : '$1'.
expression -> application   : '$1'.
expression -> match         : '$1'.
expression -> symbol        : '$1'.
expression -> type_symbol   : '$1'.

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

callable -> symbol      : '$1'.
callable -> type_symbol : '$1'.

application -> callable collection                : {application, line('$1'), unwrap('$1'), build_args('$2')}.
application -> expression apply symbol            : {application, line('$1'), unwrap('$3'), ['$1']}.
application -> expression apply symbol collection : {application, line('$1'), unwrap('$3'), build_args('$1', '$4')}.

match -> expression apply match_keyword tuple   : {match, line('$1'), '$1', lists:nth(1,unwrap('$4'))}.
clause -> patterns implies expression 	        : {clause, line('$2'), '$1', '$3'}.
patterns -> pattern                             : ['$1'].
patterns -> pattern patterns                    : ['$1' | '$2'].
pattern -> expression                           : '$1'.

collection -> tuple : '$1'.
collection -> list  : '$1'.
collection -> dict  : '$1'.

tuple -> open close                                 : {tuple, line('$1'), []}.
tuple -> open elements close                        : parse_tuple(line('$1'), '$2').
tuple -> open separator elements close              : parse_tuple(line('$1'), '$3').
list -> square_open square_close                    : {list, line('$1'), []}.
list -> square_open elements square_close           : {list, line('$1'), '$2'}.
list -> square_open separator elements square_close : {list, line('$1'), '$3'}.
dict -> curly_open curly_close                      : {dict, line('$1'), []}.
dict -> curly_open elements curly_close             : {dict, line('$1'), '$2'}.
dict -> curly_open separator elements curly_close   : {dict, line('$1'), '$3'}.

elements -> element : ['$1'].
elements -> element separator elements : ['$1' | '$3'].

element -> expression : '$1'.
element -> clause : '$1'.
element -> definition : '$1'.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line.

% Tuples are ambigious. A touple can be:
% - A set of non-clause elements: A normal tuple
% - A set of clauses: A tuple with a single element 'clauses' element containing all clauses
% - A set of both clauses and elements: A tuple where each clause is wrapped in a 'clauses' element
parse_tuple(Line, Elements) ->
    {Clauses, Elems} = lists:splitwith(fun({clause, _, _, _}) -> true; (_) -> false end, Elements),
    WrapClause = fun({clause, ClauseLine, _, _} = Clause) -> {clauses, ClauseLine, [Clause]};
                    (Other) -> Other
                 end,
    case {Clauses, Elems} of
        {[], []}        -> {tuple, Line, []};
        {Clauses, []}   -> {tuple, Line, [{clauses, Line, Clauses}]};
        _               -> {tuple, Line, lists:map(WrapClause, Elements)}
    end.

build_args({tuple, _, Elems}) -> Elems;
build_args(Collection) -> [Collection].
build_args(Elem, {tuple, Line, Elems}) -> 
    case Elems of
        [{clause, _, _, _} | _] -> [ Elem, {lambda, Line, Elems} ];
        _                       -> [ Elem | Elems ]
    end;
build_args(Elem, Collection) -> [ Elem, Collection ].
