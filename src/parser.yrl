Terminals
    def type val 
    symbol type_symbol match_keyword
    integer float string
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    pipe right_arrow slash.

Nonterminals
    definitions definition expression
    assignment function newtype implies
    def_match match clause patterns pattern clauses guard_clauses clause_list clause_tuple
    application callable terminal_callable qualified_callable qualifier
    type_symbols symbols newlines elements
    collection tuple list dict
    literal element 
    separator primary_separator secondary_separator.

Rootsymbol definitions.


definitions -> definition                       : ['$1'].
definitions -> definition newlines definitions  : ['$1' | '$3'].

newlines -> newline  : '$1'.
newlines -> newline newlines : '$1'.

definition -> assignment   : '$1'.
definition -> function     : '$1'.
definition -> newtype      : '$1'.

implies -> right_arrow          : '$1'.
implies -> right_arrow newlines : '$1'.

assignment -> val symbol assign expression      : {val, line('$1'), unwrap('$2'), '$4'}.
function -> def symbols implies expression      : {def, line('$1'), '$2', '$4'}.
function -> def symbols newlines def_match      : {def, line('$1'), '$2', '$4'}.
newtype -> type type_symbols assign elements    : {type, line('$1'), unwrap('$2'), '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

type_symbols -> type_symbol     	   : ['$1'].
type_symbols -> type_symbol type_symbols   : ['$1' | '$2'].

callable -> terminal_callable       : '$1'.
callable -> qualified_callable      : {qualified_symbol, '$1'}.
callable -> collection              : '$1'.

terminal_callable -> symbol         : '$1'.
terminal_callable -> type_symbol    : '$1'.

qualified_callable -> terminal_callable qualifier terminal_callable  : [unwrap('$1'), unwrap('$3')].
qualified_callable -> terminal_callable qualifier qualified_callable : [unwrap('$1') | '$3'].

qualifier -> slash : '$1'.

expression -> literal       : '$1'.
expression -> application   : '$1'.
expression -> match         : '$1'.
expression -> callable      : '$1'.

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

application -> callable collection                  : {application, line('$1'), '$1', build_args('$2')}.
application -> expression apply callable            : {application, line('$1'), '$3', ['$1']}.
application -> expression apply callable collection : {application, line('$1'), '$3', build_args('$1', '$4')}.

match -> expression apply match_keyword clause_tuple    : {match, line('$1'), '$1', '$4'}.
clause -> patterns implies expression 	                : {clause, line('$2'), '$1', '$3'}.
patterns -> pattern                                     : ['$1'].
patterns -> pattern patterns                            : ['$1' | '$2'].
pattern -> expression                                   : '$1'.

clause_tuple -> open clauses close                      : '$2'.
clauses -> clause_list                                  : {clauses, line('$1'), '$1'}.
clause_list -> clause                                   : ['$1'].
clause_list -> clause secondary_separator clause_list   : ['$1' | '$3'].

def_match -> clauses.
def_match -> guard_clauses                          : {clauses, line('$1'), '$1'}.
guard_clauses -> pipe clause                        : ['$2'].
guard_clauses -> pipe clause newlines guard_clauses : ['$2' | '$4'].

collection -> tuple : '$1'.
collection -> list  : '$1'.
collection -> dict  : '$1'.

tuple -> open close                                 : {tuple, line('$1'), []}.
tuple -> open elements close                        : {tuple, line('$1'), '$2'}.
tuple -> open separator elements close              : {tuple, line('$1'), '$3'}.
list -> square_open square_close                    : {list, line('$1'), []}.
list -> square_open elements square_close           : {list, line('$1'), '$2'}.
list -> square_open separator elements square_close : {list, line('$1'), '$3'}.
dict -> curly_open curly_close                      : {dict, line('$1'), []}.
dict -> curly_open elements curly_close             : {dict, line('$1'), '$2'}.
dict -> curly_open separator elements curly_close   : {dict, line('$1'), '$3'}.

separator -> primary_separator.
separator -> secondary_separator.

primary_separator -> comma.
primary_separator -> comma newlines.
secondary_separator -> newlines.
secondary_separator -> pipe.

elements -> element                     : ['$1'].
elements -> element separator elements  : ['$1' | '$3'].

element -> expression : '$1'.
element -> definition : '$1'.
element -> clauses    : '$1'.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line;
line([Head|_]) 		-> line(Head).

build_args({tuple, _, Elems}) -> Elems;
build_args(Collection) -> [Collection].
build_args(Elem, {tuple, Line, Elems}) -> 
    case Elems of
        [{clause, _, _, _} | _] -> [ Elem, {lambda, Line, Elems} ];
        _                       -> [ Elem | Elems ]
    end;
build_args(Elem, Collection) -> [ Elem, Collection ].
