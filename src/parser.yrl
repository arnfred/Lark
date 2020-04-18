Terminals
    def type val 
    symbol type_symbol match_keyword
    integer float string
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    pipe right_arrow slash colon.

Nonterminals
    definitions definition expression
    assignment function newtype implies
    def_match match clause patterns pattern clauses guard_clauses clause_list clause_tuple
    application callable terminal_callable qualified_callable qualifier
    type_symbols symbols newlines elements
    collection tuple list dict
    literal element 
    type_element type_elements type_pairs type_pair
    type_application type_sum type_product type_sum_block
    separator secondary_separator.

Rootsymbol definitions.


definitions -> definition                       : ['$1'].
definitions -> definition newlines definitions  : ['$1' | '$3'].

newlines -> newline          : '$1'.
newlines -> newline newlines : '$1'.

definition -> assignment        : '$1'.
definition -> function          : '$1'.
definition -> newtype           : '$1'.

implies -> right_arrow          : '$1'.
implies -> right_arrow newlines : '$1'.

assignment -> val symbol assign expression          : {val, line('$1'), unwrap('$2'), '$4'}.
function -> def symbols implies expression          : {def, line('$1'), '$2', '$4'}.
function -> def symbols newlines def_match          : {def, line('$1'), '$2', '$4'}.
newtype -> type type_symbols assign type_element    : {type, line('$1'), '$2', '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

callable -> terminal_callable       : '$1'.
callable -> qualified_callable      : {qualified_symbol, '$1'}.
callable -> open expression close   : '$2'.

terminal_callable -> symbol      : '$1'.
terminal_callable -> type_symbol : '$1'.

qualified_callable -> terminal_callable qualifier terminal_callable  : ['$1', '$3'].
qualified_callable -> terminal_callable qualifier qualified_callable : ['$1' | '$3'].

qualifier -> slash          : '$1'.

expression -> literal       : '$1'.
expression -> application   : '$1'.
expression -> match         : '$1'.
expression -> callable      : '$1'.
expression -> collection    : '$1'.

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
clause_tuple -> open newlines clauses close             : '$3'.
clauses -> clause_list                                  : {clauses, line('$1'), '$1'}.
clause_list -> clause                                   : ['$1'].
clause_list -> clause secondary_separator clause_list   : ['$1' | '$3'].

def_match -> guard_clauses                          : {clauses, line('$1'), '$1'}.
guard_clauses -> pipe clause                        : ['$2'].
guard_clauses -> pipe clause newlines guard_clauses : ['$2' | '$4'].

collection -> tuple : '$1'.
collection -> list  : '$1'.
collection -> dict  : '$1'.

tuple -> open close                                 : {tuple, line('$1'), []}.
tuple -> open elements close                        : {tuple, line('$1'), '$2'}.
tuple -> open newlines elements close               : {tuple, line('$1'), '$3'}.
list -> square_open square_close                    : {list, line('$1'), []}.
list -> square_open elements square_close           : {list, line('$1'), '$2'}.
list -> square_open newlines elements square_close  : {list, line('$1'), '$3'}.
dict -> curly_open curly_close                      : {dict, line('$1'), []}.
dict -> curly_open elements curly_close             : {dict, line('$1'), '$2'}.
dict -> curly_open newlines elements curly_close    : {dict, line('$1'), '$3'}.

separator -> comma          : '$1'.
separator -> comma newlines : '$1'.
separator -> newlines       : '$1'.

secondary_separator -> newlines : '$1'.
secondary_separator -> pipe     : '$1'.

elements -> element                     : ['$1'].
elements -> element newlines            : ['$1'].
elements -> element separator elements  : ['$1' | '$3'].

element -> expression   : '$1'.
element -> definition   : '$1'.
element -> clauses      : '$1'.

type_symbols -> type_symbol     	        : ['$1'].
type_symbols -> type_symbol type_symbols    : ['$1' | '$2'].

type_sum -> type_element pipe type_element      : ['$1', '$2'].
type_sum -> open type_sum_block close           : '$2'.
type_sum -> open newlines type_sum_block close  : '$2'.

type_sum_block -> type_element secondary_separator type_element             : ['$1', '$3'].
type_sum_block -> type_element secondary_separator type_element newlines    : ['$1', '$3'].
type_sum_block -> type_element secondary_separator type_sum_block           : ['$1' | '$3'].

type_product -> type_symbol open type_pairs close           : {product, line('$1'), '$1', '$3'}.
type_product -> type_symbol open newlines type_pairs close  : {product, line('$1'), '$1', '$3'}.

type_pairs -> type_pair                         : ['$1'].
type_pairs -> type_pair newlines                : ['$1'].
type_pairs -> type_pair separator type_pairs    : ['$1' | '$3'].
type_pair -> symbol colon type_element          : {type_pair, line('$1'), '$1', '$3'}.

type_application -> type_symbol open type_elements close            : {appliaction, '$1', '$3'}.
type_application -> type_symbol open newlines type_elements close   : {appliaction, '$1', '$3'}.

type_elements -> type_element                           : ['$1'].
type_elements -> type_element newlines                  : ['$1'].
type_elements -> type_element separator type_elements   : ['$1' | '$3'].

type_element -> type_sum            : {sum, line('$1'), '$1'}.
type_element -> type_product        : '$1'.
type_element -> type_symbol         : '$1'.
type_element -> type_application    : '$1'.



Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line;
line([Head|_]) 		-> line(Head).

build_args({tuple, _, Elems}) -> Elems;
build_args(Collection) -> [Collection].
build_args(Elem, {tuple, _, Elems}) -> [ Elem | Elems ];
build_args(Elem, Collection) -> [ Elem, Collection ].
