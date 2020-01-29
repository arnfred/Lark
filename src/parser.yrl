Terminals
    def type val 
    symbol type_symbol
    integer float string
    open close
    apply separator assign bar
    endl.

Nonterminals
    statements statement definition expression type_expression
    assignment function newtype
    symbols expression_list
    literal application type_application.

Rootsymbol statements.


statements -> statement             : ['$1'].
statements -> statement statements  : ['$1' | '$2'].

statement -> definition : '$1'.
statement -> expression : '$1'.

definition -> assignment   : '$1'.
definition -> function     : '$1'.
definition -> newtype      : '$1'.

assignment -> val symbol assign expression : {val, line('$1'), unwrap('$2'), '$4'}.

function -> def symbols assign expression : {def, line('$1'), '$2', '$4'}.

newtype -> type type_symbol assign type_expression : {type, line('$1'), unwrap('$2'), '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

expression_list -> expression                           : ['$1'].
expression_list -> expression separator expression_list : ['$1' | '$3'].

expression -> literal           : '$1'.
expression -> application       : '$1'.
expression -> type_application  : '$1'.

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

application -> expression apply symbol                              : {application, line('$1'), unwrap('$3'), ['$1']}.
application -> expression apply symbol open close                   : {application, line('$1'), unwrap('$3'), ['$1']}.
application -> expression apply symbol open expression_list close   : {application, line('$1'), unwrap('$3'), ['$1' | '$5' ]}.

type_application -> type_symbol                             : {type_application, line('$1'), unwrap('$1'), []}.
type_application -> type_symbol open close                  : {type_application, line('$1'), unwrap('$1'), []}.
type_application -> type_symbol open expression_list close  : {type_application, line('$1'), unwrap('$1'), '$3'}.

type_expression -> type_symbol                      : ['$1'].
type_expression -> type_symbol bar type_expression  : ['$1' | '$3'].

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line.
