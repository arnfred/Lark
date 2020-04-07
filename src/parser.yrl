Terminals
    def type val 
    symbol type_symbol match
    integer float string
    open close
    apply comma newline assign
    bar implies.

Nonterminals
    statements statement definition expression type_expression
    assignment function newtype
    def_match expr_match pattern_clauses pattern_clause patterns pattern pattern_separator
    symbols expression_list
    literal application type_application
    separator.

Rootsymbol statements.


statements -> statement                     : ['$1'].
statements -> statement newline statements  : ['$1' | '$3'].

statement -> definition : '$1'.
statement -> expression : '$1'.

definition -> assignment   : '$1'.
definition -> function     : '$1'.
definition -> newtype      : '$1'.

assignment -> val symbol assign expression         : {val, line('$1'), unwrap('$2'), '$4'}.
function -> def symbols assign expression          : {def, line('$1'), '$2', '$4'}.
function -> def symbols def_match                  : {def, line('$1'), '$2', '$3'}.
newtype -> type type_symbol assign type_expression : {type, line('$1'), unwrap('$2'), '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

separator -> comma          : '$1'.
separator -> comma newline  : '$1'.
separator -> newline        : '$1'.

expression_list -> expression                           : ['$1'].
expression_list -> expression separator                 : ['$1'].
expression_list -> expression separator expression_list : ['$1' | '$3'].

expression -> literal       	    	: '$1'.
expression -> application    	   	: '$1'.
expression -> type_application		: '$1'.
expression -> expr_match	        : '$1'.
expression -> symbol 			: '$1'.

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

application -> expression apply symbol                              : {application, line('$1'), unwrap('$3'), ['$1']}.
application -> expression apply symbol open close                   : {application, line('$1'), unwrap('$3'), ['$1']}.
application -> expression apply symbol open expression_list close   : {application, line('$1'), unwrap('$3'), ['$1' | '$5' ]}.

type_application -> type_symbol                             : {type_application, line('$1'), unwrap('$1'), []}.
type_application -> type_symbol open close                  : {type_application, line('$1'), unwrap('$1'), []}.
type_application -> type_symbol open expression_list close  : {type_application, line('$1'), unwrap('$1'), '$3'}.

type_expression -> type_symbol bar type_expression  : ['$1' | '$3'].
type_expression -> type_symbol                      : ['$1'].

expr_match -> expression apply match open pattern_clauses close	        : {expr_match, line('$1'), '$1', '$5'}.
def_match -> pattern_separator pattern_clauses			        : {def_match, '$2'}.
pattern_clauses -> pattern_clause 		                        : ['$1'].
pattern_clauses -> pattern_clause pattern_separator pattern_clauses     : ['$1' | '$3'].
pattern_clause -> patterns implies expression 	                        : {pattern_clause, line('$2'), '$1', '$3'}.
patterns -> pattern					                : ['$1'].
patterns -> pattern patterns                                            : ['$1' | '$2'].
pattern -> expression 				                        : '$1'.
pattern_separator -> newline bar			                : '$2'.
pattern_separator -> bar			                        : '$1'.
pattern_separator -> separator			    	                : '$1'.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line.
