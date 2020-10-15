Terminals
    def type val 
    type_symbol var_symbol
    value
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    module_keyword import_keyword
    pipe right_arrow slash colon.

Nonterminals
    literal
    all statements statement
    assignment function newtype module import
    implies
    pattern patterns pattern_application pattern_verb
    def_clauses clause lambda_clauses lambda
    type_clauses type_clause
    application arguments
    noun verb expression expressions
    collection list dict sequence
    dict_elements dict_element
    flat_sum_list sum_list sum_terms sum_elem sum_or_expression
    symbol symbols newlines elements element
    pair pair_key pair_val
    qualified_symbol qualified_type qualified_variable qualified_type_type
    separator secondary_separator clause_separator.

Rootsymbol all.

Left 900 slash.
Right 500 colon.
Left 100 newline.
Unary 100 expression.
Unary 200 pattern.
Unary 300 pattern_verb.
Nonassoc 300 pair.

all -> statements          : '$1'.
all -> newlines statements : '$2'.

newlines -> newline          : '$1'.
newlines -> newline newlines : '$1'.

literal -> value : '$1'.



% Statements
% ----------

statements -> statement                                  : ['$1'].
statements -> statement statements                       : ['$1' | '$2'].

statement -> statement newlines : '$1'.
statement -> function           : '$1'.
statement -> newtype            : '$1'.
statement -> module             : '$1'.
statement -> import             : '$1'.

implies -> right_arrow          : '$1'.
implies -> right_arrow newlines : '$1'.

assignment -> val expression assign expression          : {val, ctx('$1'), '$2', '$4'}.
function -> def symbols implies expression              : {def, ctx('$1'), name('$2'), args('$2'), '$4'}.
function -> def symbols clause_separator def_clauses    : {def, ctx('$1'), name('$2'), args('$2'), '$4'}.
newtype -> type symbols implies sum_or_expression       : {type_def, ctx('$1'), name('$2'), args('$2'), '$4'}.
newtype -> type symbols clause_separator type_clauses   : {type_def, ctx('$1'), name('$2'), args('$2'), '$4'}.



% Symbols
% -------

symbol -> type_symbol : make_symbol('$1').
symbol -> var_symbol : make_symbol('$1').

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

qualified_symbol -> qualified_type      : {qualified_type, ctx('$1'), '$1'}.
qualified_symbol -> qualified_variable  : {qualified_variable, ctx('$1'), '$1'}.

qualified_type -> var_symbol slash type_symbol                  : [make_symbol('$1'), make_symbol('$3')].
qualified_type -> var_symbol slash qualified_type_type          : [make_symbol('$1') | '$3'].
qualified_type -> qualified_variable slash type_symbol          : '$1' ++ [make_symbol('$3')].
qualified_type -> qualified_variable slash qualified_type_type  : '$1' ++ '$3'.
qualified_type -> qualified_type_type                           : '$1'.
qualified_type_type -> type_symbol slash type_symbol            : [make_symbol('$1'), make_symbol('$3')].
qualified_type_type -> type_symbol slash var_symbol             : [make_symbol('$1'), make_symbol('$3')].
qualified_type_type -> type_symbol slash dict                   : [make_symbol('$1'), '$3'].
qualified_type_type -> type_symbol slash qualified_type_type    : [make_symbol('$1') | '$3'].

qualified_variable -> var_symbol slash var_symbol               : [make_symbol('$1'), make_symbol('$3')].
qualified_variable -> var_symbol slash dict                     : [make_symbol('$1'), '$3'].
qualified_variable -> var_symbol slash qualified_variable       : [make_symbol('$1') | '$3'].



% Module
% ------

module -> module_keyword var_symbol dict                : {module, ctx('$1'), [make_symbol('$2')], unwrap('$3')}.
module -> module_keyword qualified_variable dict        : {module, ctx('$1'), '$2', unwrap('$3')}.

import -> import_keyword symbol                         : {import, ctx('$1'), ['$2']}.
import -> import_keyword qualified_symbol               : {import, ctx('$1'), unwrap('$2')}.



% Expressions
% -----------

expression -> application           : '$1'.  % f(a)
expression -> lambda                : '$1'.  % a -> a + 3
expression -> pair                  : '$1'.  % a: T                
expression -> collection            : '$1'.  % {a, b: T}
expression -> symbol                : '$1'.  % a
expression -> qualified_symbol      : '$1'.  % a/b/T
expression -> literal               : '$1'.  % 1 or "string" or 'atom'
expression -> sequence              : '$1'.  % (val a = 1, a + b)
expression -> sum_list              : '$1'.  % (A | B)

expressions -> expression                        : ['$1'].
expressions -> expression newlines               : ['$1'].
expressions -> expression separator expressions  : ['$1' | '$3'].



% Application
% -----------
% * f(a, b, c)
% * f(a -> True)
% * a.f
% * a.f(b, c)
% * a.f(b -> False)

% Things that can be called before the dot in an application
noun -> application             : '$1'.
noun -> collection              : '$1'.
noun -> sequence                : '$1'.
noun -> symbol                  : '$1'.
noun -> qualified_symbol        : '$1'.
noun -> literal                 : '$1'.

% Things that can be called after the dot in an application
verb -> symbol                  : '$1'.
verb -> qualified_symbol        : '$1'.
verb -> literal                 : '$1'.

application -> noun arguments               : {application, ctx('$1'), '$1', '$2'}.
application -> noun apply verb              : {application, ctx('$1'), '$3', ['$1']}.
application -> noun apply verb arguments    : {application, ctx('$1'), '$3', ['$1' | '$4']}.
application -> noun apply verb lambda       : {application, ctx('$1'), '$3', ['$1', '$4']}.

arguments -> open close                          : [].
arguments -> open expressions close              : '$2'.
arguments -> open newlines expressions close     : '$3'.



% Pair
% ----

pair -> pair_key colon pair_val : {pair, ctx('$1'), '$1', '$3'}.

pair_key -> application             : '$1'.
pair_key -> collection              : '$1'.
pair_key -> symbol                  : '$1'.
pair_key -> qualified_symbol        : '$1'.
pair_key -> literal                 : '$1'.

pair_val -> sum_list                : '$1'.
pair_val -> application             : '$1'.
pair_val -> collection              : '$1'.
pair_val -> symbol                  : '$1'.
pair_val -> qualified_symbol        : '$1'.
pair_val -> literal                 : '$1'.
pair_val -> open sum_elem close     : '$2'.



% Clauses
% -------

clause_separator -> newlines pipe : '$1'.

clause -> patterns implies expression               : {clause, ctx('$2'), '$1', '$3'}.
type_clause -> patterns implies sum_or_expression   : {clause, ctx('$2'), '$1', '$3'}.

def_clauses -> clause                              : ['$1'].
def_clauses -> clause newlines                     : ['$1'].
def_clauses -> clause clause_separator def_clauses : ['$1' | '$3'].

type_clauses -> type_clause                                 : ['$1'].
type_clauses -> type_clause newlines                        : ['$1'].
type_clauses -> type_clause clause_separator type_clauses   : ['$1' | '$3'].

lambda_clauses -> type_clause                           : ['$1'].
lambda_clauses -> type_clause newlines                  : ['$1'].
lambda_clauses -> type_clause separator lambda_clauses  : ['$1' | '$3'].

lambda -> open lambda_clauses close              : {lambda, ctx('$1'), '$2'}.
lambda -> open newlines lambda_clauses close     : {lambda, ctx('$1'), '$3'}.



% Patterns
% --------

patterns -> pattern                     : ['$1'].
patterns -> pattern patterns            : ['$1' | '$2'].

pattern -> pattern_application     : '$1'.   % A(T) or T.A(S)
pattern -> collection              : '$1'.   % {a, b: T}
pattern -> symbol                  : '$1'.   % a
pattern -> qualified_symbol        : '$1'.   % a/b/T
pattern -> literal                 : '$1'.   % 2
pattern -> open pair close         : '$2'.   % (a: T)
pattern -> sum_list                : '$1'.   % (A | B | C)

%pattern_application -> pattern_verb arguments                : {application, ctx('$1'), '$1', '$2'}.
pattern_application -> noun apply pattern_verb            : {application, ctx('$1'), '$3', ['$1']}.
pattern_application -> noun apply pattern_verb arguments  : {application, ctx('$1'), '$3', ['$1' | '$4']}.

pattern_verb -> qualified_type : {qualified_type, ctx('$1'), '$1'}.
pattern_verb -> type_symbol : make_symbol('$1').


% Collections
% -----------
% * {a, b: T}
% * [a, b, c]

collection -> list : '$1'.
collection -> dict : '$1'.

list -> square_open square_close                             : {list, ctx('$1'), []}.
list -> square_open expressions square_close                 : {list, ctx('$1'), '$2'}.
list -> square_open newlines expressions square_close        : {list, ctx('$1'), '$3'}.
dict -> curly_open curly_close                               : {dict, ctx('$1'), []}.
dict -> curly_open dict_elements curly_close                 : {dict, ctx('$1'), '$2'}.
dict -> curly_open newlines dict_elements curly_close        : {dict, ctx('$1'), '$3'}.

dict_element -> pair : '$1'.
dict_element -> noun : '$1'.

dict_elements -> dict_element                          : ['$1'].
dict_elements -> dict_element newlines                 : ['$1'].
dict_elements -> dict_element separator dict_elements  : ['$1' | '$3'].

separator -> comma          : '$1'.
separator -> comma newlines : '$1'.
separator -> newlines       : '$1'.



% Sequence
% --------

sequence -> open close                      : {tuple, ctx('$1'), []}.
sequence -> open elements close             : {tuple, ctx('$1'), '$2'}.
sequence -> open newlines elements close    : {tuple, ctx('$1'), '$3'}.

elements -> element : ['$1'].
elements -> element separator elements                 : ['$1' | '$3'].
elements -> element newlines         : ['$1'].

element -> expression           : '$1'.
element -> assignment           : '$1'.
element -> newtype              : '$1'.
element -> function             : '$1'.



% Sum or Expression
% -----------------

sum_or_expression -> flat_sum_list : unpack_sum('$1').
sum_or_expression -> sum_list : '$1'.
sum_or_expression -> open sum_elem close : '$2'.
flat_sum_list -> sum_elem : ['$1'].
flat_sum_list -> sum_elem pipe flat_sum_list : ['$1' | '$3'].

sum_list -> open sum_terms close                    : {sum, ctx('$1'), '$2'}.
sum_list -> open newlines sum_terms close           : {sum, ctx('$1'), '$3'}.

sum_terms -> sum_elem secondary_separator sum_elem          : ['$1', '$3'].
sum_terms -> sum_elem secondary_separator sum_elem newlines : ['$1', '$3'].
sum_terms -> sum_elem secondary_separator sum_terms         : ['$1' | '$3'].

sum_elem -> application             : '$1'.
sum_elem -> pair                    : '$1'.
sum_elem -> collection              : '$1'.
sum_elem -> symbol                  : '$1'.
sum_elem -> qualified_symbol        : '$1'.
sum_elem -> literal                 : '$1'.

secondary_separator -> newlines : '$1'.
secondary_separator -> pipe newlines : '$1'.
secondary_separator -> newlines pipe : '$1'.
secondary_separator -> pipe     : '$1'.



Erlang code.

unpack_tuple([T]) -> T;
unpack_tuple([T | _] = Terms) -> {tuple, ctx(T), Terms}.
unpack_sum([T]) -> T;
unpack_sum([T | _] = Terms) -> {sum, ctx(T), Terms}.

name([{symbol, _, _, S} | _]) -> S.
args([_ | Args]) -> Args.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

make_symbol({var_symbol, L, S}) -> {symbol, L, variable, S};
make_symbol({type_symbol, L, S}) -> {symbol, L, type, S}.

ctx({_, Ctx})         -> Ctx;
ctx({_, Ctx, _})      -> Ctx;
ctx({_, Ctx, _, _})   -> Ctx;
ctx([Head|_]) 		  -> ctx(Head).
