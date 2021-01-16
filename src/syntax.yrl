Terminals
    def type val macro
    type_symbol var_symbol
    rightbias_operator plus_operator mult_operator comp_operator eq_operator
    minus_operator div_operator caret_operator other_operator
    value
    open close square_open square_close curly_open curly_close
    space_open space_square_open space_curly_open
    apply comma newlines assign
    module_keyword import_keyword
    pipe right_arrow slash colon.

Nonterminals
    literal
    all statements statement
    assignment function newtype module import newmacro
    implies
    pattern patterns pattern_application pattern_verb braced_pattern
    clauses clause fun def_fun
    application arguments 
    infix rightbias_infix leftbias_infix
    noun verb expression expressions
    collection list dict sequence
    dict_elements dict_element
    sum_list sum_terms sum_elem
    symbol symbols operator elements element
    pair pair_key pair_val
    qualified_symbol
    separator secondary_separator clause_separator.

Rootsymbol all.

Left 900 slash.
Right 500 colon.

Right 770 caret_operator.
Right 760 mult_operator.
Right 750 div_operator.
Left 740 plus_operator.
Left 740 minus_operator.
Left 720 comp_operator.
Left 710 eq_operator.
Left 700 other_operator.
Right 699 rightbias_operator.

Left 745 leftbias_infix.
%Right 701 rightbias_infix.

Left 800 apply.

Nonassoc 200 pattern.
Nonassoc 200 braced_pattern.
Nonassoc 300 pattern_verb.
Nonassoc 100 sum_elem.

all -> statements          : '$1'.
all -> newlines statements : '$2'.

literal -> value : '$1'.



% Statements
% ----------

statements -> statement                                  : ['$1'].
statements -> statement statements                       : ['$1' | '$2'].

statement -> statement newlines : '$1'.
statement -> function           : '$1'.
statement -> newtype            : '$1'.
statement -> newmacro           : '$1'.
statement -> module             : '$1'.
statement -> import             : '$1'.

implies -> right_arrow          : '$1'.
implies -> right_arrow newlines : '$1'.

assignment -> val pattern assign expression             : {val, ctx('$1'), '$2', '$4'}.
function -> def symbol def_fun                          : {def, ctx('$1'), unwrap('$2'), '$3'}.
function -> def symbol implies expression               : {def, ctx('$1'), unwrap('$2'), '$4'}.
newtype -> type symbol def_fun                          : {type_def, ctx('$1'), unwrap('$2'), '$3'}.
function -> type symbol implies expression              : {type_def, ctx('$1'), unwrap('$2'), '$4'}.
newmacro -> macro symbol def_fun                        : {macro, ctx('$1'), unwrap('$2'), '$3'}.
function -> macro symbol implies expression             : {macro, ctx('$1'), unwrap('$2'), '$4'}.



% Symbols
% -------

symbol -> type_symbol           : make_symbol('$1').
symbol -> var_symbol            : make_symbol('$1').
symbol -> operator              : '$1'.
symbol -> rightbias_operator    : make_symbol('$1', operator).

operator -> caret_operator        : make_symbol('$1', operator).
operator -> mult_operator         : make_symbol('$1', operator).
operator -> div_operator          : make_symbol('$1', operator).
operator -> plus_operator         : make_symbol('$1', operator).
operator -> minus_operator        : make_symbol('$1', operator).
operator -> comp_operator         : make_symbol('$1', operator).
operator -> eq_operator           : make_symbol('$1', operator).
operator -> other_operator        : make_symbol('$1', operator).

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

qualified_symbol -> symbol slash symbol            : {qualified_symbol, ctx('$1'), ['$1', '$3']}.
qualified_symbol -> symbol slash dict              : {qualified_symbol, ctx('$1'), ['$1', '$3']}.
qualified_symbol -> symbol slash qualified_symbol  : {qualified_symbol, ctx('$1'), ['$1' | unwrap('$3')]}.



% Module
% ------

module -> module_keyword var_symbol dict                : {module, ctx('$1'), [make_symbol('$2')], unwrap('$3')}.
module -> module_keyword qualified_symbol dict          : {module, ctx('$1'), unwrap('$2'), unwrap('$3')}.

import -> import_keyword symbol                         : {import, ctx('$1'), ['$2']}.
import -> import_keyword qualified_symbol               : {import, ctx('$1'), unwrap('$2')}.



% Expressions
% -----------

expression -> open expression close         : '$2'. % ( ... )
expression -> space_open expression close   : '$2'. % ( ... )
expression -> application                   : '$1'.  % f(a)
expression -> fun                           : '$1'.  % | a -> a + 2
expression -> pair                          : '$1'.  % a: T                
expression -> collection                    : '$1'.  % {a, b: T}
expression -> symbol                        : '$1'.  % a
expression -> qualified_symbol              : '$1'.  % a/b/T
expression -> literal                       : '$1'.  % 1 or "string" or 'atom'
expression -> sequence                      : '$1'.  % (val a = 1, a + b)
expression -> sum_list                      : '$1'.  % (A | B)

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
noun -> open expression close       : '$2'.
noun -> space_open expression close : '$2'.
noun -> application                 : '$1'.
noun -> collection                  : '$1'.
noun -> sequence                    : '$1'.
noun -> symbol                      : '$1'.
noun -> qualified_symbol            : '$1'.
noun -> literal                     : '$1'.

% Things that can be called after the dot in an application
verb -> open expression close   : '$2'.
verb -> symbol                  : '$1'.
verb -> qualified_symbol        : '$1'.
verb -> literal                 : '$1'.

application -> verb arguments                       : {application, ctx('$1'), '$1', '$2'}.
application -> noun apply verb                      : {application, ctx('$1'), '$3', ['$1']}.
application -> noun apply verb arguments            : {application, ctx('$1'), '$3', ['$1' | '$4']}.
application -> infix                                : '$1'.

arguments -> open close                             : [].
arguments -> open expressions close                 : '$2'.
arguments -> open newlines expressions close        : '$3'.

infix -> leftbias_infix                             : '$1'.
infix -> rightbias_infix                            : '$1'.
leftbias_infix -> noun operator noun                : {application, ctx('$2'), '$2', ['$1', '$3']}.
leftbias_infix -> infix operator noun               : {application, ctx('$2'), '$2', ['$1', '$3']}.
rightbias_infix -> noun rightbias_operator noun     : {application, ctx('$2'), make_symbol('$2', operator), ['$1', '$3']}.
rightbias_infix -> noun rightbias_operator infix    : {application, ctx('$2'), make_symbol('$2', operator), ['$1', '$3']}.




% Pair
% ----

pair -> pair_key colon pair_val : {pair, ctx('$1'), '$1', '$3'}.

pair_key -> application                 : '$1'.
pair_key -> collection                  : '$1'.
pair_key -> symbol                      : '$1'.
pair_key -> qualified_symbol            : '$1'.
pair_key -> literal                     : '$1'.

pair_val -> sum_list                    : '$1'.
pair_val -> application                 : '$1'.
pair_val -> collection                  : '$1'.
pair_val -> symbol                      : '$1'.
pair_val -> qualified_symbol            : '$1'.
pair_val -> literal                     : '$1'.
pair_val -> open expression close       : '$2'.
pair_val -> space_open expression close : '$2'.



% Clauses and Functions
% -------

clause_separator -> newlines pipe           : '$2'.
clause -> patterns implies expression       : {clause, ctx('$2'), '$1', '$3'}.
clauses -> clause                           : ['$1'].
clauses -> clause newlines                  : ['$1'].
clauses -> clause clause_separator clauses  : ['$1' | '$3'].
def_fun -> clauses                          : {'fun', ctx('$1'), '$1'}.
def_fun -> clause_separator clauses         : {'fun', ctx('$2'), '$2'}.
fun -> pipe clauses                         : {'fun', ctx('$1'), '$2'}.



% Patterns
% --------

patterns -> pattern                         : ['$1'].
patterns -> pattern patterns                : ['$1' | '$2'].

pattern -> symbol                           : '$1'.   % a
pattern -> pattern_application              : '$1'.   % T.A(S)
pattern -> space_open braced_pattern close  : '$2'.   % (...)
pattern -> collection                       : '$1'.   % {a, b: T}
pattern -> qualified_symbol                 : '$1'.   % a/b/T
pattern -> literal                          : '$1'.   % 2
pattern -> sum_list                         : '$1'.   % (A | B | C)

braced_pattern -> pair          : '$1'.
braced_pattern -> application   : '$1'.
braced_pattern -> pattern       : '$1'.

pattern_application -> pattern_verb arguments             : {application, ctx('$1'), '$1', '$2'}.
pattern_application -> noun apply pattern_verb            : {application, ctx('$1'), '$3', ['$1']}.
pattern_application -> noun apply pattern_verb arguments  : {application, ctx('$1'), '$3', ['$1' | '$4']}.

pattern_verb -> qualified_symbol : '$1'.
pattern_verb -> symbol : '$1'.



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

list -> space_square_open square_close                             : {list, ctx('$1'), []}.
list -> space_square_open expressions square_close                 : {list, ctx('$1'), '$2'}.
list -> space_square_open newlines expressions square_close        : {list, ctx('$1'), '$3'}.
dict -> space_curly_open curly_close                               : {dict, ctx('$1'), []}.
dict -> space_curly_open dict_elements curly_close                 : {dict, ctx('$1'), '$2'}.
dict -> space_curly_open newlines dict_elements curly_close        : {dict, ctx('$1'), '$3'}.

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

sequence -> space_open close                      : {tuple, ctx('$1'), []}.
sequence -> space_open elements close             : {tuple, ctx('$1'), '$2'}.
sequence -> space_open newlines elements close    : {tuple, ctx('$1'), '$3'}.

elements -> element separator element   : ['$1', '$3'].
elements -> element separator elements  : ['$1' | '$3'].
elements -> element newlines            : ['$1'].

element -> expression           : '$1'.
element -> assignment           : '$1'.
element -> newtype              : '$1'.
element -> function             : '$1'.



% Sum or Expression
% -----------------

sum_list -> space_open sum_terms close                      : {sum, ctx('$1'), '$2'}.
sum_list -> open sum_terms close                            : {sum, ctx('$1'), '$2'}.
sum_list -> space_open newlines sum_terms close             : {sum, ctx('$1'), '$3'}.
sum_list -> open newlines sum_terms close                   : {sum, ctx('$1'), '$3'}.

sum_terms -> sum_elem secondary_separator sum_elem          : ['$1', '$3'].
sum_terms -> sum_elem secondary_separator sum_elem newlines : ['$1', '$3'].
sum_terms -> sum_elem secondary_separator sum_terms         : ['$1' | '$3'].

sum_elem -> open expression close       : '$2'.
sum_elem -> space_open expression close : '$2'.
sum_elem -> application                 : '$1'.
sum_elem -> pair                        : '$1'.
sum_elem -> collection                  : '$1'.
sum_elem -> sequence                    : '$1'.
sum_elem -> symbol                      : '$1'.
sum_elem -> qualified_symbol            : '$1'.
sum_elem -> literal                     : '$1'.

secondary_separator -> newlines : '$1'.
secondary_separator -> pipe newlines : '$1'.
secondary_separator -> pipe     : '$1'.



Erlang code.

unpack_sum([T]) -> T;
unpack_sum([T | _] = Terms) -> {sum, ctx(T), Terms}.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap({_,_,_,V}) -> V.

make_symbol({var_symbol, L, S}) -> {symbol, L, variable, S};
make_symbol({type_symbol, L, S}) -> {symbol, L, type, S}.
make_symbol({_, L, S}, Type) -> {symbol, L, Type, S}.

ctx({_, Ctx})         -> Ctx;
ctx({_, Ctx, _})      -> Ctx;
ctx({_, Ctx, _, _})   -> Ctx;
ctx([Head|_]) 		  -> ctx(Head).
