Terminals
    def val macro fun_decl
    keyword_symbol var_symbol
    rightbias_operator plus_operator mult_operator comp_operator eq_operator
    minus_operator div_operator caret_operator other_operator
    value
    open close square_open square_close curly_open curly_close
    space_open space_square_open space_curly_open
    apply comma newlines assign
    module_keyword export_keyword import_keyword
    pipe right_arrow slash colon semicolon.

Nonterminals
    root statements statement module_statements
    module import export implies
    pattern patterns pattern_application pattern_verb braced_pattern
    clauses clause fun definition def_keyword
    application arguments arglist arg
    infix rightbias_infix leftbias_infix
    noun verb expression block block_elem recur_block_elem let seq
    collection list dict list_elements
    dict_elements dict_element
    symbol operator
    pair pair_key
    qualified_symbol
    arg_separator separator line_separator clause_separator.

Rootsymbol root.

Left 900 slash.

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

Left 50 pipe.
Right 50 line_separator.
Unary 200 pattern.
Unary 250 braced_pattern.
Unary 300 pattern_verb.

root -> statements                   : '$1'.
root -> newlines statements          : '$2'.



% Statements
% ----------

statements -> statement                           : ['$1'].
statements -> statement line_separator statements : ['$1' | '$3'].

statement -> definition                 : '$1'.
statement -> module                     : '$1'.
statement -> import                     : '$1'.
statement -> export                     : '$1'.
statement -> statement line_separator   : '$1'.

implies -> right_arrow          : '$1'.

definition -> def_keyword symbol fun                : {name('$1'), ctx('$1'), unwrap('$2'), '$3'}.
definition -> def_keyword symbol newlines fun       : {name('$1'), ctx('$1'), unwrap('$2'), '$4'}.
definition -> def_keyword symbol implies expression : {name('$1'), ctx('$1'), unwrap('$2'), '$4'}.

def_keyword -> def : '$1'.
def_keyword -> macro : '$1'.

line_separator -> semicolon : '$1'.
line_separator -> newlines  : '$1'.


% Symbols
% -------

symbol -> keyword_symbol          : make_symbol('$1').
symbol -> var_symbol              : make_symbol('$1').
symbol -> operator                : '$1'.
symbol -> rightbias_operator      : make_symbol('$1', operator).

operator -> caret_operator        : make_symbol('$1', operator).
operator -> mult_operator         : make_symbol('$1', operator).
operator -> div_operator          : make_symbol('$1', operator).
operator -> plus_operator         : make_symbol('$1', operator).
operator -> minus_operator        : make_symbol('$1', operator).
operator -> comp_operator         : make_symbol('$1', operator).
operator -> eq_operator           : make_symbol('$1', operator).
operator -> other_operator        : make_symbol('$1', operator).

qualified_symbol -> symbol slash symbol            : {qualified_symbol, ctx('$1'), ['$1', '$3']}.
qualified_symbol -> symbol slash dict              : {qualified_symbol, ctx('$1'), ['$1', '$3']}.
qualified_symbol -> symbol slash qualified_symbol  : {qualified_symbol, ctx('$1'), ['$1' | unwrap('$3')]}.



% Module
% ------

module -> module_keyword symbol module_statements              : {module, ctx('$1'), ['$2'], '$3'}.
module -> module_keyword qualified_symbol module_statements    : {module, ctx('$1'), unwrap('$2'), '$3'}.

module_statements -> space_open close                           : [].
module_statements -> space_open statements close                : '$2'.

export -> export_keyword dict                           : {exports, ctx('$1'), unwrap('$2')}.

import -> import_keyword symbol                         : {import, ctx('$1'), ['$2']}.
import -> import_keyword qualified_symbol               : {import, ctx('$1'), unwrap('$2')}.



% Expressions
% -----------

expression -> block                                     : '$1'.  % (val a = 1; a + 2)
expression -> application                               : '$1'.  % f(a)
expression -> collection                                : '$1'.  % {a, b: T}
expression -> symbol                                    : '$1'.  % a
expression -> qualified_symbol                          : '$1'.  % a/b/T
expression -> value                                     : '$1'.  % 1 or "string" or 'atom'
expression -> expression pipe expression                : {'or', ctx('$1'), '$1', '$3'}.  % A | B
%expression -> expression and expression                 : {'and', ctx('$1'), '$1', '$3'}.  % A & B



% Expression Block
% ----------------

block -> space_open block_elem close            : '$2'.
block -> open block_elem close                  : '$2'.

block_elem -> fun_decl fun          : '$2'.
block_elem -> pair                  : '$1'.
block_elem -> recur_block_elem      : '$1'.

recur_block_elem -> let         : '$1'.
recur_block_elem -> seq         : '$1'.
recur_block_elem -> expression  : '$1'.

let -> val pattern assign expression line_separator recur_block_elem      : {'let', ctx('$1'), '$2', '$4', '$6'}.
seq -> recur_block_elem line_separator recur_block_elem                   : {seq, ctx('$1'), '$1', '$3'}.



% Application
% -----------
% * f(a, b, c)
% * f(a -> True)
% * a.f
% * a.f(b, c)
% * a.f(b -> False)

% Things that can be called before the dot in an application
noun -> application                 : '$1'.
noun -> collection                  : '$1'.
noun -> block                       : '$1'.
noun -> symbol                      : '$1'.
noun -> qualified_symbol            : '$1'.
noun -> value                       : '$1'.

% Things that can be called after the dot in an application
verb -> block                   : '$1'.
verb -> symbol                  : '$1'.
verb -> qualified_symbol        : '$1'.
verb -> value                   : '$1'.

application -> noun arguments                       : {application, ctx('$1'), '$1', '$2'}.
application -> noun apply verb                      : {application, ctx('$1'), '$3', ['$1']}.
application -> noun apply verb arguments            : {application, ctx('$1'), '$3', ['$1' | '$4']}.
application -> infix                                : '$1'.

arguments -> open close                             : [].
arguments -> open arglist close                     : '$2'.

arglist -> arg                                      : ['$1'].
arglist -> arg comma                                : ['$1'].
arglist -> arg comma arglist                        : ['$1' | '$3'].

% Why can an arg be a pattern?
% -- When parsing for example `f(g(a))`, the argument `g(a)` can be parsed
%    either as a pattern or as an expression. Because `pattern` is declared as
%    `nonassoc` it takes precedence over expression. This is a problem because
%    the `fun` nonterminal starts with `pattern`, and when we don't list
%    `pattern` as one of the options below and a pattern is parsed, the parser
%    will expect the result of the `fun` syntax. By adding a `pattern` as one
%    of the options, the parser can parse an `expression` that happens to also
%    look like a `pattern` but isn't part of a `fun`.
%    : '$1'.
arg -> pattern                                      : '$1'.                                       
arg -> expression                                   : '$1'.
arg -> fun_decl fun                                 : '$2'.
arg -> fun                                          : '$1'.

infix -> leftbias_infix                             : '$1'.
infix -> rightbias_infix                            : '$1'.
leftbias_infix -> noun operator noun                : {application, ctx('$2'), '$2', ['$1', '$3']}.
leftbias_infix -> infix operator noun               : {application, ctx('$2'), '$2', ['$1', '$3']}.
rightbias_infix -> noun rightbias_operator noun     : {application, ctx('$2'), make_symbol('$2', operator), ['$1', '$3']}.
rightbias_infix -> noun rightbias_operator infix    : {application, ctx('$2'), make_symbol('$2', operator), ['$1', '$3']}.



% Pair
% ----

pair -> pair_key colon expression : {pair, ctx('$1'), '$1', '$3'}.

% pair_key -> application                 : '$1'.
pair_key -> collection                  : '$1'.
pair_key -> symbol                      : '$1'.
pair_key -> qualified_symbol            : '$1'.
pair_key -> value                       : '$1'.


% Clauses and Functions
% -------

fun -> clauses                              : {'fun', ctx('$1'), '$1'}.

clauses -> clause                           : ['$1'].
clauses -> clause clause_separator clauses  : ['$1' | '$3'].

clause -> patterns implies expression       : {clause, ctx('$1'), '$1', '$3'}.

clause_separator -> comma : '$1'.
clause_separator -> comma newlines : '$1'.


% Patterns
% --------

patterns -> pattern                             : ['$1'].
patterns -> pattern patterns                    : ['$1' | '$2'].

pattern -> symbol                               : '$1'.   % a
pattern -> pattern_application                  : '$1'.   % T.A(S)
% For braced patterns, spaced_open is used to distinguise `a(1)` (application)
% from `a (1)` (two separate patterns)
pattern -> space_open braced_pattern close      : '$2'.   % (...)
pattern -> collection                           : '$1'.   % {a, b: T}
pattern -> qualified_symbol                     : '$1'.   % a/b/T
pattern -> value                                : '$1'.   % 2

braced_pattern -> pair                          : '$1'.
braced_pattern -> application                   : '$1'.
braced_pattern -> pattern                       : '$1'.
braced_pattern -> open braced_pattern close     : '$2'.   % (...)
braced_pattern -> pattern pipe braced_pattern   : {'or', ctx('$1'), '$1', '$3'}. % A | B

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
list -> square_open list_elements square_close                 : {list, ctx('$1'), '$2'}.
dict -> curly_open curly_close                               : {dict, ctx('$1'), []}.
dict -> curly_open dict_elements curly_close                 : {dict, ctx('$1'), '$2'}.

list -> space_square_open square_close                             : {list, ctx('$1'), []}.
list -> space_square_open list_elements square_close                 : {list, ctx('$1'), '$2'}.
dict -> space_curly_open curly_close                               : {dict, ctx('$1'), []}.
dict -> space_curly_open dict_elements curly_close                 : {dict, ctx('$1'), '$2'}.

list_elements -> expression                                 : ['$1'].
list_elements -> expression separator                       : ['$1'].
list_elements -> expression separator list_elements         : ['$1' | '$3'].

dict_element -> pair : '$1'.
dict_element -> noun : '$1'.

dict_elements -> dict_element                          : ['$1'].
dict_elements -> dict_element separator                : ['$1'].
dict_elements -> dict_element separator dict_elements  : ['$1' | '$3'].

separator -> comma          : '$1'.
separator -> comma newlines : '$1'.
separator -> newlines       : '$1'.



Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap({_,_,_,V}) -> V.

make_symbol({var_symbol, L, S}) -> {symbol, L, variable, S};
make_symbol({keyword_symbol, L, S}) -> {symbol, L, keyword, S}.
make_symbol({_, L, S}, Type) -> {symbol, L, Type, S}.

ctx({_, Ctx})           -> Ctx;
ctx({_, Ctx, _})        -> Ctx;
ctx({_, Ctx, _, _})     -> Ctx;
ctx({_, Ctx, _, _, _})  -> Ctx;
ctx([Head|_]) 		    -> ctx(Head).

name(Tuple) when is_tuple(Tuple)    -> element(1, Tuple).
