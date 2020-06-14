Terminals
    def type val 
    type_symbol var_symbol match_keyword
    integer float string
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    pipe right_arrow slash colon.

Nonterminals
    all definitions definition
    assignment function newtype implies
    pattern_match clause pattern patterns clauses clause_tuple pattern_tuple
    def_clauses flat_clause
    type_clauses flat_type_clause
    application index lookup
    noun verb expression
    collection tuple list dict
    sum sum_list sum_or_expression
    symbol symbols newlines elements 
    literal element pair qualified_type qualified_variable qualified_type_type
    separator secondary_separator.

Rootsymbol all.

Right 400 secondary_separator.
Right 300 separator.

all -> definitions          : '$1'.
all -> newlines definitions : '$2'.

definitions -> definition                                  : ['$1'].
definitions -> definition newlines                         : ['$1'].
definitions -> definition newlines definitions             : ['$1' | '$3'].

newlines -> newline          : '$1'.
newlines -> newline newlines : '$1'.

definition -> function          : '$1'.
definition -> newtype           : '$1'.

implies -> right_arrow          : '$1'.
implies -> right_arrow newlines : '$1'.

assignment -> val expression assign expression      : {val, ctx('$1'), '$2', '$4'}.
function -> def symbols implies expression          : {def, ctx('$1'), name('$2'), args('$2'), '$4'}.
function -> def symbols newlines def_clauses        : {def, ctx('$1'), name('$2'), args('$2'), '$4'}.
newtype -> type symbols implies sum_or_expression   : {type_def, ctx('$1'), name('$2'), args('$2'), '$4'}.
newtype -> type symbols newlines type_clauses       : {type_def, ctx('$1'), name('$2'), args('$2'), '$4'}.

symbol -> type_symbol : make_symbol('$1').
symbol -> var_symbol : make_symbol('$1').

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

expression -> noun          : '$1'.

noun -> pattern_match       : '$1'.
noun -> pattern             : '$1'.

pattern -> application      : '$1'.
pattern -> index            : '$1'.
pattern -> lookup           : '$1'.
pattern -> literal          : '$1'.
pattern -> verb             : '$1'.
pattern -> pair             : '$1'.

verb -> symbol              : '$1'.
verb -> qualified_type      : {qualified_type, ctx('$1'), unwrap_symbols('$1')}.
verb -> qualified_variable  : {qualified_variable, ctx('$1'), unwrap_symbols('$1')}.
verb -> collection          : '$1'.

qualified_type -> var_symbol slash qualified_type_type          : [make_symbol('$1') | '$3'].
qualified_type -> qualified_variable slash qualified_type_type  : '$1' ++ '$3'.
qualified_type -> qualified_type_type                           : '$1'.
qualified_type_type -> type_symbol slash type_symbol            : [make_symbol('$1'), make_symbol('$3')].
qualified_type_type -> type_symbol slash qualified_type_type    : [make_symbol('$1') | '$3'].

qualified_variable -> var_symbol slash var_symbol               : [make_symbol('$1'), make_symbol('$3')].
qualified_variable -> var_symbol slash qualified_variable       : [make_symbol('$1') | '$3'].

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

index -> pattern list                        : {index, ctx('$1'), '$1', unwrap('$2')}.
lookup -> pattern dict                       : {lookup, ctx('$1'), '$1', unwrap('$2')}.

application -> verb tuple                    : {application, ctx('$1'), '$1', unwrap('$2')}.
application -> noun apply verb               : {application, ctx('$1'), '$3', ['$1']}.
application -> noun apply verb tuple         : {application, ctx('$1'), '$3', ['$1' | unwrap('$4')]}.

def_clauses -> pipe flat_clause                      : ['$2'].
def_clauses -> pipe flat_clause newlines def_clauses : ['$2' | '$4'].

flat_clause -> pattern implies expression           : {clause, ctx('$2'), ['$1'], '$3'}.
flat_clause -> patterns implies expression          : {clause, ctx('$2'), '$1', '$3'}.

type_clauses -> pipe flat_type_clause                       : ['$2'].
type_clauses -> pipe flat_type_clause newlines type_clauses : ['$2' | '$4'].

flat_type_clause -> pattern implies sum_or_expression  : {clause, ctx('$2'), ['$1'], '$3'}.
flat_type_clause -> patterns implies sum_or_expression : {clause, ctx('$2'), '$1', '$3'}.

patterns -> pattern separator pattern    : ['$1', '$3'].
patterns -> pattern separator patterns   : ['$1' | '$3'].

pattern_tuple -> open patterns close          : '$2'.
pattern_tuple -> open newlines patterns close : '$3'.

pattern_match -> noun apply match_keyword clause_tuple   : {match, ctx('$1'), '$1', '$4'}.

clauses -> clause                               : ['$1'].
clauses -> clause secondary_separator clauses   : ['$1' | '$3'].

clause_tuple -> open clauses close              : '$2'.
clause_tuple -> open newlines clauses close     : '$3'.

clause -> pattern implies expression            : {clause, ctx('$1'), ['$1'], '$3'}.
clause -> pattern_tuple implies expression      : {clause, ctx('$1'), '$1', '$3'}.

collection -> tuple : '$1'.
collection -> list  : '$1'.
collection -> dict  : '$1'.

tuple -> open close                                     : {tuple, ctx('$1'), []}.
tuple -> open sum close                                 : {tuple, ctx('$1'), '$2'}.
tuple -> open elements close                            : {tuple, ctx('$1'), '$2'}.
tuple -> open newlines elements close                   : {tuple, ctx('$1'), '$3'}.
list -> square_open square_close                        : {list, ctx('$1'), []}.
list -> square_open elements square_close               : {list, ctx('$1'), '$2'}.
list -> square_open newlines elements square_close      : {list, ctx('$1'), '$3'}.
dict -> curly_open curly_close                          : {dict, ctx('$1'), []}.
dict -> curly_open elements curly_close                 : {dict, ctx('$1'), '$2'}.
dict -> curly_open newlines elements curly_close        : {dict, ctx('$1'), '$3'}.

separator -> comma          : '$1'.
separator -> comma newlines : '$1'.
separator -> newlines       : '$1'.

elements -> element                     : ['$1'].
elements -> element newlines            : ['$1'].
elements -> element separator elements  : ['$1' | '$3'].

secondary_separator -> newlines : '$1'.
secondary_separator -> pipe     : '$1'.

element -> expression           : '$1'.
element -> assignment           : '$1'.
element -> newtype              : '$1'.
element -> clauses              : {lambda, ctx('$1'), '$1'}.

pair -> noun colon noun : {pair, ctx('$1'), '$1', '$3'}.

sum_or_expression -> sum_list : unpack_tuple('$1').
sum_list -> expression : ['$1'].
sum_list -> expression pipe sum_list : ['$1' | '$3'].
sum -> expression secondary_separator expression : ['$1', '$3'].
sum -> expression secondary_separator sum : ['$1' | '$3'].

Erlang code.

unpack_tuple([T]) -> T;
unpack_tuple([T | _] = Terms) -> {tuple, ctx(T), Terms}.

name([{symbol, _, _, S} | _]) -> S.
args([_ | Args]) -> Args.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

make_symbol({var_symbol, L, S}) -> {symbol, L, variable, S};
make_symbol({type_symbol, L, S}) -> {symbol, L, type, S}.

unwrap_symbols(Symbols) -> [{T, S} || {_, _, T, S} <- Symbols].

ctx({_, Ctx})         -> Ctx;
ctx({_, Ctx, _})      -> Ctx;
ctx({_, Ctx, _, _})   -> Ctx;
ctx([Head|_]) 		    -> ctx(Head).
