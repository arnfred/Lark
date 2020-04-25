Terminals
    def type val 
    symbol match_keyword
    integer float string
    open close square_open square_close curly_open curly_close
    apply comma newline assign
    pipe right_arrow slash colon.

Nonterminals
    all definitions definition
    assignment function newtype implies
    pattern_match clause patterns clauses def_clauses clause_tuple
    application 
    noun verb expression
    collection tuple list dict
    sum sum_list sum_or_expression
    symbols newlines elements 
    literal element pair qualified_symbol
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

assignment -> val noun assign expression            : {val, line('$1'), unwrap('$2'), '$4'}.
function -> def symbols implies expression          : {def, line('$1'), name('$2'), args('$2'), '$4'}.
function -> def symbols newlines def_clauses        : {def, line('$1'), name('$2'), args('$2'), '$4'}.
newtype -> type symbols implies sum_or_expression   : {type_def, line('$1'), name('$2'), args('$2'), '$4'}.

symbols -> symbol           : ['$1'].
symbols -> symbol symbols   : ['$1' | '$2'].

expression -> pair          : '$1'.
expression -> noun          : '$1'.

noun -> application         : '$1'.
noun -> pattern_match       : '$1'.
noun -> verb                : '$1'.

verb -> literal             : '$1'.
verb -> symbol              : '$1'.
verb -> qualified_symbol    : {qualified_symbol, line('$1'), unwrap_symbols('$1')}.
verb -> collection          : '$1'.

qualified_symbol -> symbol slash symbol           : ['$1', '$3'].
qualified_symbol -> symbol slash qualified_symbol : ['$1' | '$3'].

literal -> string   : '$1'.
literal -> integer  : '$1'.
literal -> float    : '$1'.

application -> verb collection               : {application, line('$2'), '$1', build_args('$2')}.
application -> noun apply verb               : {application, line('$1'), '$3', ['$1']}.
application -> noun apply verb collection    : {application, line('$1'), '$3', build_args('$1', '$4')}.

pattern_match -> noun apply match_keyword clause_tuple   : {match, line('$1'), '$1', '$4'}.

clause -> patterns implies expression : {clause, line('$2'), '$1', '$3'}.
patterns -> noun                      : ['$1'].
patterns -> noun patterns             : ['$1' | '$2'].

clause_tuple -> open clauses close              : '$2'.
clause_tuple -> open newlines clauses close     : '$3'.
clauses -> clause                               : ['$1'].
clauses -> clause secondary_separator clauses   : ['$1' | '$3'].

def_clauses -> pipe clause                      : ['$2'].
def_clauses -> pipe clause newlines def_clauses : ['$2' | '$4'].

collection -> tuple : '$1'.
collection -> list  : '$1'.
collection -> dict  : '$1'.

tuple -> open close                                     : {tuple, line('$1'), []}.
tuple -> open sum close                                 : {tuple, line('$1'), '$2'}.
tuple -> open elements close                            : {tuple, line('$1'), '$2'}.
tuple -> open newlines elements close                   : {tuple, line('$1'), '$3'}.
list -> square_open square_close                        : {list, line('$1'), []}.
list -> square_open elements square_close               : {list, line('$1'), '$2'}.
list -> square_open newlines elements square_close      : {list, line('$1'), '$3'}.
dict -> curly_open curly_close                          : {dict, line('$1'), []}.
dict -> curly_open elements curly_close                 : {dict, line('$1'), '$2'}.
dict -> curly_open newlines elements curly_close        : {dict, line('$1'), '$3'}.

separator -> comma          : '$1'.
separator -> comma newlines : '$1'.
separator -> newlines       : '$1'.

elements -> element                     : ['$1'].
elements -> element newlines            : ['$1'].
elements -> element separator elements  : ['$1' | '$3'].

secondary_separator -> newlines : '$1'.
secondary_separator -> pipe     : '$1'.

element -> expression    : '$1'.
element -> assignment           : '$1'.
element -> newtype              : '$1'.
element -> clauses              : {clauses, line('$1'), '$1'}.

pair -> verb colon noun : {pair, line('$1'), '$1', '$3'}.

sum_or_expression -> sum_list : unpack_tuple('$1').
sum_list -> expression : ['$1'].
sum_list -> expression pipe sum_list : ['$1' | '$3'].
sum -> expression secondary_separator expression : ['$1', '$3'].
sum -> expression secondary_separator sum : ['$1' | '$3'].

Erlang code.

unpack_tuple([T]) -> T;
unpack_tuple([T | [_ | _]] = Terms) -> {tuple, line(T), Terms}.

name([{symbol, _, S} | _]) -> S.
args([_ | Args]) -> Args.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

unwrap_symbols(Symbols) -> [S || {symbol, _, S} <- Symbols].

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line;
line([Head|_]) 		-> line(Head).

build_args({tuple, _, Elems}) -> Elems;
build_args(Collection) -> [Collection].
build_args(Elem, {tuple, _, Elems}) -> [ Elem | Elems ];
build_args(Elem, Collection) -> [ Elem, Collection ].
