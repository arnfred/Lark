Definitions.

Identifier	= [a-z][a-zA-Z0-9_]*
TypeSymbol	= [A-Z][a-zA-Z0-9_]*
Number       	= [0-9]
% string taken from Reia
String	 	= "(\\\^.|\\.|[^\"])*"
Apply 		= \.
White      	= [\s|\n]
Comment 	= #.*
Open    	= \(
Close   	= \)
OpenBlock    	= {
CloseBlock   	= }
OpenList	= \[
CloseList	= \]
Equals		= =
Def		= def
Type		= type
Guard		= \|
Ltr		= ->
Rtl		= <-
Split		= :

% These shouldn't be exposed as language primitives, but I'm including them to get started
AddOp     	= (\+|-)
MulOp     	= (\*|/|%)
CompOp    	= (<|<=|==|===|>=|>|!=|!==)
BoolOp   	= (and|or)
Bool    	= (true|false)


Rules.

{Number}+   		: {token, {integer,	TokenLine, list_to_integer(TokenChars)}}.
{Number}+\.{Number}+   	: {token, {float,	TokenLine, list_to_float(TokenChars)}}.
{Open}			: {token, {open,	TokenLine, list_to_atom(TokenChars)}}.
{Close}			: {token, {close,	TokenLine, list_to_atom(TokenChars)}}.
{OpenBlock}		: {token, {open_block,	TokenLine, list_to_atom(TokenChars)}}.
{CloseBlock}		: {token, {close_block,	TokenLine, list_to_atom(TokenChars)}}.
{OpenList}		: {token, {open_list,	TokenLine, list_to_atom(TokenChars)}}.
{CloseList}		: {token, {close_list,	TokenLine, list_to_atom(TokenChars)}}.
{Def}  			: {token, {def,		TokenLine, list_to_atom(TokenChars)}}.
{Type}  		: {token, {type,	TokenLine, list_to_atom(TokenChars)}}.
{End}   		: {token, {endl,	TokenLine, list_to_atom(TokenChars)}}.
{Equals}		: {token, {equals,	TokenLine, list_to_atom(TokenChars)}}.
{Rtl}			: {token, {rtl,		TokenLine, list_to_atom(TokenChars)}}.
{Ltr}			: {token, {ltr,		TokenLine, list_to_atom(TokenChars)}}.
{Identifier}	   	: {token, {var,		TokenLine, list_to_atom(TokenChars)}}.
{Split}			: {token, {split_op,	TokenLine, list_to_atom(TokenChars)}}.
{White}+  		: skip_token.
{Comment}		: skip_token.
{String} 		: build_string(string, TokenChars, TokenLine, TokenLen).

% I would rather these didn't exist, but I'm including them to get started
&			: {token, {and_op,	TokenLine, list_to_atom(TokenChars)}}.
!			: {token, {or_op,	TokenLine, list_to_atom(TokenChars)}}.
{CompOp}   		: {token, {comp_op,	TokenLine, list_to_atom(TokenChars)}}.
{Bool}   		: {token, {boolean,	TokenLine, list_to_atom(TokenChars)}}.
{BoolOp}   		: {token, {bool_op,	TokenLine, list_to_atom(TokenChars)}}.
\^			: {token, {xor_op,	TokenLine, list_to_atom(TokenChars)}}.
{AddOp}   		: {token, {add_op,	TokenLine, list_to_atom(TokenChars)}}.
{MulOp}   		: {token, {mul_op,	TokenLine, list_to_atom(TokenChars)}}.
{UnaryOp}   		: {token, {unary_op,	TokenLine, list_to_atom(TokenChars)}}.


Erlang code.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2)),
    {token, {Type, Line, String}}.

unescape_string(String) -> unescape_string(String, []).
 
unescape_string([], Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).
