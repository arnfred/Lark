Definitions.

Symbol          = [a-z_][a-zA-Z0-9_-]*
TypeSymbol      = [A-Z_][a-zA-Z0-9_-]*
Number          = [0-9]
String          = "(\\\^.|\\.|[^\"])*"
Implies         = ->
Qualifier       = blup

Rules.

{Number}+               : {token, {integer,         TokenLine, list_to_integer(TokenChars)}}.
{Number}+\.{Number}+    : {token, {float,           TokenLine, list_to_float(TokenChars)}}.
\(                      : {token, {open,            TokenLine, list_to_atom(TokenChars)}}.
\)                      : {token, {close,           TokenLine, list_to_atom(TokenChars)}}.
\{                      : {token, {curly_open,      TokenLine, list_to_atom(TokenChars)}}.
\}                      : {token, {curly_close,     TokenLine, list_to_atom(TokenChars)}}.
\|                      : {token, {bar,             TokenLine, list_to_atom(TokenChars)}}.
\/                      : {token, {qualifier,       TokenLine, list_to_atom(TokenChars)}}.
def                     : {token, {def,             TokenLine, list_to_atom(TokenChars)}}.
val                     : {token, {val,             TokenLine, list_to_atom(TokenChars)}}.
\.                      : {token, {apply,           TokenLine, list_to_atom(TokenChars)}}.
,                       : {token, {comma,   	    TokenLine, list_to_atom(TokenChars)}}.
=                       : {token, {assign,          TokenLine, list_to_atom(TokenChars)}}.
{Implies}               : {token, {implies,         TokenLine, list_to_atom(TokenChars)}}.
type                    : {token, {type,            TokenLine, list_to_atom(TokenChars)}}.
match                   : {token, {match_keyword,   TokenLine, list_to_atom(TokenChars)}}.
{Symbol}                : {token, {symbol,          TokenLine, list_to_atom(TokenChars)}}.
{TypeSymbol}            : {token, {type_symbol,     TokenLine, list_to_atom(TokenChars)}}.
[\n]+                   : {token, {newline,	        TokenLine, list_to_atom(TokenChars)}}.
[\s]+                   : skip_token.
#.*                     : skip_token.
{String}                : build_string(string, TokenChars, TokenLine, TokenLen).

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
