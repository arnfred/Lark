Definitions.

Symbol            = [a-z_][a-zA-Z0-9_-]*
Type_Symbol       = [A-Z][a-zA-Z0-9_-]*
Number            = [0-9]
String            = "(\\\^.|\\.|[^\"])*"
RightArrow        = ->
LeftArrow         = <-

Rules.

{Number}+               : {token, {integer,         #{line => TokenLine}, list_to_integer(TokenChars)}}.
{Number}+\.{Number}+    : {token, {float,           #{line => TokenLine}, list_to_float(TokenChars)}}.
\(                      : {token, {open,            #{line => TokenLine}, list_to_atom(TokenChars)}}.
\)                      : {token, {close,           #{line => TokenLine}, list_to_atom(TokenChars)}}.
\{                      : {token, {curly_open,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
\}                      : {token, {curly_close,     #{line => TokenLine}, list_to_atom(TokenChars)}}.
\|                      : {token, {pipe,            #{line => TokenLine}, list_to_atom(TokenChars)}}.
\/                      : {token, {slash,           #{line => TokenLine}, list_to_atom(TokenChars)}}.
\:                      : {token, {colon,           #{line => TokenLine}, list_to_atom(TokenChars)}}.
def                     : {token, {def,             #{line => TokenLine}, list_to_atom(TokenChars)}}.
val                     : {token, {val,             #{line => TokenLine}, list_to_atom(TokenChars)}}.
\.                      : {token, {apply,           #{line => TokenLine}, list_to_atom(TokenChars)}}.
,                       : {token, {comma,   	    #{line => TokenLine}, list_to_atom(TokenChars)}}.
=                       : {token, {assign,          #{line => TokenLine}, list_to_atom(TokenChars)}}.
{RightArrow}            : {token, {right_arrow,     #{line => TokenLine}, list_to_atom(TokenChars)}}.
{LeftArrow}             : {token, {left_arrow,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
type                    : {token, {type,            #{line => TokenLine}, list_to_atom(TokenChars)}}.
import                  : {token, {import_keyword,  #{line => TokenLine}, list_to_atom(TokenChars)}}.
module                  : {token, {module_keyword,  #{line => TokenLine}, list_to_atom(TokenChars)}}.
{Symbol}                : {token, {var_symbol,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
{Type_Symbol}           : {token, {type_symbol,     #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\n]+                   : {token, {newline,	    #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\s]+                   : skip_token.
#.*                     : skip_token.
{String}                : build_string(string, TokenChars, TokenLine, TokenLen).

Erlang code.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2)),
    {token, {Type, Line, String}}.

unescape_string(String) -> unescape_string(String, []).
 
unescape_string([], Output) -> lists:reverse(Output);
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
unescape_string([Char|Rest], Output) -> unescape_string(Rest, [Char|Output]).
