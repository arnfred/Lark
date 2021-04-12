Definitions.

Symbol            = [a-z_\<=>*^+$-][a-zA-Z0-9_\<=>*^+$-]*
Keyword_Symbol    = [A-Z][a-zA-Z0-9_<=>-]*
Operator          = [\<=>*^+$-][a-zA-Z0-9_\<=>*^+$-]*
DivOperator       = //[a-zA-Z0-9_\<=>*^+$-]*
RightBiasOperator = [:][a-zA-Z0-9_\<=>*^+$-]+
Number            = [0-9]
SignedNumber      = [-]?[0-9]
String            = "(\\\^.|\\.|[^\"])*"
Atom              = '(\\\^.|\\.|[^\'])*'
RightArrow        = ->
LeftArrow         = <-

Rules.

{SignedNumber}+             : {token, {value,               #{line => TokenLine}, integer, list_to_integer(TokenChars)}}.
{SignedNumber}+\.{Number}+  : {token, {value,               #{line => TokenLine}, float, list_to_float(TokenChars)}}.
{DivOperator}               : {token, {op_type(TokenChars), #{line => TokenLine}, list_to_atom(TokenChars)}}.
\(                          : {token, {open,                #{line => TokenLine}, list_to_atom(TokenChars)}}.
\)                          : {token, {close,               #{line => TokenLine}, list_to_atom(TokenChars)}}.
\{                          : {token, {curly_open,          #{line => TokenLine}, list_to_atom(TokenChars)}}.
\}                          : {token, {curly_close,         #{line => TokenLine}, list_to_atom(TokenChars)}}.
\[                          : {token, {square_open,         #{line => TokenLine}, list_to_atom(TokenChars)}}.
\]                          : {token, {square_close,        #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\s]+\(                     : {token, {space_open,          #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\s]+\{                     : {token, {space_curly_open,    #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\s]+\[                     : {token, {space_square_open,   #{line => TokenLine}, list_to_atom(TokenChars)}}.
\|                          : {token, {pipe,                #{line => TokenLine}, list_to_atom(TokenChars)}}.
\/                          : {token, {slash,               #{line => TokenLine}, list_to_atom(TokenChars)}}.
\:                          : {token, {colon,               #{line => TokenLine}, list_to_atom(TokenChars)}}.
\.                          : {token, {apply,               #{line => TokenLine}, list_to_atom(TokenChars)}}.
,                           : {token, {comma,   	        #{line => TokenLine}, list_to_atom(TokenChars)}}.
=                           : {token, {assign,              #{line => TokenLine}, list_to_atom(TokenChars)}}.
{RightArrow}                : {token, {right_arrow,         #{line => TokenLine}, list_to_atom(TokenChars)}}.
{LeftArrow}                 : {token, {left_arrow,          #{line => TokenLine}, list_to_atom(TokenChars)}}.
def                         : {token, {def,                 #{line => TokenLine}, list_to_atom(TokenChars)}}.
val                         : {token, {val,                 #{line => TokenLine}, list_to_atom(TokenChars)}}.
type                        : {token, {type,                #{line => TokenLine}, list_to_atom(TokenChars)}}.
macro                       : {token, {macro,               #{line => TokenLine}, list_to_atom(TokenChars)}}.
import                      : {token, {import_keyword,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
module                      : {token, {module_keyword,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
{Operator}                  : {token, {op_type(TokenChars), #{line => TokenLine}, list_to_atom(TokenChars)}}.
{RightBiasOperator}         : {token, {rightbias_operator,  #{line => TokenLine}, list_to_atom(TokenChars)}}.
{Symbol}                    : {token, {var_symbol,          #{line => TokenLine}, list_to_atom(TokenChars)}}.
{Keyword_Symbol}            : {token, {keyword_symbol,      #{line => TokenLine}, list_to_atom(TokenChars)}}.
[\n]+                       : {token, {newlines,            #{line => TokenLine}, list_to_atom(TokenChars)}}.
{String}                    : {token, {value,               #{line => TokenLine}, string, build_string(TokenChars, TokenLen)}}.
{Atom}                      : {token, {value,               #{line => TokenLine}, atom, build_atom(TokenChars, TokenLen)}}.
[\s]+                       : skip_token.
\%.*                         : skip_token.

Erlang code.

build_atom(Chars, Len) -> list_to_atom(lists:sublist(Chars, 2, Len - 2)).

build_string(Chars, Len) -> unicode:characters_to_binary(unescape_string(lists:sublist(Chars, 2, Len - 2))).

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

op_type([$+ | _]) -> plus_operator;
op_type([$* | _]) -> mult_operator;
op_type([$< | _]) -> comp_operator;
op_type([$> | _]) -> comp_operator;
op_type([$= | _]) -> eq_operator;
op_type([$- | _]) -> minus_operator;
op_type([$/ | _]) -> div_operator;
op_type([$^ | _]) -> caret_operator;
op_type(_)        -> other_operator.
