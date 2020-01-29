## Lexing

Open erlang OTP using `erl`. Then use `leex` to generate `lexer.erl` and the compiler to compile it:

```
leex:file('src/lexer').
> {ok,"src/lexer.erl"}
c('src/lexer').
> {ok,lexer}
```

We can now call the lexer on input using `lexer:string`:

```
lexer:string("def blah 1.3").
> {ok,[{def,1,def},
     {var,1,blah},
     {float,1,1.3}],
    1}
lexer:string("def ?blah").
> {error,{1,lexer,{illegal,"?"}},1}
```

## Parsing

Open erlang OTP using `erl`. Then use `yecc` to generate `parser.erl` and the compiler to compile it:

```
yecc:file('src/parser').
> {ok,"src/parser.erl"}
c('src/parser').
> {ok,parser}
```

Assuming you've also compiled the lexer above, we can parse a string of code:

```
{ok, Tokens, _} = lexer:string("type Boolean = True | False").
parser:parse(Tokens_Bool).
> {ok,[{type,1,'Boolean',[{type_symbol,1,'True'}]},
       {type_application,1,'False',[]}]}
```
