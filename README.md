# Lexing

Open erlang OTP using `erl`. Then use `leex` to generate `lexer.erl` and the compiler to compile it:

```
leex:file('src/lexer')
> {ok,"src/lexer.erl"}
c('src/lexer')
> {ok, lexer}
```

We can now call the lexer on input using `lexer:string`:

```
lexer:string("def blah 1 + 3").
> {ok,[{def,1,def},
     {var,1,blah},
     {integer,1,1},
     {add_op,1,'+'},
     {integer,1,3}],
    1}
lexer:string("def ?blah").
> {error,{1,lexer,{illegal,"?"}},1}
```
