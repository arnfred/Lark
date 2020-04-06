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
parser:parse(Tokens).
> {ok,[{type,1,'Boolean',[{type_symbol,1,'True'}]},
       {type_application,1,'False',[]}]}
```

## Code Generation

Open earlang OTP using `erl`. Then compile the codegen module once you've followed the steps above to compile the lexer and parser:

```
c('src/codegen').
> {ok,codegen}
```

Then you can compile a string to code and execute it:

```
{ok, Tokens, _} = lexer:string("def id a = a").
{ok, Parsed} = parser:parse(Tokens).
{ok, Forms} = codegen:gen({"test", Parsed}).
{ok, Name, Bin} = compile:forms(Forms, [report, verbose, from_core]).
code:load_binary(Name, "test.beam", Bin).
test:id("hello world!").
> "hello world!"
```

To run the unit tests I've included the eunit lib which enables us to run:

```
codegen:test().
```

## Dializer

To type check the src directory, run dializer and see if it's happy:

```
dialyzer --build_plt --apps erts kernel stdlib mnesia compiler
dializer --src src
```

More info here: https://learnyousomeerlang.com/dialyzer

## Type representation

How do I represent a line containing e.g. `type Bool = True | False`?

The answer probably depends on what I need to do with the type afterwards:
* I'd like for a match to know that it is complete
* I'd like for the type-checker to understand that the values belong to a type
* I'd like to be able to qualify the type values so that True always refer to the same atom

## Next Steps

This is currently very unfinished. Instead of a Trello board, I'm keeping this section as way to think about next steps.

### 2020-04-01

* Code generation support for type enum declarations (e.g. `type Bool = True | False`)
* Parser module to sanitise AST into something more intuitive
* ~guards and matching~
* type inference
* Figure out an FFI so that I can add support for lists and numbers without hard-coding it in to the code-gen
* lists
* infix functions
* numbers?
