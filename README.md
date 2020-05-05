# Kind

> Hello babies. Welcome to Earth. It's hot in the summer and cold in the winter. It's round and wet and crowded. On the outside, babies, you've got a hundred years here. There's only one rule that I know of, babies-"God damn it, you've got to be kind."

_Kurt Vonnegut_.

# Compiling and Running Tests

Use `rebar3` to compile and run tests:

```
rebar3 compile
rebar3 eunit
```

# Notes from building individual components

As I've built the compiler, I've tried to keep notes on how to run and test new additions as I've progressed. I've kept these notes here because I think they might be valuable for someone else (or me from the future) trying to repurpose the code for something else.

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

## Using the Debugger
To use the debugger, first open a shell with the modules on path:

```
> rebar3 shell
```

Then attach the debugger and run a command:
```
dbg:tracer().
dbg:p(all, c). % Trace all processes, only calls

% tpl = trace local functions as well
% x means print exceptions (and normal return values)
dbg:tpl(domain, union, x). 
domain:union({product, #{a => 1, b => 2}}, {product, #{a => 1, c => 3}}).
```

More information in [this helpful stackoverflow answer](https://stackoverflow.com/questions/6438041/how-to-debug-erlang-code).

## Type representation

How do I represent a line containing e.g. `type Bool = True | False`?

The answer probably depends on what I need to do with the type afterwards:
* I'd like for a match to know that it is complete
* I'd like for the type-checker to understand that the values belong to a type
* I'd like to be able to qualify the type values so that True always refer to the same atom

# Running from CLI

Ideally I'd like to run the Kind compiler via the cli as in `kind compile my-program.kind` and run the output of the compiler using `kind run my-program`. However this turns out not to be so super simple. Erlang programs are compiled to beam files that run on the Erlang Beam VM.

Using Rebar3 we can produce a release that includes a script for running Kind:

```
rebar3 release
_build/test/rel/kind/bin/kind-0.0.1 console
```

However, this script is made for deploying Erlang code as a long running process and doesn't support passing in arguments from the command line (at least not as far as I can tell).

One way I've found of parsing in arguments is to use the `escript` option instead which can either be used alone or through the binary:

```
 escript _build/default/lib/kind/ebin/cli.beam "test"
 > Args: ["test"]

./_build/default/rel/kind/bin/kind escript lib/kind-0.1.0/ebin/cli.beam test
> Args: ["test"]
```

# Next Steps

This is currently very unfinished. Instead of a Trello board, I'm keeping this section as way to think about next steps.

### 2020-04-15

* Product types (e.g. `type Test = Blip(blap: Bool, blup: Bool)`)
* Evaluation rules for tuples (e.g. `(val x = False, x or True)` should evaluate to `True`)
* Guards for pattern match (e.g. `a.match(n if n < 5 -> True | _ -> False)`)
* Type inference from guard constraints (e.g. `def not a -> a.match(True -> False | False -> true)` infers that `not` accepts only `True` and `False`
* Type


### 2020-04-01

* ~Code generation support for type enum declarations (e.g. `type Bool = True | False`)~
* Parser module to sanitise AST into something more intuitive
* ~guards and matching~
* type inference
* ~Figure out an FFI so that I can add support for lists and numbers without hard-coding it in to the code-gen~
* lists
* infix functions
* numbers?
