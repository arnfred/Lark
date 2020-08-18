# Kind

> Hello babies. Welcome to Earth. It's hot in the summer and cold in the winter. It's round and wet and crowded. On the outside, babies, you've got a hundred years here. There's only one rule that I know of, babies-"God damn it, you've got to be kind."

_Kurt Vonnegut_.

Kind is a programming language built on top of the [Erlang Virtual Machine (BEAM)](https://stackoverflow.com/questions/16779162/what-kind-of-virtual-machine-is-beam-the-erlang-vm). It's designed to be _simple to use_, _easy to read_ and provide _powerful static guarantees_. My goal with the langauge is to bring the goodness of both functional programming and strong type inference to the BEAM. It's inspired by a host of different language features that I've had the pleasure to use over the last two decades:

* The no-nonsense succint syntax of Haskell
* The ease of passing around and modifying data structures using Clojure
* The power of pattern matching in Erlang
* The expressability of Scala

The language relies on domain inference to learn the values that a function or variable can take. These values can be numbers, strings, relations, ranges or functions. Knowing the domain allows Kind to provide static guarantees similar to languages with dependent types, but without having the programmer jump through hoops of formal logic and complex type errors.

# Development

## Compiling and Running Tests

Use `rebar3` to compile and run tests:

```
rebar3 compile
rebar3 eunit
```

## Notes from building individual components

As I've built the compiler, I've tried to keep notes on how to run and test new additions as I've progressed. I've kept these notes here because I think they might be valuable for someone else (or me from the future) trying to repurpose the code for something else.

### Lexing

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

### Parsing

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

### Code Generation

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

### Dializer

To type check the src directory, run dializer and see if it's happy:

```
dialyzer --build_plt --apps erts kernel stdlib mnesia compiler
dializer --src src
```

More info here: https://learnyousomeerlang.com/dialyzer

### Using the Debugger
To use the debugger to run tests, first open a shell with the modules on path:

```
> rebar3 as test shell
```

Then attach the debugger:
```
debugger:start().
```

Once attached, use the GUI to add modules that are interpreted by the debugger. Then add break points.

At this point things are set up to run a test, but the default test timeout is 5 seconds which will interrupt the debugger. To avoid this issue, change the tests you're interested in running to have a longer timeout in the test source:

```
{timeout,3600, [?_assertMatch({ok, _}, parser:parse(text, [Module]))]}
```

Then run the tests:

```
r3:do("eunit --suite parser_test").
```

To run a specific test (if it's a generator), do:

```
r3:do("eunit --generator typegen_test:pattern_dict_sum_test_").
```

For more information, see this [very helpful stackoverflow answer](https://stackoverflow.com/questions/34658714/how-to-debug-my-eunit-test-suite-run-when-using-rebar3#34667937). There's also [this helpful answer](https://stackoverflow.com/questions/6438041/how-to-debug-erlang-code) if you want to use a command line debugger instead.

## Running from CLI

Ideally I'd like to run the Kind compiler via the cli as in `kind compile
my-program.kind` and run the output of the compiler using `kind run
my-program`. However this turns out not to be so super simple. Erlang programs
are compiled to beam files that run on the Erlang Beam VM.

Using Rebar3 we can produce a release that includes a script for running Kind:

```
rebar3 release
_build/test/rel/kind/bin/kind-0.0.1 console
```

However, this script is made for deploying Erlang code as a long running
process and doesn't support passing in arguments from the command line (at
least not as far as I can tell).

One way I've found of parsing in arguments is to use the `escript` option
instead which can either be used alone or through the binary:

```
 escript _build/default/lib/kind/ebin/cli.beam "test"
 > Args: ["test"]

./_build/default/rel/kind/bin/kind escript lib/kind-0.1.0/ebin/cli.beam test
> Args: ["test"]
```

# Type Classes
Type classes or traits are a way for a piece of code to call functions on a
type without having access to the function body at compile time. This allows
for an API to accept user types, as long as the user types have the neccessary
functions defined.

In kind, I propose implementing type classes by duck typing. If a library calls
a function `f` on a type `T`, the call is valid if `f` is defined for `T` in
one of the modules in the call stack. 

For the function `f` to be defined for the type `T` in a module `M`, the module
`M` needs to either have an import clause importing `f` or define `f` itself.
Because modules don't expose their imports, we export a transitive module
consisting of the exports of `M` alongside the scope generated by the imports
of `M` called `M_class`. So for example, if `M` imports `string/_` and the
`String` module defines `reverse`, then `M_class` will also export `reverse`.
This way a library we use that depends on `reverse` being defined in the call
stack for its input will be able to call the `reverse` function exported by
`M_class`.

It's possible that multiple definitions of `f` are imported in the same module.
In this case `f` as defined in `M_class` will pattern match against `T` and
call the appropriate function.

One of the features of type classes is that we can verify at compile time
whether there exists an `f` defined for `T`. This isn't usually a feature of
duck typing, but Kind is a strongly duck-typed system so we scan a main function
we can check that the call stack admits an `f` defined for `T`.

When exporting a library function, we also export the domain function. This
means that as we call our library function from some user code we can execute
the domain function and check if it type checks given the domains of the
function parameters. What we're interested in checking though, is if the call
to `f` is defined for `T` for a module along the call stack. To do that we
emulate a call stack while scanning and include module information for each
step. This way we can look up in the call stack if a module exports a function
`f` defined for `T` and error otherwise.

# Next Steps

This is currently very unfinished. Instead of a Trello board, I'm keeping this
section as way to think about next steps.

### 2020-07-02
I'd like to be able to compile a single-file program that's submitted via a
webpage. I think one possible path there is:

* Imports (to be able to import a prelude)
* Exports (so I can define in the prelude what's exported)
* A prelude (Not sure what it would contain other than `match` and maybe `True` and `False`)
* Main function (so I know what to input to the type inference)
* Codegen for `seq` and `let`
* Codegen for pattern matching

There's plenty more that would be nice to put in place:

* Better error messages
* support for range types (e.g. numbers)
* support for lists and list types
* inference for type statements
* constraints (or interfaces or type classs or whatever we want to call them)

I think it might be nice when I've gotten the first part done to develop some
examples of the language that are missing features and work out a path for
making them work, instead of trying to decide on an order of development untied
to actual code I'd like to see working.

### Long pause
For a while I mostly put these kind of lists in the commit message

### 2020-04-15

* ~Product types (e.g. `type Test = Blip(blap: Bool, blup: Bool)`)~
* ~Evaluation rules for tuples (e.g. `(val x = False, x or True)` should evaluate to `True`)~
* Guards for pattern match (e.g. `a.match(n if n < 5 -> True | _ -> False)`)
* ~Type inference from guard constraints (e.g. `def not a -> a.match(True -> False | False -> true)` infers that `not` accepts only `True` and `False`~


### 2020-04-01

* ~Code generation support for type enum declarations (e.g. `type Bool = True | False`)~
* ~Parser module to sanitise AST into something more intuitive~
* ~guards and matching~
* ~type inference~
* ~Figure out an FFI so that I can add support for lists and numbers without hard-coding it in to the code-gen~
* lists
* infix functions
* numbers?
