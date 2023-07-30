# OCaLustre

## Description

Synchronous extension of OCaml in the style of the Lustre synchronous programming language.


The OCaml language is extended with Lustre "nodes". These nodes can be viewed as synchronous functions,
which run at every instant. An instant is the atomic unit of time at which the main node of the program computes outputs from inputs.

Inputs and outputs are considered as data flows, i.e. streams of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...

Here is an OcaLustre node that takes two streams a and b, and produces a stream c that is the sum of a and b :

```ocaml
let%node example (a,b) ~return:(c) =
  c = a + b
```

## Published versions

A description of a previous version of the prototype has been published for the 8th European Congress on Embedded Real Time Software and Systems (ERTS 2016) (read the article at https://hal.archives-ouvertes.fr/hal-01292266/)

A french description of a more complete version has been described here : https://hal.archives-ouvertes.fr/hal-01446225/document

## Complete syntax

```ocaml
let%node <ident> <inputs> ~return:<outputs> =
  <out> = <expr>;
  ...
  <out> = <expr>

```
with
<br />
```ocaml
<expr> ::= ()
       | if <expr> then <expr> else <expr>
       | <ident> <param> (* function application *)
       | <expr> <binop> <expr>
       | <value> ->> <expr>
       | <unop> <expr>
       | <value>
       | (<expr>,*)
       | <expr> [@ when <ident>]
       | <expr> [@ whennot <ident>]
       | merge <ident> <expr> <expr>
<ident> ::= [a-zA-z][a-zA-Z0-9]*
<value> ::= int | bool | float | constant constructor
<param> ::= (<ident>,*) | <ident>
<inputs> ::= (<param>,*)
<outputs> ::= (<param>,*)
<out> ::= <ident> | (<ident>,*)
<binop> ::= + | - | / | * | +. | -. | /. | *. | < | > | <= | >= | = | <>
<unop> ::= not | - | -.
```
NB: The sequence of assignations can be listed in any order (even if a variable in an expression has not yet been assigned), for example:
```ocaml
let%node foo () ~return:(a,c,b) =
  c = b + a;
  b = a * 3;
  a = 7
```

is - at compile time - automatically transformed into :

```ocaml
let%node foo () ~return:(a,c,b) =
  a = 7;
  b = a * 3;
  c = b + a
```

Note that scenarios where streams mutually depend on each others (ie. causality loops) are rejected during compilation :

```ocaml
let%node causloop () ~return:(a,b) =
  a = 7 + b;
  b = a - 2
```
```
  Error:Causality loop in node causloop including these variables : b a
```

## Synchronous Operators

- The ```->>``` operator (``followed by'' in Lustre / Lucid) is the initialized delay operator. It is used to define a stream as a constant value for the first instant and as the _previous_ value of another expression for the next instants :  (it is similar to "-> pre" in Lustre) :

```ocaml
   n = 0 ->> (n + 1)
```

means that n is equal to 0 at the first instant and then to the previous value of (n + 1) for the next instants (i.e. it's the stream of natural integers).
## Clocks

- You can use the ```[@ when _]``` annotation in order to generate streams at a slower rate. This operator takes an expression ```e``` and a clock ```ck``` (i.e. boolean flow) and produces the value of ```e``` only when ```ck``` is ```true```.

For example, in the following example, we return the value of x only when c is true:

```ocaml

let%node sampler (x,c) ~return:y =
   y = x [@ when c]
```

Clocks are equivalent to a type system and the type of the previous example is :

```(base * (ck_a : base)) -> (base on ck_a)```

With ```base``` being the implicit faster clock of the node and ```(c : base)``` meaning that ```c```is a clock itself on the clock ```base```


- The ```[@ whennot _]``` ("when not") annotation is the counterpart of ```[@ when _ ]```and produces a value only when its clock is ```false```

- You can use operators only on streams declared on the same clocks. The following node is correct :

```ocaml

let%node sampler (x1,x2,c) ~return:y =
   a = x1 [@ when c];
   b = x2 [@ when c];
   y = a + b
```

and has the following clock : ```(base * base * (ck_a : base)) -> (base on ck_a) ```

But the following example is incorrect :

```ocaml

let%node sampler (x1,x2,c,d) ~return:y =
   a = x1 [@ when c];
   b = x2 [@ when d];
   y = a + b
```

- You can combine two streams on complementary clocks ( ```'a on x```and ```'a on not x```) by using the ```merge``` operator. The result is on clock ```'a```

In the following example, we return the value ```1``` half the time, and the value ```2``` the other half of time:

```ocaml
let%node tictoc c ~return:y =
  a = 1 [@ when c];
  b = 2 [@ whennot c];
  y = merge c a b

let%node call_tictoc () ~return:d =
  c = true >>> (false >>> c);
  d = tictoc c
```

## Requirements

- OCaml (>= 4.08)
- ppx_tools (>= 6.5)
- oasis

## Quick install

OCaLustre is still a prototype! But if you want to try it out, just do :

```
opam pin add ocalustre git@github.com:stevenvar/OCaLustre.git
```

or

```
  dune build @install && dune install 
```

And use it as a ppx preprocessor :

```
ocamlc -ppx ocalustre tests/foo.ml
```

### Generation of the main loop

```
ocamlfind ppx_tools/rewriter "ocalustre -main my_main_node" ocalustre_file.ml -o ocaml_file.ml
```

The ```-main``` option, followed by the name of the principal node, generates the global loop of the program. Next, you just need to fill the correct input/output functions for this node in the generated ocaml file.
