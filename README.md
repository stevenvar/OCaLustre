# OCaLustre

## Description

Synchronous extension of OCaml in the style of the Lustre synchronous programming language.

The OCaml language is extended with Lustre "nodes". These nodes can be viewed as synchronous functions,
which run at every instant. An instant is the atomic unit of time at which a node computes outputs from inputs.

Inputs and outputs are considered as data flows, i.e. flows of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...  

Here is an OcaLustre node that takes two flows a and b, and produces a flow c that is the sum of a and b :

```ocaml
let%node example ~i:(a,b) ~o:(c) =
  c = a + b
```

## Complete syntax

```ocaml
let%node <ident> ~i:<inputs> ~o:<outputs> =
  <out> = <expr>;
  ...
  <out> = <expr>

```
with
<br />
```haskell
<expr> ::= ()
       | if <expr> then <expr> else <expr>
       | <ident> <param> (* function application *)
       | <expr> <binop> <expr>
       | <expr> --> <expr>
       | <value> ->> <expr>
       | <unop> <expr>
       | <value>
       | (<expr>,*)
       | pre <expr>
<ident> ::= [a-zA-z][a-zA-Z0-9]*
<value> ::= int | bool | float
<param> ::= (<ident>,*) | <ident>
<inputs> ::= (<param>,*)
<outputs> ::= (<param>,*)
<out> ::= <ident> | (<ident>,*)
<binop> ::= + | - | / | * | +. | -. | /. | *. | < | > | <= | >= | = | <>
<unop> ::= not | - | -.
```
NB: The sequence of assignations can be listed in any order (even if a variable in an expression has not yet been assigned), for example:
```ocaml
let%node foo ~i:() ~o:(a,c,b) =
  c = b + a;
  b = a * 3;
  a = 7
```

is - at compile time - automatically transformed into :

```ocaml
let%node foo ~i:() ~o:(a,c,b) =
  a = 7;
  b = a * 3;
  c = b + a
```

Note that scenarios where flows mutually depend on each others (ie. causality loops) are rejected during compilation :

```ocaml
let%node loop ~i:() ~o:(a,b) =
  a = 7 + b;
  b = a - 2
```
```
  Error:Causality loop in node loop including these variables : b a
```

## Synchronous Operators

- The --> operator is the init operator : it initializes a flow with a value for the first instant and another value for the next instants.


For example :
```ocaml
   n = 0 --> 1
```

produces `0, 1, 1, 1, ...`

- The pre operator is the memory operator : it returns the value of the flow at the previous instant.

For example :
```ocaml
   n = 0 --> ( pre n + 1 )
```
means that n is equal to 0 at the first instant and then to its previous value + 1 for the next instants. Thus, n is the flow of natural integers : `0, 1, 2, 3, 4, ...`



- The ->> operator (known as fby - followed by - in Lustre) mixes the two and is similar to "--> pre" , so the previous example can also be written :

```ocaml
   n = 0 ->> (n + 1)
```

## Requirements

- OCaml (>= 4.03)
- ppx_tools
- oasis

## Quick install

OCaLustre is still a prototype! But if you really want to try it out, just do :

```
  oasis setup && make install
```

And use it with ocamlfind as any other package, for example :
```
  ocamlfind ocamlc -package ocalustre foo.ml
```


## Example

```ocaml

let%node fibonacci ~i:() ~o:(f) =
  f = 0 ->> ( 1 --> (pre f + f))
```

Returns the fibonacci sequence : `1, 1, 2, 3, 5, 8, 13, ...`  
