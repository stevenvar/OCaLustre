# OCaLustre

**Description**

Synchronous extension to OCaml with the style of the Lustre synchronous programming language.

OCaml is extended with "nodes" that are synchronous functions,
running at every instant.

An instant is the atomic unit of time at which a node computes outputs from inputs.

Inputs and outputs are considered as data flows, that is a flow of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...  

**Syntax**

```ocaml
let%node <ident> ~i:<inputs> ~o:<outputs> =
  <out> = <expr>;
  ...
  <out> = <expr>

```
with
<br />
```ocaml
<value> ::= int | bool | float
<ident> ::= [a-zA-z][a-zA-Z0-9]*
<param> ::= (<ident>,*) | <ident>
<inputs> ::= <param>
<outputs> ::= <param>
<out> ::= <ident> | (<ident>,*)
<binop> ::= + | - | / | * | +. | -. | /. | *. | --> | ->> | < | > | <= | >= | = | <>
<unop> ::= not | - | -.
<expr> ::= ()
       | if <expr> then <expr> else <expr>
       | <ident> <param> (* the application of the function named IDENT *)
       | <expr> <binop> <expr>
       | <unop> EXPR
       | <value>
       | (<expr>,*)
       | pre <expr>
```
NB: The sequence of assignations ( OUT := EXPR; ... ) can be listed in any order (even if a variable in an expression has not yet been assigned), for example:
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

**Synchronous Operators**

- The --> operator is the init operator : it initializes a flow with a value for the first instant and another value for the next instants<br />

- The pre operator is the memory operator : it returns the value of the flow at the previous instant<br />

For example :
```ocaml
   n := 0 --> ( pre n + 1 )
```
means that n is equal to 0 at the first instant and then to its previous value + 1 for the next instants. Thus, n is the flow of natural integers : 0, (0+1), (0+1+1) , ...<br />

- The ->> operator (known as fby - followed by - in Lustre) mixes the two and is similar to "--> pre" , so the previous example can also be written :

```ocaml
   n := 0 ->> (n + 1)
```
**Quick install**

OCaLustre is still a prototype! But if you really want to try it out, just do :

```
  oasis setup && make install
```

And use it with ocamlfind as any other package, for example :
```
  ocamlfind ocamlc -package ocalustre foo.ml
```


**Example**

```ocaml

let%node fibonacci ~i:() ~o:(f) =
  f = 1 ->> ( 1 --> pre f + f)
```

Returns the fibonacci sequence : 1, 1, 2, 3, 5, 8, 13, ...  
