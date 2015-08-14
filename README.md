# OCaLustre

# Description

Synchronous extension to OCaml with the style of the Lustre synchronous programming language

OCaml is extended with "nodes" that are synchronous functions,
running at every instant.

An instant is the atomic unit of time at which a node computes outputs from inputs.

Inputs and outputs are considered as data flows, that is a flow of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...  

# Syntax 
```ocaml
let%node NAMEOFNODE INPUTS OUTPUTS = 
  OUT1 := EXPR; 
  ... 
  OUTN := EXPR
  
```
with
<br />
```ocaml
VALUE ::= "string" | int | bool | ... 
NAMEOFNODE ::= string 
IDENT ::= string 
UNIT ::= () 
PARAMETERS ::= (IDENT,IDENT,...) | IDENT | UNIT
INTPUTS ::= PARAMETERS 
OUTPUTS ::= PARAMETERS 
OUT ::= IDENT | (IDENT,IDENT) >
INFIXOP ::= + | - | / | * | --> | < | > | <= | >= | = | <> 
PREFIXOP ::= pre | not 
EXPR ::=   UNIT 
        | if EXPR then EXPR else EXPR
	| IDENT PARAMETERS (* the application of the function named IDENT *)
	| EXPR INFIXOP EXPR
	| PREFIXOP EXPR
	| VALUE
	| (EXPR,EXPR)
```
NB: The sequence of assignations ( OUT := EXPR; ... ) can be listed in any order (even if a variable in an expression has not yet been assigned), for example:
```ocaml
  a := b * 5;
  b := 9
```

# Synchronous Operators

The --> operator is the init operator : it initializes a flow with a value for the first instant and another value for the next instants<br />

The pre operator is the memory operator : it returns the value of the flow at the previous instant<br />

For example :
```ocaml
   n := 0 --> (pre n) + 1
```
means that n is equal to 0 at the first instant and then to its previous value + 1 for the next instants. Thus, n is the flow of natural integers : 0, (0+1), (0+1+1) , ...<br />

# Example
```ocaml

let%node bool_to_string (bool) (string) =
  string := if bool then "TRUE" else "FALSE"

let%node xor_writer (a,b) (x,s) =
   s:= bool_to_string (x);
   x:= if a then (not b) else b
```

Returns the boolean value a xor b as well as a string representation of it