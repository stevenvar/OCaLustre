# OCaLustre

# Description

Synchronous extension to OCaml with the style of the Lustre synchronous programming language

OCaml is extended with "nodes" that are synchronous functions,
running at every instant.

An instant is the atomic unit of time at which a node computes outputs from inputs.

Inputs and outputs are considered as data flows, that is a flow of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...  

# Syntax 

let%node NAMEOFNODE INPUTS OUTPUTS = <br />
  OUT1 := EXPR; <br />
  ... <br />
  OUTN := EXPR <br />
<br />
with
<br />

VALUE ::= "string" | int | bool | ... <br />

NAMEOFNODE ::= string <br />

IDENT ::= string <br />

UNIT ::= () <br />

PARAMETERS ::= (IDENT,IDENT,...) | IDENT | UNIT <br />

INTPUTS ::= PARAMETERS <br />

OUTPUTS ::= PARAMETERS <br />

OUT ::= IDENT | (IDENT,IDENT) <br />

INFIXOP ::= + | - | / | * | --> | < | > | <= | >= | = | <> <br />

PREFIXOP ::= pre | not 

EXPR ::=   UNIT <br />
           | if EXPR then EXPR else EXPR<br />
	   | IDENT PARAMETERS (* the application of the function named IDENT *)<br />
           | EXPR INFIXOP EXPR<br />
	   | PREFIXOP EXPR<br />
	   | VALUE<br />
	   | (EXPR,EXPR)<br />
<br />
NB: The sequence of assignations ( OUT := EXPR; ... ) can be listed in any order, for example:<br />
   a := b * 5;<br />
   b := 9<br />
<br />
# Synchronous Operators

The --> operator is the init operator : it initializes a flow with a value for the first instant and another value for the next instants<br />

The pre operator is the memory operator : it returns the value of the flow at the previous instant<br />

For example :<br />
   n := 0 --> (pre n) + 1<br />

means that n is equal to 0 at the first instant and then to its previous value + 1 for the next instants. Thus, n is the flow of natural integers : 0, (0+1), (0+1+1) , ...<br />

# Example

let%node xor_writer (a,b) (x,s) =<br />
   s:= if x then "TRUE" else "FALSE" <br />
   x:= if a then (not b) else b<br />
