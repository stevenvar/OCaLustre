
val string_dec : char list -> char list -> bool

type 'a nelist =
| Nebase of 'a
| Necons of 'a * 'a nelist

type ident = char list

type const =
| Cc

type operator =
| Op

type clock =
| Cbase
| Con of clock * ident
| Conot of clock * ident

type lexp =
| Econst of const * clock
| Evar of ident
| Ebinop of operator * lexp * lexp
| Ewhen of lexp * ident
| Ewhenot of lexp * ident

type lidents = ident nelist

type lexps = lexp nelist

type cexp =
| Eexp of lexp
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp

type equation =
| EqDef of ident * clock * cexp
| EqFby of ident * clock * const * lexp
| EqApp of lidents * clock * ident * lexps

type leqns = equation nelist

type d =
| Mk_node of ident * lidents * lidents * leqns

val ident_eqb : char list -> char list -> bool

type clockenv = (char list*clock) list

val assoc : char list -> clockenv -> clock option

val clock_eqb : clock -> clock -> bool

val clockof_var : clockenv -> ident -> clock option

val clockof_vars : clockenv -> ident nelist -> clock option

val clockof_lexp : clockenv -> lexp -> clock option

val clockof_lexps : clockenv -> lexp nelist -> clock option

val clockof_cexp : clockenv -> cexp -> clock option

val well_clocked_eq : clockenv -> equation -> bool

val well_clocked_eqs : clockenv -> equation nelist -> bool

val well_clocked_node : clockenv -> d -> bool
