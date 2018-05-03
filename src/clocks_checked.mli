
val string_dec : char list -> char list -> bool

type ident = char list

val ident_eqb : char list -> char list -> bool

type clock =
| Cbase
| Con of clock * ident * bool

type const =
| Cint of int
| Cbool of bool

type operator =
| Plus
| Minus
| Times

type lexp =
| Econst of const
| Evar of ident
| Ewhen of lexp * ident * bool
| Ebinop of operator * lexp * lexp

type lexps = lexp list

type cexp =
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp
| Eexp of lexp

type equation =
| EqDef of ident * clock * cexp
| EqApp of ident * clock * ident * lexps
| EqFby of ident * clock * const * lexp

type node = { n_name : ident; n_input : ident list; n_output : ident list;
              n_eqs : equation list }

val n_input : node -> ident list

val n_output : node -> ident list

val n_eqs : node -> equation list

type clockenv = (char list*clock) list

val assoc : char list -> clockenv -> clock option

val eqb : bool -> bool -> bool

val clock_eqb : clock -> clock -> bool

val clockof_var : clockenv -> ident -> clock option

val clockof_vars : clockenv -> ident list -> clock option

val clockof_lexp : clockenv -> lexp -> clock option

val clockof_lexps : clockenv -> lexp list -> clock option

val clockof_cexp : clockenv -> cexp -> clock option

val well_clocked_eq : clockenv -> equation -> bool

val well_clocked_eqs : clockenv -> equation list -> bool

val well_clocked_node : clockenv -> node -> bool
