
val string_dec : char list -> char list -> bool

type 'a nelist =
| Nebase of 'a
| Necons of 'a * 'a nelist

type ident = char list

val ident_eqb : char list -> char list -> bool

type clock =
| Cbase
| Con of clock * ident * bool

type const =
| Cconst

type lexp =
| Econst of const * clock
| Evar of ident
| Ewhen of lexp * ident * bool
| Ebinop of lexp * lexp

type lexps = lexp nelist

type lidents = ident nelist

type cexp =
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp
| Eexp of lexp

type equation =
| EqDef of ident * clock * cexp
| EqApp of lidents * clock * ident * lexps
| EqFby of ident * clock * const * lexp

type node = { n_name : ident; n_input : ident nelist;
              n_output : ident nelist; n_eqs : equation list }

val n_input : node -> ident nelist

val n_output : node -> ident nelist

val n_eqs : node -> equation list

type clockenv = (char list*clock) list

val assoc : char list -> clockenv -> clock option

val eqb : bool -> bool -> bool

val clock_eqb : clock -> clock -> bool

val clockof_var : clockenv -> ident -> clock option

val clockof_vars : clockenv -> ident nelist -> clock option

val clockof_lexp : clockenv -> lexp -> clock option

val clockof_lexps : clockenv -> lexp nelist -> clock option

val clockof_cexp : clockenv -> cexp -> clock option

val well_clocked_eq : clockenv -> equation -> bool

val well_clocked_eqs : clockenv -> equation list -> bool

val well_clocked_node : clockenv -> node -> bool
