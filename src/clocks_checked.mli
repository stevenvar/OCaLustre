
val app : 'a1 list -> 'a1 list -> 'a1 list

val string_dec : char list -> char list -> bool

type 'a nelist =
| Nebase of 'a
| Necons of 'a * 'a nelist

type ident = char list

type operator =
| Op

type clock =
| Cbase
| Con of clock * ident
| Conot of clock * ident

type const =
| Cc

type lexp =
| Econst of const * clock
| Evar of ident
| Ebinop of operator * lexp * lexp
| Ewhen of lexp * ident
| Ewhenot of lexp * ident

type cexp =
| Eexp of lexp
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp

type lexps = lexp nelist

type lidents = ident nelist

type lclocks = clock nelist

type equation =
| EqDef of ident * clock * cexp
| EqFby of ident * clock * const * lexp
| EqApp of lidents * clock * ident * lexps
| EqTuple of lidents * lclocks * lexps

type leqns = equation nelist

type tau = clock nelist*ident nelist

type d =
| Mk_node of ident * lidents * lclocks * lidents * lclocks * leqns

type ident_pair = ident*ident

type xlist = ident list

type lident_pair = ident_pair list

val subst_ck : clock -> clock -> clock

val subst_base : lclocks -> clock -> lclocks

val clockof : clock -> xlist

val clocks : lclocks -> xlist

val ident_eqb : char list -> char list -> bool

val assoc_sub : char list -> (char list*char list) list -> char list

val subst_name_ck : clock -> lident_pair -> clock

val subst_names : clock nelist -> lident_pair -> clock nelist

type clockenv = (char list*clock) list

type globalclockenv = (char list*(tau*tau)) list

val assoc : char list -> clockenv -> clock option

val assoc_global : char list -> globalclockenv -> (tau*tau) option

val mem_S : char list -> xlist -> bool

val clock_eqb : clock -> clock -> bool

val clocks_eqb : clock nelist -> clock nelist -> bool

val clk_subst_fun :
  char list nelist -> lexp nelist -> xlist -> (char list*ident) list option

val clk_subst_var_fun :
  lidents -> lidents -> xlist -> (ident*ident) list option

val clockof_var : clockenv -> ident -> clock option

val clockof_global_var : globalclockenv -> ident -> (tau*tau) option

val clockof_vars : clockenv -> ident nelist -> clock nelist option

val clockof_lexp : clockenv -> lexp -> clock option

val clockof_lexps : clockenv -> lexp nelist -> clock nelist option

val clockof_cexp : clockenv -> cexp -> clock option

val well_clocked_eq : globalclockenv -> clockenv -> equation -> bool

val well_clocked_eqs : globalclockenv -> clockenv -> equation nelist -> bool

val well_clocked_node : globalclockenv -> clockenv -> d -> bool
