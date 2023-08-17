
val negb : bool -> bool

val app : 'a1 list -> 'a1 list -> 'a1 list

val eq_bool : bool -> bool -> bool

val eq_ascii : char -> char -> bool

type identifier = char list

val eq_identifier : identifier -> identifier -> bool

type type_constr = char list

type h = char list

val eq_h : h -> h -> bool

type unop =
| Unopminus
| Unopminusf
| Unopnot

type constant =
| Kint
| Kfloat
| Kbool

type binop =
| Binopop_int
| Binopop_float
| Binopop_compare
| Binopop_bool

type ck =
| Ckbase
| Ckon of ck * identifier
| Ckonnot of ck * identifier
| Cktuple of ck * ck
| Ckarrow of ck * ck

type e =
| Eunit of ck
| Econst of constant * ck
| Evar of identifier
| Econstructor of type_constr * ck
| Ebinop of e * binop * e
| Eunop of unop * e
| Ewhen of e * identifier
| Ewhennot of e * identifier

type cexp =
| Ceexp of e
| Cemerge of identifier * cexp * cexp
| Ceif of e * cexp * cexp

type xs =
| Patp_nil
| Patp_var of identifier
| Patp_tuple of identifier * xs

type es =
| Esone_exp of e
| Escons_exps of e * es

type eqn =
| EqDef of identifier * ck * cexp
| EqFby of identifier * ck * constant * e
| EqApp of xs * ck * identifier * es
| EqEval of identifier * ck * h * e

type leqns =
| Eqseqs_one of eqn
| Eqseqs_cons of eqn * leqns

type c = (char list*ck) list

type nodedef =
| Nodemk_node of identifier * xs * xs * leqns

val eq_ck : ck -> ck -> bool

type sign =
| Signcons of xs * ck * xs * ck

type s = identifier list

type h0 = (char list*sign) list

val assoc : char list -> c -> ck option

val assoc_global : char list -> h0 -> sign option

val apply_subst : ck -> identifier -> identifier -> ck

val apply_substs : (identifier*identifier) list -> ck -> ck

val carriers : ck -> s

val subst_ck : ck -> ck -> ck

val mem_S : char list -> identifier list -> bool

val e_subst_fun_var :
  identifier -> identifier -> identifier list -> (identifier*identifier) list
  option

val e_subst_fun_exp :
  identifier -> es -> identifier list -> (identifier*identifier) list option

val e_subst_fun :
  xs -> es -> identifier list -> (identifier*identifier) list option

val p_subst_fun_vars :
  identifier -> identifier -> identifier list -> (identifier*identifier) list
  option

val p_subst_fun :
  xs -> xs -> identifier list -> (identifier*identifier) list option

val clockof_exp : c -> e -> ck option

val clockof_exps : c -> es -> ck option

val eq_ident : identifier -> identifier -> bool

val eq_clock : ck -> ck -> bool

val clockof_cexp : c -> cexp -> ck option

val clockof_pat : c -> xs -> ck option

val clockof_params : c -> e -> ck option

val well_clocked_equation : eqn -> h0 -> c -> bool

val well_clocked_eqns : leqns -> h0 -> c -> bool

val clockof_node : nodedef -> h0 -> c -> sign option

val well_clocked_prog : (c*nodedef) list -> h0 -> bool
