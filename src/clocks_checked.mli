
val negb : bool -> bool

val app : 'a1 list -> 'a1 list -> 'a1 list

val eq_bool : bool -> bool -> bool

val eq_ascii : char -> char -> bool

type identifier = char list

val eq_identifier : identifier -> identifier -> bool

type type_constr = char list

type ocaml_expr = char list

val eq_ocaml_expr : ocaml_expr -> ocaml_expr -> bool

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

type clock =
| Ckbase
| Ckon of clock * identifier
| Ckonnot of clock * identifier
| Cktuple of clock * clock
| Ckarrow of clock * clock

type lexp =
| Eunit of clock
| Econst of constant * clock
| Evar of identifier
| Econstructor of type_constr * clock
| Ebinop of lexp * binop * lexp
| Eunop of unop * lexp
| Ewhen of lexp * identifier
| Ewhennot of lexp * identifier

type cexp =
| Ceexp of lexp
| Cemerge of identifier * cexp * cexp
| Ceif of lexp * cexp * cexp

type pattern =
| Patp_unit
| Patp_var of identifier
| Patp_tuple of identifier * pattern

type lexps =
| Esone_exp of lexp
| Escons_exps of lexp * lexps

type equation =
| EqDef of pattern * clock * cexp
| EqFby of pattern * clock * constant * lexp
| EqApp of pattern * clock * identifier * lexps
| EqEval of pattern * clock * ocaml_expr

type leqns =
| Eqseqs_one of equation
| Eqseqs_cons of equation * leqns

type c = (char list*clock) list

type nodedef =
| Nodemk_node of identifier * pattern * pattern * leqns

val eq_clock : clock -> clock -> bool

type sign =
| Signcons of pattern * clock * pattern * clock

type s = identifier list

type h = (char list*sign) list

val assoc : char list -> c -> clock option

val assoc_global : char list -> h -> sign option

val apply_subst : clock -> identifier -> identifier -> clock

val apply_substs : (identifier*identifier) list -> clock -> clock

val carriers : clock -> s

val subst_ck : clock -> clock -> clock

val mem_S : char list -> identifier list -> bool

val e_subst_fun_var :
  identifier -> identifier -> identifier list -> (identifier*identifier) list option

val e_subst_fun_exp : identifier -> lexps -> identifier list -> (identifier*identifier) list option

val e_subst_fun : pattern -> lexps -> identifier list -> (identifier*identifier) list option

val p_subst_fun_vars :
  identifier -> identifier -> identifier list -> (identifier*identifier) list option

val p_subst_fun : pattern -> pattern -> identifier list -> (identifier*identifier) list option

val clockof_exp : c -> lexp -> clock option

val clockof_exps : c -> lexps -> clock option

val eq_ident : identifier -> identifier -> bool

val eq_ck : clock -> clock -> bool

val clockof_cexp : c -> cexp -> clock option

val clockof_pat : c -> pattern -> clock option

val well_clocked_equation : equation -> h -> c -> bool

val well_clocked_eqns : leqns -> h -> c -> bool

val clockof_node : nodedef -> h -> c -> sign option

val well_clocked_prog : (c*nodedef) list -> h -> bool
