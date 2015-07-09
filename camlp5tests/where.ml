open Camlp4

module Id : Sig.Id = struct 
  let name = "where" 
  let version = "0.1" 
end 

module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax

  let _loc = Loc.ghost
  let rec exp_let_bindings body = function
    | [] -> <:expr< >>
    | [(rf,lb)] -> <:expr< let $rec:rf$ $lb$ in $body$ >>
    | (rf,lb)::xs -> <:expr< let $rec:rf$ $lb$ in $exp_let_bindings body xs$ >>

    EXTEND Gram
      GLOBAL: expr;

      let_binding_seq: [[ rf = opt_rec; lb = let_binding -> (rf,lb) ]];
      expr: BEFORE ":="
        [ "where"
          [ e = SELF; "where"; lb = LIST1 let_binding_seq SEP "and" ->
            <:expr< $exp_let_bindings e lb$ >> ]
        ];
    END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
