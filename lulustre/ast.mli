type 'a node = {
 name : string ;
 inputs : string list;  
 outputs : string list; 
 equations : 'a equation list; 
}
and 
 'a equation = { 
 pattern : 'a pattern ; 
 expression : 'a expression; 
} 
and 
 'a pattern = ('a ident) list
and
 camlvalue 
and
  ('a, 'b) localized = { loc : 'a ; content : 'b }
and
 'a expression = ('a , exp_desc) localized 
and
 'a ident = ('a , string) localized
and
 exp_desc =   
  | Alternative of exp_desc * exp_desc * exp_desc 
  | InfixOp of inf_operator * exp_desc * exp_desc 
  | PrefixOp of pre_operator * exp_desc
  | Value of camlvalue  
  | Variable of string ident  
and
 inf_operator = 
  | Plus
  | Minus
  | Times
  | Div
  | Arrow 
and
 pre_operator = 
  | Not
  | Opposed 
  | Pre 
