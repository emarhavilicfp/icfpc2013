(* BV language definition *)
signature BV =
sig
  type id = Symbol.symbol

  datatype unop = Not | Shl1 | Shr1 | Shr4 | Shr16

  datatype binop = And | Or | Xor | Plus

  datatype expr = Zero
                | One
                | Id of id
                (* if e0 == 0 then e1 else e2 *)
                | Ifz of expr * expr * expr
                (* fold e_vec e_init (\x_element x_accum -> e_step) *)
                | Fold of expr * expr * id * id * expr
                | Unop of unop * expr
                | Binop of binop * expr * expr
  
  datatype operator = O_Unop of unop
                    | O_Binop of binop
                    | O_Ifz
                    | O_Tfold
                    | O_Fold

  datatype program = Lambda of id * expr

  val show : program -> string

end

structure BV : BV =
struct

  type id = Symbol.symbol

  datatype unop = Not | Shl1 | Shr1 | Shr4 | Shr16

  datatype binop = And | Or | Xor | Plus

  datatype expr = Zero
                | One
                | Id of id
                (* if e0 == 0 then e1 else e2 *)
                | Ifz of expr * expr * expr
                (* fold e_vec e_init (\x_element x_accum -> e_step) *)
                | Fold of expr * expr * id * id * expr
                | Unop of unop * expr
                | Binop of binop * expr * expr

  datatype operator = O_Unop of unop
                    | O_Binop of binop
                    | O_Ifz
                    | O_Tfold
                    | O_Fold

  datatype program = Lambda of id * expr

  (* Programs -> Text *)

  fun show_id id = Symbol.name id

  fun show_unop Not   = "not"
    | show_unop Shl1  = "shl1"
    | show_unop Shr1  = "shr1"
    | show_unop Shr4  = "shr4"
    | show_unop Shr16 = "shr16"

  fun show_binop And  = "and"
    | show_binop Or   = "or"
    | show_binop Xor  = "xor"
    | show_binop Plus = "plus"

  fun show_expr Zero = "0"
    | show_expr One = "1"
    | show_expr (Id x) = show_id x
    | show_expr (Ifz (e0,e1,e2)) =
          "(if0 " ^ show_expr e0 ^ " " ^ show_expr e1 ^ " " ^ show_expr e2 ^ ")"
    | show_expr (Fold (ev,e0,x,y,e)) =
          "(fold " ^ show_expr ev ^ " " ^ show_expr e0
          ^ " (lambda (" ^ show_id x ^ " " ^ show_id y ^ ") " ^ show_expr e ^ ")"
    | show_expr (Unop (oper,e)) = "(" ^ show_unop oper ^ " " ^ show_expr e ^ ")"
    | show_expr (Binop (oper,e1,e2)) =
          "(" ^ show_binop oper ^ " " ^ show_expr e1 ^ " " ^ show_expr e2 ^ ")"

  fun show (Lambda (x,e)) = "(lambda (" ^ show_id x ^ ") " ^ show_expr e ^ ")"

  (* computing AST size *)

  fun size_expr Zero = 1
    | size_expr One = 1
    | size_expr (Id x) = 1
    | size_expr (Ifz (e0,e1,e2)) =
        1 + size_expr e0 + size_expr e1 + size_expr e2
    | size_expr (Fold (ev,e0,x,y,e)) =
        2 + size_expr ev + size_expr e0 + size_expr e
    | size_expr (Unop (oper,e)) = 1 + size_expr e
    | size_expr (Binop (oper,e1,e2)) = 1 + size_expr e1 + size_expr e2

  fun size (Lambda (x,e)) = 1 + size_expr e
end

