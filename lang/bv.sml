(* BV language definition *)
signature BV =
sig
  type id = int

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
                    | O_Bonus

  datatype program = Lambda of id * expr

  val show : program -> string
  val show_expr : expr -> string

  val all_operators        : operator list
  val all_operators_tfold  : operator list
  val all_operators_nofold : operator list

  val contains_fold_expr : expr -> bool
  val contains_fold      : program -> bool

  val check_freevars_expr : id list -> expr-> bool
  val check_freevars      : program -> bool

  val size_expr : expr -> int
  val size      : program -> int

  val constexpr : expr -> bool
end

structure BV : BV =
struct

  type id = int

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

  exception BadID
  fun show_id 0 = "x"
    | show_id 1 = "y"
    | show_id 2 = "z"
    | show_id _ = raise BadID

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
          ^ " (lambda (" ^ show_id x ^ " " ^ show_id y ^ ") " ^ show_expr e ^ "))"
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

  (* miscellaneous *)

  val all_operators_nofold =
    List.map (fn x => O_Unop x) [Not, Shl1, Shr1, Shr4, Shr16]@
    List.map (fn x => O_Binop x) [And, Or, Xor, Plus]@
    [O_Ifz]
  val all_operators = O_Fold::all_operators_nofold
  val all_operators_tfold = O_Tfold::all_operators_nofold

  fun contains_fold_expr Zero = false
    | contains_fold_expr One = false
    | contains_fold_expr (Id _) = false
    | contains_fold_expr (Ifz (e0,e1,e2)) =
        contains_fold_expr e0 orelse contains_fold_expr e1 orelse contains_fold_expr e2
    | contains_fold_expr (Fold _) = true
    | contains_fold_expr (Unop (oper,e)) = contains_fold_expr e
    | contains_fold_expr (Binop (oper,e1,e2)) =
        contains_fold_expr e1 orelse contains_fold_expr e2

  fun contains_fold (Lambda (_,e)) = contains_fold_expr e

  fun check_freevars_expr g Zero = true
    | check_freevars_expr g One = true
    | check_freevars_expr g (Id x) = List.exists (fn y => y=x) g
    | check_freevars_expr g (Ifz (e0,e1,e2)) =
        check_freevars_expr g e0 andalso
        check_freevars_expr g e1 andalso
        check_freevars_expr g e2
    | check_freevars_expr g (Fold (e0,e1,x,y,e2)) =
        check_freevars_expr g e0 andalso
        check_freevars_expr g e1 andalso
        check_freevars_expr (x::y::g) e2
    | check_freevars_expr g (Unop (oper,e)) = check_freevars_expr g e
    | check_freevars_expr g (Binop (oper,e1,e2)) =
        check_freevars_expr g e1 andalso check_freevars_expr g e2

  fun check_freevars (Lambda (x,e)) = check_freevars_expr [x] e

  fun constexpr Zero = true
    | constexpr One = true
    | constexpr (Id _) = false
    | constexpr (Ifz (e0,e1,e2)) =
        constexpr e0 andalso constexpr e1 andalso constexpr e2
    | constexpr (Fold (e0,e1,x,y,e2)) =
        constexpr e0 andalso constexpr e1 andalso constexpr e2
    | constexpr (Unop (oper,e)) = constexpr e
    | constexpr (Binop (oper,e1,e2)) =
        constexpr e1 andalso constexpr e2
end

