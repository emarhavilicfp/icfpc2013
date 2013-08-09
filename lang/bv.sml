(* BV language definition *)
signature BV =
sig
  datatype id = Ident of string

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

  datatype program = Lambda of id * expr
  
  val show : program -> string
  
  val parse : string -> program
  
end

structure BV : BV = 
struct

  datatype id = Ident of string
 
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

  datatype program = Lambda of id * expr

  (* Programs -> Text *)

  fun show_id (Ident s) = s

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

  (* Text -> Programs *)

  fun parse _ = (Lambda ((Ident "x"), (Id (Ident "x")))) (* TODO implement *)

  (* Pretty printer tests *)

  fun test () = let
    fun assert_eq x y msg =
      print (if x = y then "\027[01;32m%% OK %%\027[00m\n"
             else "\027[01;31m!! FAIL: " ^ msg ^ " !!\027[00m\n")
    (* tests AST -> str -> AST is a fixpoint *)
    fun test_from_prog prog = let
      val str = show prog
    in
      assert_eq prog (parse str) str
    end
    (* tests str -> AST -> str -> AST is a fixpoint *)
    fun test_from_str str = let
      val prog = parse str
    in
      assert_eq prog (parse (show prog)) str
    end
  in
    test_from_prog (Lambda ((Ident "x"), (Id (Ident "x"))));
    test_from_str  "(lambda (x) x)";
    test_from_prog (Lambda ((Ident "x"), (Binop (Plus,(Id (Ident "x")),(Id (Ident "x"))))));
    ()
  end
end

