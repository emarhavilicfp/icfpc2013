signature EVAL =
sig
  type word

  val eval : BVP.program -> word -> word
  val eval_expr : word Symbol.table -> BVP.expr -> word
end

structure Eval : EVAL =
struct
  type word = Word64.word

  fun eval_fold G vec init e acc step =
    let
      fun fold' 0 res vec = res
        | fold' n res vec =
            let
              val lowByte = Word64.andb (vec, 0wxFF)
              val G' = Symbol.bind G (acc, res)
              val G' = Symbol.bind G' (e, lowByte)
              val res' = eval_expr G' step
            in
              fold' (n-1) res' (Word64.>> (vec, 0w8))
            end
    in
      fold' 8 init vec
    end

  and eval_unop G BVP.Not e = Word64.notb (eval_expr G e)
    | eval_unop G BVP.Shl1 e = Suq.Word64.<< (eval_expr G e, Word32.fromInt 1)
    | eval_unop G BVP.Shr1 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 1)
    | eval_unop G BVP.Shr4 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 4)
    | eval_unop G BVP.Shr16 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 16)

  and eval_binop G BVP.And e1 e2 = Word64.andb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BVP.Or e1 e2 = Word64.orb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BVP.Xor e1 e2 = Word64.xorb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BVP.Plus e1 e2 = Word64.+ (eval_expr G e1, eval_expr G e2)

  and eval_expr _ BVP.Zero = Word64.fromInt 0
    | eval_expr _ BVP.One = Word64.fromInt 1
    | eval_expr G (BVP.Id i) = Symbol.look' G i
    | eval_expr G (BVP.Ifz (e, e1, e2)) =
      (case Word64.compare (eval_expr G e, Word64.fromInt 0) of
          EQUAL => eval_expr G e1
        | _ => eval_expr G e2)
    | eval_expr G (BVP.Fold (vec, init, e, acc, step)) =
      let
        val vec' = eval_expr G vec
        val init' = eval_expr G init
      in
        eval_fold G vec' init' e acc step
      end
    | eval_expr G (BVP.Unop (oper, e)) = eval_unop G oper e
    | eval_expr G (BVP.Binop (oper, e1, e2)) = eval_binop G oper e1 e2

  fun eval (BVP.Lambda (i, e)) w =
    let
      val G = Symbol.bind Symbol.empty (i, w)
    in eval_expr G e end
end
