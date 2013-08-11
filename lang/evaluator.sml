signature EVAL =
sig
  type word

  val eval : BV.program -> word -> word
  val eval_constexpr : BV.expr -> word
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
              val _ = Array.update (G, acc, res)
              val _ = Array.update (G, e, lowByte)
              val res' = eval_expr G step
            in
              fold' (n-1) res' (Word64.>> (vec, 0w8))
            end
    in
      fold' 8 init vec
    end

  and eval_unop G BV.Not e = Word64.notb (eval_expr G e)
    | eval_unop G BV.Shl1 e = Suq.Word64.<< (eval_expr G e, Word32.fromInt 1)
    | eval_unop G BV.Shr1 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 1)
    | eval_unop G BV.Shr4 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 4)
    | eval_unop G BV.Shr16 e = Suq.Word64.>> (eval_expr G e, Word32.fromInt 16)

  and eval_binop G BV.And e1 e2 = Word64.andb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BV.Or e1 e2 = Word64.orb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BV.Xor e1 e2 = Word64.xorb (eval_expr G e1, eval_expr G e2)
    | eval_binop G BV.Plus e1 e2 = Word64.+ (eval_expr G e1, eval_expr G e2)

  and eval_expr _ BV.Zero = Word64.fromInt 0
    | eval_expr _ BV.One = Word64.fromInt 1
    | eval_expr G (BV.Id i) = Array.sub (G, i)
    | eval_expr G (BV.Ifz (e, e1, e2)) =
      (case Word64.compare (eval_expr G e, Word64.fromInt 0) of
          EQUAL => eval_expr G e1
        | _ => eval_expr G e2)
    | eval_expr G (BV.Fold (vec, init, e, acc, step)) =
      let
        val vec' = eval_expr G vec
        val init' = eval_expr G init
        (* Allow for shadowing *)
        val G' = Array.array (3, 0w0)
        val _ = Array.copy {src=G, dst=G', di=0}
      in
        eval_fold G' vec' init' e acc step
      end
    | eval_expr G (BV.Unop (oper, e)) = eval_unop G oper e
    | eval_expr G (BV.Binop (oper, e1, e2)) = eval_binop G oper e1 e2

  fun eval_constexpr e =
    let
      (* Don't tread on me *)
      val G = Array.array (0, 0w0)
    in eval_expr G e end

  fun eval (BV.Lambda (i, e)) w =
    let
      val G = Array.array (3, 0w0)
      val _ = Array.update(G, i, w)
    in eval_expr G e end
end

structure EvalTest =
struct
  open Eval;
  infixr 0 $
  fun f $ x = f x

  fun eval_str msg str input expected =
    let
      val result = eval (TestUtil.Parse str) (Word64.fromInt input)
    in
      Assert.assert (msg ^ " expected: " ^ Int.toString expected
                         ^ " got: " ^ Word64.toString result) $
        (Word64.fromInt expected) = result
    end

  fun test () = List.all (fn x => x) [
    eval_str "basic test" "(lambda (x) (plus x x))" 10 20,
    eval_str "2comp1" "(lambda (x) (plus (not x) 1))" 1 (~1),
    eval_str "2comp4" "(lambda (x) (plus (not x) 1))" 4 (~4),
    eval_str "fold1" "(lambda (x) (fold 0 0 (lambda (y z) (plus x z))))" 10 80,
    eval_str "fold2" "(lambda (x) (fold 0 1 (lambda (y z) (shl1 z))))" 0 256,
    eval_str "dauthi_marauder"
      "(lambda (x) (fold 1 0 (lambda (x z) (plus x z))))" 0 1,
    eval_str "dauthi_horror"
      "(lambda (x) (fold x 0 (lambda (z x) (plus x x))))" (~1) 0,
    eval_str "dauthi_slayer"
      "(lambda (x) (fold x 0 (lambda (x z) (plus x x))))" (~1) (255*2),
    true]
end
