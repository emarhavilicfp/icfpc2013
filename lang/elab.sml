signature ELAB =
sig
  val elab : BVP.program -> BV.program
end

structure Elab : ELAB =
struct
  open Haskell

  val nextIndex = ref 0

  fun getIndex () =
    let
      val ret = !nextIndex
    in
      (nextIndex := (ret + 1); ret)
    end

  fun elab_unop G BVP.Not e = BV.Unop(BV.Not, elab_expr G e)
    | elab_unop G BVP.Shl1 e = BV.Unop(BV.Shl1,  elab_expr G e)
    | elab_unop G BVP.Shr1 e = BV.Unop(BV.Shr1, elab_expr G e)
    | elab_unop G BVP.Shr4 e = BV.Unop(BV.Shr4, elab_expr G e)
    | elab_unop G BVP.Shr16 e = BV.Unop(BV.Shr16, elab_expr G e)

  and elab_binop G BVP.And e1 e2 = BV.Binop(BV.And, elab_expr G e1, elab_expr G e2)
    | elab_binop G BVP.Or e1 e2 = BV.Binop(BV.Or, elab_expr G e1, elab_expr G e2)
    | elab_binop G BVP.Xor e1 e2 = BV.Binop(BV.Xor, elab_expr G e1, elab_expr G e2)
    | elab_binop G BVP.Plus e1 e2 = BV.Binop(BV.Plus, elab_expr G e1, elab_expr G e2)

  and elab_expr _ BVP.Zero = BV.Zero
    | elab_expr _ BVP.One = BV.One
    | elab_expr G (BVP.Id s) = BV.Id $ Symbol.look' G s
    | elab_expr G (BVP.Ifz (e, e1, e2)) = BV.Ifz(elab_expr G e, elab_expr G e1, elab_expr G e2)
    | elab_expr G (BVP.Fold(vec, init, b1, b2, step)) =
        let
          val b1' = Symbol.look' G b1 handle _ => getIndex()
          val b2' = Symbol.look' G b2 handle _ => getIndex()
          val G' = Symbol.bind G (b1, b1')
          val G' = Symbol.bind G' (b2, b2')
        in
          BV.Fold(elab_expr G vec, elab_expr G init, b1', b2', elab_expr G' step)
        end
    | elab_expr G (BVP.Unop (oper, e)) = elab_unop G oper e
    | elab_expr G (BVP.Binop (oper, e1, e2)) = elab_binop G oper e1 e2

  fun elab (BVP.Lambda (s, e)) =
    let
      val _ = nextIndex := 0
      val firstIndex = getIndex()
      val G = Symbol.bind Symbol.empty (s, firstIndex)
    in
      BV.Lambda (firstIndex, elab_expr G e)
    end
end
