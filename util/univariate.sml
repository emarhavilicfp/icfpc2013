structure Univariate =
struct
  structure Set = BinarySetFn (struct
                    type ord_key = Word64.word
                    val compare = Word64.compare
                  end)

  type word = Word64.word
  type id = BV.id
  datatype binop = And | Or | Xor | Plus | Shl | Shr
  datatype expr = Const of word
                | Id of id
                | Not of expr
                | Binop of (binop * expr * expr)
                | Ifz of (expr * expr * expr)
  datatype prog = Lambda of (id * expr)

  val w64w32 = Word32.fromLargeInt o Word64.toLargeInt

  fun cfold_expr (Id id) = Id id
    | cfold_expr (Const c) = Const c
    | cfold_expr (Not e) = let
          val ce = cfold_expr e
        in
          (
            case ce
              of (Const c) => Const (Word64.notb c)
               | _ => ce
          )
        end
    | cfold_expr (Binop (b, l, r)) = let
          val cl = cfold_expr l
          val cr = cfold_expr r
          val not0 = Word64.notb 0w0
          fun cfold_binop (And, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Word64.andb (l, r))
                     | (Const 0w0, _) => Const 0w0
                     | (_, Const 0w0) => Const 0w0
                     | _ => Binop (And, l, r)
                )
            | cfold_binop (Or, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Word64.orb (l, r))
                     | (Const not0, _) => Const not0
                     | (_, Const not0) => Const not0
                     | _ => Binop (Or, l, r)
               )
            | cfold_binop (Xor, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Word64.xorb (l, r))
                     | (Const 0w0, _) => r
                     | (Const cl, _) => if cl = not0 then (Not r) else Binop (Xor, l, r)
                     | (_, Const 0w0) => l
                     | (_, Const cr) => if cr = not0 then (Not l) else Binop (Xor, l, r)
                     | _ => Binop (Xor, l, r)
               )
            | cfold_binop (Plus, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Word64.+ (l, r))
                     | (Const 0w0, _) => r
                     | (_, Const 0w0) => l
                     | _ => Binop (Plus, l, r)
               )
            | cfold_binop (Shl, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Suq.Word64.<< (l, w64w32 r))
                     | (Const 0w0, _) => Const 0w0
                     | (_, Const 0w0) => l
                     | (_, Const c) => if Word64.>= (c, 0w64) then Const 0w0 else l
                     | _ => Binop (Shl, l, r)
               )
            | cfold_binop (Shr, l, r) = (
                  case (l, r)
                    of (Const l, Const r) => Const (Suq.Word64.>> (l, w64w32 r))
                     | (Const 0w0, _) => Const 0w0
                     | (_, Const 0w0) => l
                     | _ => Binop (Shr, l, r)
               )
        in
          cfold_binop (b, cl, cr)
        end
    | cfold_expr (Ifz (e, t, f)) = let
          val ce = cfold_expr e
          val ct = cfold_expr t
          val cf = cfold_expr f
        in
          (
            case ce
              of (Const 0w0) => ct
               | (Const _) => cf
               | _ => Ifz (ce, ct, cf)
          )
        end

  fun trans_prog (BV.Lambda (x, expr)) = let
      fun trans_expr BV.Zero = Const 0w0
        | trans_expr BV.One = Const 0w1
        | trans_expr (BV.Id id) = Id id
        | trans_expr (BV.Ifz (e, t, f)) =
            Ifz (trans_expr e, trans_expr t, trans_expr f)
        | trans_expr (BV.Unop (u, e)) = (let
            fun trans_unop_expr BV.Not = (fn e => Not e)
              | trans_unop_expr BV.Shl1 = (fn e => Binop (Shl, e, Const 0w1))
              | trans_unop_expr BV.Shr1 = (fn e => Binop (Shr, e, Const 0w1))
              | trans_unop_expr BV.Shr4 = (fn e => Binop (Shr, e, Const 0w4))
              | trans_unop_expr BV.Shr16 = (fn e => Binop (Shr, e, Const 0w16))
          in
            (trans_unop_expr u) (trans_expr e)
          end)
        | trans_expr (BV.Binop (b, l, r)) = (let
              fun trans_binop BV.And = And
                | trans_binop BV.Or = Or
                | trans_binop BV.Xor = Xor
                | trans_binop BV.Plus = Plus
            in
              Binop (trans_binop b, trans_expr l, trans_expr r)
            end)
        | trans_expr (BV.Fold (v, v0, y, z, e)) = (let
            fun remap (id, e', (Id id')) =
                  if id = id' then e' else (Id id')
              | remap (id, e', Not e) =
                  Not (remap (id, e', e))
              | remap (id, e', Binop (b, l, r)) =
                  Binop (b, remap (id, e', l), remap (id, e', r))
              | remap (id, e', Ifz (e, t, f)) =
                  Ifz (remap (id, e', e), remap (id, e', t), remap (id, e', f))
              | remap (_, _, e) = e
            val e = trans_expr e
            val v = trans_expr v
            val v0 = trans_expr v0
            val v1 = remap (y, Binop (And, Binop (Shr, v, Const 0w0), Const 0w255), remap(z, v0, e))
            val v2 = remap (y, Binop (And, Binop (Shr, v, Const 0w8), Const 0w255), remap(z, v1, e))
            val v3 = remap (y, Binop (And, Binop (Shr, v, Const 0w16), Const 0w255), remap(z, v2, e))
            val v4 = remap (y, Binop (And, Binop (Shr, v, Const 0w24), Const 0w255), remap(z, v3, e))
            val v5 = remap (y, Binop (And, Binop (Shr, v, Const 0w32), Const 0w255), remap(z, v4, e))
            val v6 = remap (y, Binop (And, Binop (Shr, v, Const 0w40), Const 0w255), remap(z, v5, e))
            val v7 = remap (y, Binop (And, Binop (Shr, v, Const 0w48), Const 0w255), remap(z, v6, e))
            val v8 = remap (y, Binop (And, Binop (Shr, v, Const 0w56), Const 0w255), remap(z, v7, e))
          in
            v8
          end)
    in
      Lambda (x, cfold_expr(trans_expr(expr)))
    end

  fun target (prog, result) = let
  in
    ()
  end

  fun test () = let
  in
    List.all (fn x => x) [
      true
    ]
  end

end
