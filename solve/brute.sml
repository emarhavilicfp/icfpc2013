(* brute force solver *)
structure Brute =
struct
  open BV;
  infixr 0 $
  fun f $ x = f x

  exception Impossible

  (* partitions a size into sizes of two subexpressions, e.g.,
   * partition2 6 = [(1,5), (2,4), (3,3)]
   * ternops are *not* commutative, so we do need partition2 to flip, to *)
  fun partition2 size =
   let
     fun partition2' n max =
       if max-n < n then []
       else (n,max-n)::(partition2' (n+1) max)
   in
     partition2' 1 size
   end

  (* partitions a size into sizes of three subexpressions *)
  fun partition3 0 = []
    | partition3 1 = []
    | partition3 2 = []
    | partition3 (n: int) : (int*int*int) list =
    let
      fun partition2 0 = []
        | partition2 1 = []
        | partition2 (n: int) : (int*int) list =
        let
          fun p' 0 = raise Fail "fu"
            | p' 1 = [(1, n-1)]
            | p' m = (m, n-m) :: (p' (m-1))
        in
          p' (n-1)
        end
      fun v m = map (fn (a,b) => (m,a,b)) (partition2 (n - m))
      fun p' 0 = raise Fail "fu"
        | p' 1 = [v 1]
        | p' m = (v m) :: (p' (m-1))
    in
      List.concat (p' (n-2))
    end

  fun eq e1 e2 = e1 = e2 orelse
        (constexpr e1 andalso constexpr e2 andalso
         Eval.eval_constexpr e1 = Eval.eval_constexpr e2)

  (* TODO: Later. Possible optimization. Separate out the types of ops into
   * separate lists at the top level, rather than at every nobe. *)
  fun add_unop ops e =
    let
      fun allowed (O_Unop x) = SOME x
        | allowed _ = NONE
      (* Same as maybe_add_op in add_binop below. "Zero" is the canonical zero. *)
      fun add_op_unless_id x = (case e of Zero => NONE | _ => SOME $ Unop(x,e))
      fun maybe_add_op Shl1  = add_op_unless_id Shl1
        | maybe_add_op Shr1  =
            (case e of One => NONE (* 1>>1 = 0 *)
                     | (Unop (Shr1, Unop (Shr1, Unop (Shr1, _)))) =>
                         (* shr4 -- only if the op is in the set! *)
                         if (List.exists (fn x => x = O_Unop Shr4) ops) then NONE
                         else add_op_unless_id Shr1
                     | _ => add_op_unless_id Shr1)
        | maybe_add_op Shr4  =
            (case e of One => NONE (* 1>>4 = 0 *)
                     | (Unop (Shr4, Unop (Shr4, Unop (Shr4, _)))) =>
                         (* shr16 -- only if the op is in the set! *)
                         if (List.exists (fn x => x = O_Unop Shr16) ops) then NONE
                         else add_op_unless_id Shr4
                     | _ => add_op_unless_id Shr4)
        | maybe_add_op Shr16 =
            (case e of One => NONE (* 1>>16 = 0 *)
                     | (Unop (Shr16, Unop (Shr16, Unop (Shr16, _)))) => NONE (* 0! *)
                     | _ => add_op_unless_id Shr16)
        | maybe_add_op Not =
            (* classico logic *)
            (case e of (Unop (Not, _)) => NONE | _ => SOME $ Unop(Not,e))
    in
      List.mapPartial maybe_add_op $ List.mapPartial allowed ops
    end
  fun add_binop ops (e1,e2) =
    let
      fun allowed (O_Binop x) = SOME x
        | allowed _ = NONE
      (* Some peephole optimizations. For example, we don't emit "identity"
       * binary expressions because the expression itself will already have been
       * emitted in the smaller list. Similarly we don't emit expressions that
       * we know evaluate to zero or one, or can be expressed a different way. *)
      fun add_op_unless_id x =
        (case (e1,e2) of (Zero,Zero) => NONE | (Zero,_) => NONE | (_,Zero) => NONE
                       | _ => SOME $ Binop(x,e1,e2))
      fun maybe_add_op Plus =
            (* plus e e = e<<1 -- only if the op is in the set! *)
            if eq e1 e2 andalso (List.exists (fn x => x = O_Unop Shl1) ops) then NONE
            else add_op_unless_id Plus
        | maybe_add_op x =
            if eq e1 e2 then NONE (* xor e e = 0, or e e = e, and e e = e *)
            (* bitwise binops are distributive across unary binops, e.g.
             * xor(not x, not y) = not(xor(x,y)), and the latter is 1 nobe smaller *)
            else case (e1,e2) of (Unop(o1,_), Unop(o2,_)) =>
                                      if o1 = o2 then NONE
                                      else add_op_unless_id x
                               | _ => add_op_unless_id x
    in
      List.mapPartial maybe_add_op $ List.mapPartial allowed ops
    end

  fun optmz_associative_binops x = x
  (* This is O(n^2) in the length of list of expressions. Too expensive.
  fun optmz_associative_binops [] = []
      (* canonicalize: op ((op x2 x3) x1) => op (x1 (op x2 x3)) *)
    | optmz_associative_binops ((Binop (op1, e0 as Binop (op2,e2,e3), e1))::rest) =
        optmz_associative_binops ((Binop (op1, e1, e0))::rest)
    | optmz_associative_binops ((e as (Binop (op1,e1, Binop (op2,e2,e3))))::rest) =
        if op1 <> op2 then e::(optmz_associative_binops rest)
        else
          let
            fun not_same (Binop (op1, e0 as Binop (op2,e2,e3), e1)) =
                  not_same (Binop (op1, e1, e0))
              | not_same (Binop (op1',x, Binop (op2',y,z))) =
                  op1' <> op1 orelse op2' <> op1 orelse
                  (List.all (fn (x,y,z) => Binop (op1, x, Binop (op2, y, z)) <> e)
                     [(z,x,y), (y,z,x), (z,y,x), (y,x,z), (x,z,y)])
              | not_same _ = true
          in
            e::(optmz_associative_binops $ List.filter not_same rest)
          end
    | optmz_associative_binops ((e as Binop _)::rest) =
        e::(optmz_associative_binops rest)
    | optmz_associative_binops _ = raise Fail "non binop in binop list"
   *)

  (* danger, combinatorics *)
  fun allpairs (xs: 'a list, ys: 'a list) : ('a * 'a) list =
    List.concat $ List.map (fn x => List.map (fn y => (x,y)) ys) xs
  fun allpairs_samesize [] = []
    | allpairs_samesize ((x: expr)::rest) =
        (x,x)::(List.map (fn y => (x,y)) rest)@(allpairs_samesize rest)

  fun alltriples (xs: 'a list, ys: 'a list, zs: 'a list) : ('a * 'a * 'a) list =
    List.concat $ List.concat $
      List.map (fn x => List.map (fn y => List.map (fn z => (x,y,z)) zs) ys) xs

  (* Associative map from list of variables in scope to all possible exprs.
   * The bool (part of the key) expresses whether a fold is allowed. *)
  type memo_table_slot = (id list * bool * expr list) list
  (* Each slot in the memo table corresponds to a given size of expression.
   * Slots 0 and 1 are unused (I can't see it helping there!). *)
  type memo_table = memo_table_slot Array.array

  fun generate_expr (table : memo_table) do_fold vars ops 0 = []
    | generate_expr table do_fold vars ops 1 =
        Zero::One::(List.map (fn x => Id x) vars)
    | generate_expr table do_fold vars ops size =
      let
        fun sameset (xs: id list) (ys: id list) =
          List.all (fn x => List.exists (fn y => y=x) ys) xs andalso
          List.all (fn y => List.exists (fn x => x=y) xs) ys
        val _ = Assert.assert "negative size" $ size >= 0
        val _ = Assert.assert "table too small" $ size < Array.length table
        val slot = Array.sub (table, size)

        (* fallback function for cache miss *)
        fun generate_expr_miss () =
          let
            fun smaller_exprs n = generate_expr table do_fold vars ops n
            fun smaller_exprs_fold newvars n = generate_expr table false newvars ops n

            (* Generate unary expressions. *)
            val unaries =
                List.concat $ List.map (add_unop ops) $ smaller_exprs $ size-1

            (* Generate binary expressions. *)
            val all_smaller_pairs : (expr * expr) list =
              List.concat $
                List.map (fn (x,y) =>
                            if x=y then
                              allpairs_samesize $ smaller_exprs x
                            else
                              allpairs (smaller_exprs x, smaller_exprs y))
                         (partition2 $ size-1)
            (* Optimize associative binops here. *)
            val binaries = optmz_associative_binops $
              (* note: partition2 emits [] if size < 3 *)
              List.concat $ List.map (add_binop ops) all_smaller_pairs

            (* Generate ternary expressions. *)
            (* TODO: As far-above todo in add_unops. Lift. *)
            val allowed_ifz = List.exists (fn x => x = O_Ifz) ops

            val ifzs = if not allowed_ifz then [] else
              let
                val all_smaller_trips : (expr * expr * expr) list =
                  List.concat $ List.map alltriples $
                    List.map (fn (x,y,z) =>
                                (smaller_exprs x, smaller_exprs y, smaller_exprs z))
                             (partition3 $ size-1)
                fun nonconstant_ifz (Binop(Plus,_,One), _, _) = false
                  | nonconstant_ifz (Binop(Plus,One,_), _, _) = false
                  | nonconstant_ifz (e0 : expr, e1 : expr, e2 : expr) =
                      (not $ constexpr e0) andalso (not $ eq e1 e2) andalso
                      (not (e1 = Zero andalso eq e0 e2)) (* if e=0 then 0 else e *)
              in
                List.map Ifz $ List.filter nonconstant_ifz all_smaller_trips
              end

            val folds = if not do_fold then [] else
              let
                val elt_var = 1
                val acc_var = 2
                val newvars = elt_var::acc_var::vars
                val all_smaller_trips : (expr * expr * expr) list =
                  List.concat $ List.map alltriples $
                    List.map (fn (x,y,z) => (smaller_exprs_fold vars x,
                                             smaller_exprs_fold vars y,
                                             smaller_exprs_fold newvars z))
                             (partition3 $ size-2) (* NOTE fold has size 2!! *)
                (* If the inner e doesn't refer to either fold variable, the whole
                 * fold is equivalent to that e. *)
                fun nonconstant_fold (e0, _, e) =
                  (not $ check_freevars_expr vars e)
                    (* identity accumulation *)
                    andalso e <> Id acc_var andalso e <> Unop (Not, Id acc_var)
                    (* will always accumulate to zero *)
                    (* here, shr1 and shr4 are also redundant with two greater,
                     * shrs but require the other op to do without a fold *)
                    andalso e <> Unop (Shr16, Id acc_var)
                    andalso e <> Unop (Shr4, Unop (Shr4, Id acc_var))
                    andalso e <> Unop (Shr16, Id elt_var) (* elt < 256, so... *)
                    andalso e <> Unop (Shr4, Unop (Shr4, Id elt_var))
                    (* If the base is constant and has the top byte all zeroes,
                     * then the accumulator must be used. *)
                    andalso not
                      (constexpr e0 andalso
                       Suq.Word64.>> (Eval.eval_constexpr e0, 0w56) = 0w0 andalso
                       check_freevars_expr (elt_var::vars) e (* i.e. acc_var unused *))
              in
                List.map (fn (e0,e1,e2) => Fold (e0,e1,elt_var,acc_var,e2)) $
                         List.filter nonconstant_fold all_smaller_trips
              end

            val smaller_progs =
              List.concat $ List.tabulate (size, fn n => smaller_exprs n)

            val result = List.concat [folds, ifzs, binaries, unaries, smaller_progs]
          in
            Array.update (table, size, (vars, do_fold, result)::slot); result
          end
      in
        (* Consult memo table. *)
        case (List.find (fn (ids,fold,_) => sameset vars ids andalso
                                            do_fold = fold) slot,
              do_fold)
          of
             (* Direct hit. *)
             (SOME(_,_,exprs),_) => exprs (* (print "hit!\n"; exprs) *)
             (* Miss, but maybe partial hit? (If !do_fold, but was already
              * computed for fold, we can filter out the foldful ones. *)
           | (NONE, false) =>
               (case List.find (fn (ids,_,_) => sameset vars ids) slot of
                     SOME(_,false,exprs) => raise Fail "not possible"
                   | SOME(_,true,exprs) =>
                       let
                         val nofolds = List.filter (not o contains_fold_expr) exprs
                         val newslot = (vars, false, nofolds)::slot
                       in
                         Array.update (table, size, newslot); nofolds
                       end
                     (* Miss. *)
                   |  NONE => generate_expr_miss ())
             (* Miss. (Need fold, partial hit not possible.) *)
           | (NONE, true) => generate_expr_miss ()
      end

  fun generate (spec: Solver.spec) : program list =
    let
      val top_x = 0
      val ops = #ops spec
      val size = #size spec
      val _ = Assert.assert "give me a positive program size you CLOWN" $ size > 0
      val table = Array.array (size, [])
      val tfold_overhead = 1 (*outer lambda*) + 2 (*fold*) + 1 (*x*) + 1 (*zero*)
      val tfold_specified = (List.exists (fn x => x = O_Tfold) ops)
      val fold_specified  = (List.exists (fn x => x = O_Fold) ops)
      val exprs =
        if tfold_specified andalso size - tfold_overhead >= 1 then
          let
            val _ = Assert.assert "prohibited both folds" $ not $ fold_specified
            (* A Tfold is defined to always be at the top level, always have 0
             * as the accumulator, and always shadow the top 'x' with its own. *)
            val top_y = 1
            val inner_size = size - (1 (*lambda*) + 2 (*fold*) + 1 (*x*) + 1 (*0*))
          in
            List.map (fn expr => Fold (Id top_x, Zero, top_x, top_y, expr)) $
              generate_expr table false [top_x, top_y] ops (size-tfold_overhead)
          end
        else
          let
            val do_fold = (List.exists (fn x => x = O_Fold) ops)
          in
            generate_expr table do_fold [top_x] ops (size-1)
          end
    in
      List.map (fn expr => Lambda (top_x, expr)) exprs
    end


  (* tests *)

  fun test () = List.all (fn x => x) [
    Assert.assert "partition2 2" (partition2 2 = [(1,1)]),
    Assert.assert "partition2 3" (List.length (partition2 3) = 1),
    Assert.assert "partition2 4" (List.length (partition2 4) = 2),
    Assert.assert "partition2 5" (List.length (partition2 5) = 2),
    Assert.assert "partition2 6" (List.length (partition2 6) = 3),
    Assert.assert "partition2 10" (List.length (partition2 10) = 5),
    Assert.assert "partition3 3" (partition3 3 = [(1,1,1)]),
    Assert.assert "partition3 4" (List.length (partition3 4) = 3),
    Assert.assert "generate 2" (List.length (generate {size = 2, ops = []}) = 3),
    Assert.assert "generate 3"
      (List.length (generate {size = 3, ops = all_operators}) <= 11),
    Assert.assert "generate 3 nofold"
      (List.length (generate {size = 3, ops = all_operators_nofold}) <= 11),
    Assert.assert "generate 3 tfold"
      (List.length (generate {size = 3, ops = all_operators_tfold}) <= 11),
    Assert.assert "generate 5"
      (List.length (generate {size = 5, ops = all_operators}) < 534),
    Assert.assert "generate 6 fold doesn't escape" $
      List.all check_freevars $ List.filter contains_fold $
        generate {size = 6, ops = all_operators},
    Assert.assert "benchmarks!!1!!1!11" $
      List.all (fn size =>
        let
          val len = List.length $ generate {size = size, ops = all_operators}
        in
          Assert.say_yellow ("There are " ^ (Int.toString len) ^
                             " programs of length " ^ (Int.toString size));
          true
        end) [1,2,3,4,5,6,7,8],
    true
    ]
end
