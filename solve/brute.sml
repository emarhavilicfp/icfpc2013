(* brute force solver *)
structure Brute =
struct
  open Haskell;
  open BV;

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

  (* TODO: Later. Possible optimization. Separate out the types of ops into
   * separate lists at the top level, rather than at every nobe. *)
  fun add_unop ops e =
    let
      fun allowed (O_Unop x) = SOME x
        | allowed _ = NONE
    in
      List.map (fn x => Unop (x,e)) $ List.mapPartial allowed ops
    end
  fun add_binop ops (e1,e2) =
    let
      fun allowed (O_Binop x) = SOME x
        | allowed _ = NONE
    in
      List.map (fn x => Binop (x,e1,e2)) $ List.mapPartial allowed ops
    end

  (* danger, combinatorics *)
  fun allpairs (xs: 'a list, ys: 'a list) : ('a * 'a) list =
    List.concat $ List.map (fn x => List.map (fn y => (x,y)) ys) xs

  fun alltriples (xs: 'a list, ys: 'a list, zs: 'a list) : ('a * 'a * 'a) list =
    List.concat $ List.concat $
      List.map (fn x => List.map (fn y => List.map (fn z => (x,y,z)) zs) ys) xs

  (* Associative map from list of variables in scope to all possible exprs.
   * The bool (part of the key) expresses whether a fold is allowed. *)
  type memo_table_slot = (Symbol.symbol list * bool * expr list) list
  (* Each slot in the memo table corresponds to a given size of expression.
   * Slots 0 and 1 are unused (I can't see it helping there!). *)
  type memo_table = memo_table_slot Array.array

  fun generate_expr (table : memo_table) do_fold vars ops 0 = []
    | generate_expr table do_fold vars ops 1 =
        Zero::One::(List.map (fn x => Id x) vars)
    | generate_expr table do_fold vars ops size =
      let
        fun sameset xs ys =
          List.all (fn x => List.exists (fn y => y=x) ys) xs andalso
          List.all (fn y => List.exists (fn x => x=y) xs) ys
        val _ = Assert.assert "negative size" $ size >= 0
        val _ = Assert.assert "table too small" $ size < Array.length table
        val slot = Array.sub (table, size)

        (* fallback function for cache miss *)
        fun generate_expr_miss () =
          let
            (* FIXME: thread this through to avoid regenerating *)
            (* FIXME: segregate this based on fold vs not fold *)
            val all_smaller_exprs =
              List.tabulate (size, fn x => generate_expr table do_fold vars ops x)

            (* Generate unary expressions. *)
            val unaries =
              List.concat $ List.map (add_unop ops) $
                List.nth (all_smaller_exprs, size-1)

            (* Generate binary expressions. *)
            (* TODO: Long-term: Can prune out duplciate Binop(e1,e2), Binop(e2,e1)
             * in case where the partition is the same size on both sides *)
            val all_smaller_pairs : (expr * expr) list =
              List.concat $ List.map allpairs $
                List.map (fn (x,y) => (List.nth (all_smaller_exprs,x),
                                       List.nth (all_smaller_exprs,y)))
                         (partition2 $ size-1)
            val binaries =
              (* note: partition2 emits [] if size < 3 *)
              List.concat $ List.map (add_binop ops) $ all_smaller_pairs

            (* Generate ternary expressions. *)
            (* TODO: As above todo in add_unops. Lift. *)
            val allowed_ifz = List.exists (fn x => x = O_Ifz) ops

            val ifzs = if not allowed_ifz then [] else
              let
                val all_smaller_trips : (expr * expr * expr) list =
                  List.concat $ List.map alltriples $
                    List.map (fn (x,y,z) => (List.nth (all_smaller_exprs,x),
                                             List.nth (all_smaller_exprs,y),
                                             List.nth (all_smaller_exprs,z)))
                             (partition3 $ size-1)
              in
                List.map Ifz all_smaller_trips
              end

            val folds = []
            val result = List.concat [unaries, binaries, ifzs, folds]
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
                         val nofolds = List.filter (not o contains_fold) exprs
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
      val top_x = Symbol.gensym ()
      val ops = #ops spec
      val size = #size spec
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
            val top_y = Symbol.gensym ()
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
      (List.length (generate {size = 3, ops = all_operators}) = 15),
    Assert.assert "generate 3 nofold"
      (List.length (generate {size = 3, ops = all_operators_nofold}) = 15),
    Assert.assert "generate 3 tfold"
      (List.length (generate {size = 3, ops = all_operators_tfold}) = 15),
    Assert.assert "generate 5"
      (List.length (generate {size = 5, ops = all_operators}) < 763),
    true
    ]
end
