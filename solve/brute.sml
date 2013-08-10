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

  fun generate_expr do_fold vars ops 0 = []
    | generate_expr do_fold vars ops 1 = Zero::One::(List.map (fn x => Id x) vars)
    | generate_expr do_fold vars ops size =
      let
        (* FIXME: thread this through to avoid regenerating *)
        val all_smaller_exprs =
          List.tabulate (size, fn x => generate_expr do_fold vars ops x)

        (* Generate unary expressions. *)
        val unaries =
          List.concat $ List.map (add_unop ops) $ List.nth (all_smaller_exprs, size-1)

        (* Generate binary expressions. *)
        (* TODO: Long-term: Can prune out duplciate Binop(e1,e2), Binop(e2,e1)
         * in case where the partition is the same size on both sides *)
        (* FIXME XXX handle fold *)
        val all_smaller_pairs : (expr list * expr list) list =
          List.map (fn (x,y) => (List.nth (all_smaller_exprs,x),
                                 List.nth (all_smaller_exprs,y))) (partition2 $ size-1)
        val all_smaller_pairs' : (expr * expr) list =
          List.concat $ List.map allpairs $ all_smaller_pairs
        val binaries =
          (* note: partition2 emits [] if size < 3 *)
          List.concat $ List.map (add_binop ops) $ all_smaller_pairs'

        (* Generate ternary expressions. *)
        val ternaries =
          (* note: partition3 emits [] if size < 4 *)
          [] (* TODO *)
      in
        List.concat $ [unaries, binaries, ternaries]
      end

  fun generate (spec: Solver.spec) : program list =
    let
      val top_x = Symbol.gensym()
      val exprs = generate_expr true [top_x] (#ops spec) (#size spec - 1)
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
    Assert.assert "generate 2" (List.length (generate {size = 2, ops = []}) =
    3),
    true
    ]
end
