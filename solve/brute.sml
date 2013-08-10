(* brute force solver *)
structure Brute =
struct
  open Haskell;
  open BV;

  exception Impossible

  (* partitions a size into sizes of two subexpressions, e.g.,
   * partition2 6 = [(1,5), (2,4), (3,3)]
   * ternops are *not* commutative, so we do need partition2 to flip, to *)
  fun partition2 n =
    let
      fun p' 0 = raise Fail "fu"
        | p' 1 = [(1, n-1)]
        | p' m = (m, n-m) :: (p' (m-1))
    in
      p' (n-1)
    end

  (* partitions a size into sizes of three subexpressions *)
  fun partition3 n =
    let
      fun v m = map (fn (a,b) => (m,a,b)) (partition2 (n - m))
      fun p' 0 = raise Fail "fu"
        | p' 1 = [v 1]
        | p' m = (v m) :: (p' (m-1))
    in
      List.concat (p' (n-2))
    end

  fun add_unop e = List.map (fn x => Unop (x,e)) [Not, Shl1, Shr1, Shr4, Shr16]
  fun add_binop (e1,e2) = List.map (fn x => Binop (x,e1,e2)) [And, Or, Xor, Plus]

  (* danger, combinatorics *)
  fun allpairs (xs: 'a list, ys: 'a list) : ('a * 'a) list =
    List.concat $ List.map (fn x => List.map (fn y => (x,y)) ys) xs

  fun generate_expr do_fold vars 0 = raise Impossible
    | generate_expr do_fold vars 1 = Zero::One::(List.map (fn x => Id x) vars)
    | generate_expr do_fold vars size =
      let
        (* FIXME: thread this through to avoid regenerating *)
        val all_smaller_exprs =
          List.tabulate (size, fn x => generate_expr do_fold vars x)
        val unaries =
          List.concat $ List.map add_unop $ List.nth (all_smaller_exprs, size-1)
        (* TODO: Long-term: Can prune out duplciate Binop(e1,e2), Binop(e2,e1)
         * in case where the partition is the same size on both sides *)
        (* FIXME XXX handle fold *)
        val all_smaller_pairs : (expr list * expr list) list =
          List.map (fn (x,y) => (List.nth (all_smaller_exprs,x),
                                 List.nth (all_smaller_exprs,y))) (partition3 $ size-1)
        val all_smaller_pairs' : (expr * expr) list =
          List.concat $ List.map allpairs $ all_smaller_pairs
        val binaries =
          (* note: partition2 emits [] if size < 3 *)
          List.concat $ List.map add_binop $ all_smaller_pairs'
        val ternaries =
          (* note: partition3 emits [] if size < 4 *)
          raise Impossible (* TODO *)
      in
        List.concat $ [unaries, binaries, ternaries]
      end

  fun generate (size, allowed_ops): Solver.spec -> program list = raise Impossible
end
