(* brute force solver *)
structure Brute =
struct
  open Haskell;
  open BV;

  exception Impossible

  (* partitions a size into sizes of two subexpressions, e.g.,
   * partition2 6 = [(1,5), (2,4), (3,3)]
   * NB. does not do the flipped versions because all binops are commutative *)
  fun partition2 size = 
   let
     fun partition2' n max =
       if max-n < n then []
       else (n,max-n)::(partition2' (n+1) max)
   in
     partition2' 1 size
   end
        
  (* partitions a size into sizes of two subexpressions, e.g.,
   * partition3 6 = [(1,5), (2,4), (3,3)] *)
  fun partition3 _ = raise Impossible (* TODO *)


  fun add_unop e = List.map (fn x => Unop (x,e)) [Not, Shl1, Shr1, Shr4, Shr16]
  fun add_binop e1 e2 = List.map (fn x => Binop (x,e1,e2)) [And, Or, Xor, Plus]

  fun generate_expr do_fold vars 0 = raise Impossible
    | generate_expr do_fold vars 1 = Zero::One::(List.map (fn x => Id x) vars)
    | generate_expr do_fold vars size =
      let
        (* TODO: thread this through to avoid regenerating *)
        val all_smaller_exprs =
          List.tabulate (size, fn x => generate_expr do_fold vars x)
        val unaries =
          List.concat $ List.map add_unop $ List.nth (all_smaller_exprs, size-1)
        val binaries =
          (* note: partition2 emits [] if size < 3 *)
          raise Impossible (* TODO *)
        val ternaries =
          (* note: partition3 emits [] if size < 4 *)
          raise Impossible (* TODO *)
      in
        List.concat $ [unaries, binaries, ternaries]
      end

  fun generate size = raise Impossible
end
