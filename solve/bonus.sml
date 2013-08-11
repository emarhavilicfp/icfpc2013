(* Specialized bonus problem solver. *)
structure Bonus =
struct
  open BV;
  infixr 0 $
  fun f $ x = f x

  fun findPairs [] = []
    | findPairs (x::L) =
    let
      fun outerLoop curThing [] curAcc = curAcc
        | outerLoop curThing (L as (x::xs)) curAcc = 
          let
            fun inner (t, acc) = if BitVec.orFills(curThing, t) then 
                (curThing, t)::acc
              else acc
            val newAcc = foldr inner curAcc L
          in
            outerLoop x xs newAcc
          end
    in
      outerLoop x L []
    end

  val minsize = 5 (* Believed minimum size of f, g, or h. *)

  fun solve 0 _ = raise Fail "Minsize became too min. We suck. :("
    | solve minsize (spec: Solver.spec) : unit =
    let
      val ops = #ops spec
      val _ = Assert.assert "not a bonus prob" $ List.exists (fn x => x = O_Bonus) ops
      val _ = Assert.assert "minsize way too big" $ minsize < 10

      (**** Reason about sizes of f, g, and h. ****)

      val fgh_size = #size spec - 4 (* lambda + ifz + and + 1 *)
      val _ = Assert.assert "size too small" $ fgh_size >= 3
      (* ???? should never be lower *)
      val minsize =
        let val x = fgh_size div 3 in if x < minsize then x else minsize end
      val maxsize = fgh_size - (2 * minsize) (* e,g, 18 = 5 5 and 8 *)
      (* If g is size x what's the biggest h can be, assuming f is minsize? *)
      fun other_size x = fgh_size-x-minsize
      fun segr_size gsize hsize = fgh_size-gsize-hsize (* self-explanatory *)

      (**** Get server input/output pairs. ****)

      (* Note that we just use the max size because Brute.generate will
       * generate all the smaller programs too. This is not inside the "inner
       * loop"; the inner loop just iterates over server counterexamples. *)
      val inputs = BruteSolve.get_inputs $ Brute.generate {size=1+maxsize,ops=ops}
      val pairs = ListPair.zip (inputs, ServerIO.eval inputs)
                    : (Word64.word * Word64.word) list
      (* Each g/h candidate will be paired with a bit wector that expresses how
       * much it agrees with the server's true function. *)
      fun give_wector prog =
        let
          fun set_wector_bit ((input,output),(wector,index)) =
            (* Set the bit if the candidate agrees with the server on this pair. *)
            (if Eval.eval prog input <> output then wector else
               BitVec.set (wector, index),
             index+1)
        in
          (prog, foldr set_wector_bit (BitVec.new $ List.length pairs, 0) pairs)
        end

      (**** Generate candidates. ****)

      (* We need to keep them in different size categories to optmz to avoid
       * trying g/h pairs with both of the maxsize, which would be too big.
       * This list is, for minsize=5 and maxsize=8, is [5,6,7,8]. *)
      val size_cats = List.tabulate (maxsize-minsize+1, fn x => minsize+x)
      (* Note: 1+size for the enclosing lambda, not part of f/g/h. *)
      val ghs = List.map (fn x =>
                  List.map give_wector $ Brute.generate {size=1+x,ops=ops}) size_cats
      val fs  = List.map (fn x => Brute.generate_and1   {size=1+x,ops=ops}) size_cats

      (* Inner loop. Repeat finding candidate g/h pairs (with f segregators),
       * and getting counterexamples from the server, until we get it, or we run
       * out (in which case we are left to assume our minsize was too big). *)
      fun solve_space ghs =
        let
          val shit = ()
        in
          raise Fail "unimplemented"
        end
    in
      (* Outer loop. Repeat with a laxer minsize if our estimate was too big. *)
      if solve_space ghs then ()
      else (Flags.log ("Minsize " ^ Int.toString minsize ^ " not min enough.");
            solve (minsize-1) spec)
    end

  val solver : Solver.solver =
    { shortname = "bonus", name = "bonus problem solver", f = solve minsize }


  (* Not really tests, but. *)

  fun test() = List.all (fn x => x) [
    let 
      fun split_ifzs (Lambda(_,Ifz(Binop (And,f,One),g,h))) = (f,g,h)
        | split_ifzs (Lambda(_,Ifz(_,_,_))) = raise Fail "non-and1 bonus"
        | split_ifzs _ = raise Fail "non-ifz bonus"

      val progs = List.map (split_ifzs o TestUtil.Parse) [
        "(lambda (x_5) (if0 (and (and 1 (shr16 (shr16 x_5))) 1) (plus (shr4 (shr16 x_5)) 1) (and (shr1 x_5) (and (shl1 x_5) x_5))))", 
        "(lambda (x_7) (if0 (and (or x_7 (and (not (shr4 x_7)) 1)) 1) (xor x_7 (shr4 (not x_7))) (plus (shl1 (not x_7)) (not 0))))", 
        "(lambda (x_1) (if0 (and (xor x_1 (shr16 (shr4 (shr16 (shr16 x_1))))) 1) (plus (shr16 (shr16 x_1)) (plus x_1 1))  (if0 (shr4 x_1) 1 (xor x_1 1))))", 
        "(lambda (x_11) (if0 (and (not (shr16 (not (shr1 x_11)))) 1) (and (plus (plus 1 x_11) x_11) x_11) (plus x_11 (not (shr1 x_11)))))", 
        "(lambda (x_9) (if0 (and (plus (shr16 (shr16 (shl1 x_9))) (shr16 x_9)) 1) (plus x_9 (shl1 (and (shr4 x_9) x_9))) (plus 1 (plus x_9 x_9))))", 
        "(lambda (x_4) (if0 (and (and (not (shr1 x_4)) (shr16 (not 0))) 1) (xor (plus 1 x_4) 1) (or (plus x_4 (not (shr16 x_4))) x_4)))", 
        "(lambda (x_4) (if0 (and (and (shr1 (not x_4)) x_4) 1) (and x_4 (plus x_4 1))  (and (shr4 x_4) (plus (not x_4) (not 0)))))", 
        "(lambda (x_20) (if0 (and (or x_20 (and (not (shr4 x_20)) 1)) 1) (not (or x_20 (shr1 x_20))) (plus x_20 (shl1 (and (shl1 x_20) x_20)))))", 
        "(lambda (x_7) (if0 (and (xor x_7 (plus (shr16 (not x_7)) x_7)) 1) (plus (not 1) (if0 x_7 1 x_7)) (plus (or (not x_7) (shr4 x_7)) x_7)))", 
        "(lambda (x_8) (if0 (and (and 1 (not (xor (shr4 x_8) x_8))) 1) (and (shr4 (shr16 x_8)) x_8) (plus (shr4 (shr4 (shr16 (shr1 x_8)))) x_8)))", 
        "(lambda (x_20) (if0 (and (plus x_20 (shl1 (and (shr4 x_20) x_20))) 1) (xor (plus (plus x_20 1) 1) 1) (and (plus 1 (not (shr16 x_20))) x_20)))", 
        "(lambda (x_7) (if0 (and (xor (or (shr4 (shr16 x_7)) x_7) x_7) 1) (xor x_7 (shr4 (not x_7))) (xor (shr16 (shr4 x_7)) (plus 1 x_7))))", 
        "(lambda (x_16) (if0 (and (plus x_16 (plus (shr4 (shr4 x_16)) 1)) 1) (shl1 (plus 1 (shr1 x_16))) (plus (not 1) (if0 x_16 1 x_16))))", 
        "(lambda (x_15) (if0 (and (or (not 1) (if0 x_15 1 x_15)) 1) (xor (plus x_15 1) 1) (and x_15 (plus x_15 x_15))))", 
        "(lambda (x_11) (if0 (and (or (not 1) (if0 x_11 1 x_11)) 1) (plus x_11 (shr4 (shr16 (shr4 (shr4 x_11))))) (shl1 (shr16 (shr16 (not x_11))))))  "]

    in
      List.map (fn (f,g,h) => Assert.say_yellow $
          "|f| = " ^ Int.toString (size_expr f) ^
        "; |g| = " ^ Int.toString (size_expr g) ^
        "; |h| = " ^ Int.toString (size_expr h))
        progs;
      true
    end,
    true]
end
