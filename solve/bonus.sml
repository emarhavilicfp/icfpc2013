(* Specialized bonus problem solver. *)
structure Bonus =
struct
  open BV;
  infixr 0 $
  fun f $ x = f x
  
  val log = Flags.log

  fun mapi f l = map f $ ListPair.zip (List.tabulate (length l, fn x => x), l);
  
  (* match_choice attempts to match a program from a pregenerated list to a
     list of correct outputs.  In particular, it returns a list of fs such that:
       if0 f(x)
       then g(x)
       else h(x)
     Note that the f(x) *includes* the "& 1"!
     
     It also returns a list of f's such that:
       if0 f'(x)
       then h(x)
       else g(x)
   *)
  fun match_choice values (g, h) ps =
    let
      val results =
        List.map (fn (q,a) =>
          ((Eval.eval g q) = a,
           (Eval.eval h q) = a))
          values
      val fmap =
        List.mapPartial
                     (* gcorr, hcorr *)
          (fn ((q, a), (true, true)) => NONE
            | ((q, a), (true, false)) => SOME (q, 0w1 : Word64.word)
            | ((q, a), (false, true)) => SOME (q, 0w0 : Word64.word)
            | ((q, a), (false, false)) => raise Fail "neither g nor h matched?"
          )
          (ListPair.zip (values, results))
      val f'map =
        List.map
          (fn (q, 0w0) => (q, 0w1 : Word64.word)
            | (q, 0w1) => (q, 0w0 : Word64.word)
            | _ => raise Fail "bad value in fmap?"
          )
          fmap
      val fs =
        List.filter
          (fn prog => List.all (fn (q, a) => (Eval.eval prog q) = a) fmap)
          ps
      val f's =
        List.filter
          (fn prog => List.all (fn (q, a) => (Eval.eval prog q) = a) f'map)
          ps
      val _ = log ("bonus: match_choice: matched "^(Int.toString $ length ps)^" programs and "^(Int.toString $ length values)^" test cases to "^(Int.toString $ length fs)^" fs and "^(Int.toString $ length f's)^" f's\n")
    in
      (fs, f's)
    end
  
  fun try_programs ps =
    (log ("bonus: try_results: about to send "^(Int.toString $ length ps)^" results up to the server\n");
     BruteSolve.server ps;
     true)
      handle BruteSolve.NoSolution =>
        (log ("bonus: try_results: no solution\n");
         false)

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
       * loop"; the inner loop just iterates over server counterexamples.
       * We do however save the biggest progs to not regen them later. *)
      val biggest_progs = Brute.generate {size=1+maxsize,ops=ops}
      val inputs = BruteSolve.get_inputs biggest_progs
      val pairs = ListPair.zip (inputs, ServerIO.eval inputs)
                    : (Word64.word * Word64.word) list
      (* Each g/h candidate will be paired with a bit wector that expresses how
       * much it agrees with the server's true function. *)
      fun give_wector (prog: BV.program) : (BV.program * BitVec.t) =
        let
          fun set_wector_bit ((input,output),(wector:BitVec.t,index:int)) =
            (* Set the bit if the candidate agrees with the server on this pair. *)
            (if Eval.eval prog input <> output then wector else
               BitVec.set (wector, index),
             index+1)
          val initial_wector = BitVec.new $ List.length pairs
          val (final_wector,_) = foldr set_wector_bit (initial_wector, 0) pairs
        in
          (prog, final_wector)
        end

      (**** Generate candidate programs. ****)

      (* We need to keep them in different size categories to optmz to avoid
       * trying g/h pairs with both of the maxsize, which would be too big.
       * This list is, for minsize=5 and maxsize=8, is [5,6,7,8]. *)
      val size_categories = List.tabulate (maxsize-minsize+1, fn x => minsize+x)
      (* Reuse the already-generated biggest programs if we can. *)
      fun generate_wector size = List.map give_wector
        (if size = maxsize then biggest_progs
         else Brute.generate {size=1+size,ops=ops})
      fun generate_and1 size =
        if size = maxsize then
          (* welp, I don't want to change the Brute.generate structure now, so. *)
          List.map (fn Lambda(x,e) => Lambda(x,Binop(And,e,One))) biggest_progs
        else Brute.generate {size=1+size,ops=ops}
      (* Note: 1+size for the enclosing lambda, not part of f/g/h. *)
      val ghs = List.map (fn x => generate_wector x) size_categories
      val fs  = List.map (fn x => generate_and1   x) size_categories

      (**** Find all pairs of g/h candidates. ****)

      (* Note that each slot in the list contains all programs of sizes less
       * than that slot's size, too, so we only pair up against one slot. *)
      fun find_pairs ((g as (gprog:BV.program,gwec:BitVec.t)), candidates)
            : ((BV.program * BV.program) * (int * int)) list =
        let
          val gsize = size gprog - 1 (* strip g's outer lambda *)
          val max_h_size = other_size gsize
          fun does_h_match ((hprog,hwec), candidates) =
            if BitVec.orFills(hwec,gwec) then
              let val hsize = size hprog - 1 (* likewise outer lambda *)
              in ((gprog,hprog),(gsize,hsize))::candidates
              end
            else candidates
        in
          (* Pick hs from the slot in the list that has programs no bigger
           * than the max allowable size for h given g's size. *)
          foldr does_h_match candidates $ List.nth (ghs, max_h_size-minsize)
        end
      (* The last slot has programs of all sizes.
       * Without loss of general fantasy, 'g' is not smaller than 'h'. *)
      val candidate_pairs = foldr find_pairs [] $ List.last ghs

      (**** Find matching segregators f for each g/h candidate pair. ****)

      fun find_segregators ((gh as (Lambda (xg,eg), Lambda (xh,eh)),(gsize,hsize)),
                            whole_progs) =
        let
          val _ = Assert.assert "xg xh aren't the same" $ xg = xh
          val max_f_size = segr_size gsize hsize
          val (matching_fs_gh, matching_fs_hg) =
                match_choice pairs gh $ List.nth (fs, max_f_size-minsize)
          (* Function to glue all 3 together, with 'g' in the true branch. *)
          fun make_fgh (Lambda(xf,ef)) =
            (Assert.assert "xf/xg/xh" (xf = xg); Lambda(xf, Ifz(ef,eg,eh)))
          (* Function to glue all 3 together, with 'h' in the true branch. *)
          fun make_fhg (Lambda(xf,ef)) =
            (Assert.assert "xf/xh/xg" (xf = xg); Lambda(xf, Ifz(ef,eh,eg)))
          (* Got here. *)
          val whole_progs_gh = List.map make_fgh matching_fs_gh
          val whole_progs_hg = List.map make_fgh matching_fs_gh
        in
          List.revAppend (whole_progs_gh, List.revAppend (whole_progs_hg, whole_progs))
        end

      val candidate_progs = foldr find_segregators [] candidate_pairs
    in
      (* Outer loop. Repeat with a laxer minsize if our estimate was too big. *)
      if try_programs candidate_progs then ()
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
