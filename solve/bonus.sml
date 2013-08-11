(* Specialized bonus problem solver. *)
structure Bonus =
struct
  open BV;

  val log = Flags.log
  val Sd = Int.toString
  fun Sdl (a : 'a list) = Int.toString (length a)

  fun mapi f l = map f (ListPair.zip (List.tabulate (length l, fn x => x), l));

  fun revappend ([],ys) = ys
    | revappend ((x::xs),ys) = revappend (xs,(x::ys))

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
  fun match_choice values (g, h) (ps : (BV.program * ((Word64.word * Word64.word) list)) list) =
    let
      val results =
        List.map (fn (q,a) =>
          ((Eval.eval g q) = a,
           (Eval.eval h q) = a))
          values
      val fmap : Word64.word option list =
        List.map
                     (* gcorr, hcorr *)
          (fn ((q, a), (true, true)) => NONE
            | ((q, a), (true, false)) => SOME (0w0 : Word64.word)
            | ((q, a), (false, true)) => SOME (0w1 : Word64.word)
            | ((q, a), (false, false)) => raise Fail "neither g nor h matched?"
          )
          (ListPair.zip (values, results))
      val f'map : Word64.word option list =
        List.map
          (fn SOME (0w0) => SOME (0w1 : Word64.word)
            | SOME (0w1) => SOME (0w0 : Word64.word)
            | SOME (_) => raise Fail "shit"
            | NONE => NONE
          )
          fmap
      fun satisfies (SOME(a),(_,a2)) = a = a2
        | satisfies (NONE, _) = true
      val fs : BV.program list =
        List.mapPartial
          (fn (prog, results) =>
            if ListPair.all satisfies (fmap, results) then SOME prog else NONE)
          ps
      val f's : BV.program list =
        List.mapPartial
          (fn (prog, results) =>
            if ListPair.all satisfies (f'map, results) then SOME prog else NONE)
          ps
      (*
      val _ = log ("bonus: match_choice: matched "^(Sdl ps)^" programs and "^(Sdl values)^" test cases to "^(Sdl fs)^" fs and "^(Sdl f's)^" f's\n")
      val _ = log ("   g: "^(BV.show g)^"\n")
      val _ = log ("   h: "^(BV.show h)^"\n")
      val _ = List.map (fn f => log ("  f: "^(BV.show f)^"\n")) fs
      val _ = List.map (fn f => log (" f': "^(BV.show f)^"\n")) f's
      *)
    in
      (fs, f's)
    end

  fun try_programs ps =
    (log ("bonus: try_results: about to send "^(Sdl ps)^" results up to the server\n");
     BruteSolve.server ps;
     true)
      handle BruteSolve.NoSolution =>
        (log ("bonus: try_results: no solution\n");
         false)

  val minsize = 5 (* Believed minimum size of f, g, or h. *)
  
  fun solve_with_tvecs (spec: Solver.spec) minsize biggest_progs pairs =
    let
      (**** Re-reason about sizes of f, g, and h. ****)
      
      val ops = #ops spec

      val fgh_size = #size spec - 4 (* lambda + ifz + and + 1 *)
      val _ = Assert.assert "size too small" (fgh_size >= 3)
      (* ???? should never be lower *)
      val minsize =
        let val x = fgh_size div 3 in if x < minsize then x else minsize end
      val maxsize = fgh_size - (2 * minsize) (* e,g, 18 = 5 5 and 8 *)
      (* If g is size x what's the biggest h can be, assuming f is minsize? *)
      fun other_size x = fgh_size-x-minsize
      fun segr_size gsize hsize = fgh_size-gsize-hsize (* self-explanatory *)
      
      (* Each g/h candidate will be paired with a bit wector that expresses how
       * much it agrees with the server's true function. *)
      fun give_wector (prog: BV.program) : (BV.program * BitVec.t) =
        let
          fun set_wector_bit ((input,output),(wector:BitVec.t,index:int)) =
            (* Set the bit if the candidate agrees with the server on this pair. *)
            (if Eval.eval prog input <> output then wector else
               BitVec.set (wector, index),
             index+1)
          val initial_wector = BitVec.new (List.length pairs)
          val (final_wector,_) = foldr set_wector_bit (initial_wector, 0) pairs
        in
          (prog, final_wector)
        end

      (* Each f candidate is paired with its own results on the inputs. *)
      fun give_results (prog: BV.program)
                       : (BV.program * ((Word64.word * Word64.word) list)) =
        let
          val results: (Word64.word * Word64.word) list =
                List.map (fn (input,_) => (input, Eval.eval prog input)) pairs
        in
          (prog, results)
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
      fun generate_and1 size = List.map give_results
        (if size = maxsize then
           (* welp, I don't want to change the Brute.generate structure now, so. *)
           List.map (fn Lambda(x,e) => Lambda(x,Binop(And,e,One))) biggest_progs
         else Brute.generate_and1 {size=1+size,ops=ops})
      (* Note: 1+size for the enclosing lambda, not part of f/g/h. *)
      val _ = log ("bonus: generating candidate g/h pairs\n")
      val ghs = List.map (fn x => generate_wector x) size_categories
      val num_ghs = foldr (fn (l,x) => List.length l + x) 0 ghs
      val _ = log ("bonus: generated "^(Sd num_ghs)^" g/h candidate programs\n")
      val _ = log ("bonus: generating candidate f conditions\n")
      val fs  = List.map (fn x => generate_and1   x) size_categories
      val num_fs = foldr (fn (l,x) => List.length l + x) 0 fs
      val _ = log ("bonus: generated "^(Sl num_fs)^" f candidate programs\n")

      (**** Find all pairs of g/h candidates. ****)
      val _ = log ("bonus: building g/h pairs\n")

      val ghs_top_bit_set =
        List.map (List.filter (fn (_,wector) => BitVec.top_bit_set wector)) ghs

      (* Note that each slot in the list contains all programs of sizes less
       * than that slot's size, too, so we only pair up against one slot. *)
      fun find_pairs ((g as (gprog:BV.program,gwec:BitVec.t)), candidates)
            : ((BV.program * BV.program) * (int * int)) list =
        let
          val gsize = size gprog - 1 (* strip g's outer lambda *)
          val gsize' = Int.max (minsize, gsize) (* Smaller ones get generated too. *)
          val max_h_size = Int.min (gsize', other_size gsize') (* cf WLOG *)
          val _ = Assert.assert "hsize too big" (max_h_size <= maxsize)
          val _ = Assert.assert "hsize too small" (max_h_size >= minsize)
          fun does_h_match ((hprog,hwec), candidates) =
            if BitVec.orFills(hwec,gwec) then
              let val hsize = size hprog - 1 (* likewise outer lambda *)
              in
                (*
                log ("bonus: match made! (sizes " ^ Int.toString gsize ^ " " ^
                     Int.toString hsize ^ ")\n");
                *)
                ((gprog,hprog),(gsize,hsize))::candidates
              end
            else candidates
          val where_to_look =
            if BitVec.top_bit_set gwec then ghs else ghs_top_bit_set
        in
          (* Pick hs from the slot in the list that has programs no bigger
           * than the max allowable size for h given g's size. *)
          foldr does_h_match candidates (List.nth (where_to_look, max_h_size-minsize))
        end
      (* The last slot has programs of all sizes.
       * Without loss of general fantasy, 'g' is not smaller than 'h'. *)
      val candidate_pairs = foldr find_pairs [] (List.last ghs)
      val _ = log ("bonus: found "^(Sdl candidate_pairs)^" potential pairs\n")

      (**** Find matching segregators f for each g/h candidate pair. ****)
      val _ = log ("bonus: searching for segregator functions\n")

      exception Reduce of BV.program list
      val last = ref 0
      fun find_segregators ((gh as (Lambda (xg,eg), Lambda (xh,eh)),(gsize,hsize)),
                            whole_progs) =
        let
          val _ = Assert.assert "xg xh aren't the same" (xg = xh)
          val _ =
            if length whole_progs > 250000
            then raise Reduce whole_progs
            else ()
          val max_f_size = (* As in find_pairs, smaller ones are generated. *)
            segr_size (Int.max (minsize, gsize)) (Int.max (minsize, hsize))
          val (matching_fs_gh, matching_fs_hg) =
                match_choice pairs gh (List.nth (fs, max_f_size-minsize))
          (* Function to glue all 3 together, with 'g' in the true branch. *)
          fun make_fgh (Lambda(xf,ef)) =
            (Assert.assert "xf/xg/xh" (xf = xg); Lambda(xf, Ifz(ef,eg,eh)))
          (* Function to glue all 3 together, with 'h' in the true branch. *)
          fun make_fhg (Lambda(xf,ef)) =
            (Assert.assert "xf/xh/xg" (xf = xg); Lambda(xf, Ifz(ef,eh,eg)))
          (* Got here. *)
          val whole_progs_gh = List.map make_fgh matching_fs_gh
          val whole_progs_hg = List.map make_fhg matching_fs_hg
          val new = revappend (whole_progs_gh, revappend (whole_progs_hg, whole_progs))
          val _ =
            if (length new - !last) > 10000
            then (log ("bonus: find_segregators: total "^
                       (Sd (length new))^"\n"); last := (length new))
            else ()
        in
          new
        end
      
      exception ActuallyItWasFine of BV.program list
      (* Okay, we came up with too many options.  Try to reduce a little. *)
      fun doreduce progs =
        let
          val _ = log ("bonus: too many -- trying to come up with more test vectors\n")
          val disambigs = BruteSolve.ndisambig progs (!Flags.nquestions)
          val newvecs =
            case disambigs
            of l as (_::_) => ListPair.zip (l, ServerIO.eval l)
            
               (* OK, we couldn't find a disambiguator -- just ask the server about the first one. *)
             | nil =>
               case ServerIO.guess (List.nth (progs, 0))
               
                  (* Well, then! *)
               of ServerIO.RIGHT => raise ActuallyItWasFine progs
                  (* Just add one more test vector that the server gives us. *)
                | ServerIO.WRONG {input = inp, exp = exp, ...} => [(inp, exp)]
          val _ = log ("bonus: found "^(Sdl newvecs)^" more test vectors\n")
        in
          revappend (newvecs, pairs)
        end
          
    in
      foldr find_segregators [] candidate_pairs
        handle Reduce ps =>
          solve_with_tvecs spec minsize biggest_progs (doreduce ps)
        handle ActuallyItWasFine ps => ps
    end

  fun solve 0 _ = raise Fail "Minsize became too min. We suck. :("
    | solve minsize (spec: Solver.spec) : unit =
    let
      val ops = #ops spec
      val _ = Assert.assert "not a bonus prob" (List.exists (fn x => x = O_Bonus) ops)
      val _ = Assert.assert "minsize way too big" (minsize < 10)

      (**** Reason about sizes of f, g, and h. ****)

      val fgh_size = #size spec - 4 (* lambda + ifz + and + 1 *)
      val _ = Assert.assert "size too small" (fgh_size >= 3)
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
      val _ = log ("bonus: generating for size "^(Sd (1+maxsize)^"\n"))
      val biggest_progs = Brute.generate {size=1+maxsize,ops=ops}
      val _ = log ("bonus: generating queries\n")
      val inputs = BruteSolve.get_inputs biggest_progs
      val pairs = ListPair.zip (inputs, ServerIO.eval inputs)
                    : (Word64.word * Word64.word) list
      
      val candidate_progs = solve_with_tvecs spec minsize biggest_progs pairs
    in
      (* Outer loop. Repeat with a laxer minsize if our estimate was too big. *)
      if try_programs candidate_progs then ()
      else (Flags.log ("bonus: min min minsize " ^ Sd minsize ^ " not min enough.\n");
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
      val bigprogs = List.map (split_ifzs o TestUtil.Parse) [
        "(lambda (x_13) (if0 (and (shr16 (if0 (plus (plus (shr1 (xor (shr4 x_13) x_13)) 0) x_13) 1 x_13)) 1) (shl1 (shr1 (shl1 (shr4 (if0 (plus (shl1 (plus 1 1)) x_13) 1 x_13))))) (not (plus (and (if0 (shr1 (shl1 (shr1 (shr4 x_13)))) x_13 1) x_13) x_13))))",
        "(lambda (x_12) (if0 (and (shr1 (plus (and (if0 (shr1 (shr4 (xor 1 x_12))) 0 x_12) x_12) x_12)) 1) (not (shr4 (plus (if0 (and x_12 1) (not (shr1 x_12)) x_12) 0))) (or (or (shr1 (if0 (shr16 x_12) 1 x_12)) x_12) 0)))",
        "(lambda (x_15) (if0 (and (if0 (plus (shr16 x_15) x_15) x_15 (and x_15 1)) 1) (shr4 (shr16 (if0 (and (shr16 x_15) 1) 0 x_15))) (shr16 (xor (shr1 (if0 (xor (shr4 (not x_15)) 1) 1 (shl1 x_15))) 1))))",
        "(lambda (x_2) (if0 (and (xor (plus (plus 1 x_2) (if0 x_2 0 1)) 0) 1) (plus (plus (if0 (xor (shl1 (xor x_2 0)) 0) 1 x_2) x_2) x_2) (shl1 (shr1 (shr4 (xor (if0 (shl1 (shr16 (shr4 (shr16 x_2)))) x_2 1) x_2))))))",
        "(lambda (x_15) (if0 (and (not (shr1 (shr4 (plus (if0 (plus (shr4 (shr16 x_15)) 0) 1 x_15) 1)))) 1) (plus (or (if0 (plus (and x_15 (shr1 x_15)) 0) 0 x_15) 0) x_15) (and x_15 (if0 (and (shr16 x_15) 1) 1 x_15))))",
        "(lambda (x_14) (if0 (and (not (xor (xor (if0 x_14 1 (shr1 x_14)) 1) 0)) 1) (or x_14 (shl1 (xor (if0 x_14 0 1) x_14))) (xor (not (or (if0 (shr1 (shr1 (not x_14))) (shr1 0) x_14) 1)) x_14)))",
        "(lambda (x_3) (if0 (and (if0 (plus (shr16 x_3) x_3) 0 (or (shr4 x_3) 0)) 1) (shr16 (if0 (shr16 (shr1 (xor (shr1 (not 0)) (xor x_3 0)))) 0 x_3)) (plus (not x_3) (if0 (xor x_3 1) (not 1) 0))))",
        "(lambda (x_15) (if0 (and (shr4 (shr4 (shr4 (plus (if0 (shr1 (and x_15 (shl1 x_15))) 0 x_15) 0)))) 1) (or (shr1 (if0 (and (shr1 (shl1 (shr1 x_15))) x_15) 0 x_15)) x_15) (or (plus (if0 (xor (shr16 0) (xor x_15 1)) 0 1) 1) x_15)))"]

      val f = (fn (f,g,h) => Assert.say_yellow
         ("|f| = " ^ Sd (size_expr f) ^
        "; |g| = " ^ Sd (size_expr g) ^
        "; |h| = " ^ Sd (size_expr h)))
    in
      Assert.say_yellow "bonus category 42:";
      List.map f progs;
      Assert.say_yellow "bonus category 137:";
      List.map f bigprogs;
      true
    end,
    true]
end
