structure BruteSolve =
struct
  open Haskell
  
  val log = Flags.log
  
  structure MT = MersenneTwister
  val mt = ref (MT.init32 0w0)
  
  val w32w64 = Word64.fromLargeInt o Word32.toLargeInt
  
  fun random () =
    Word64.orb (
      Word64.<< (w32w64 $ MT.rand32 (!mt), 0w32),
      w32w64 $ MT.rand32 (!mt)
    )

  (* Takes a list of functions, evaluates them on a random input word
     to get a list of outputs.  If any outputs differ from the first,
     then the radnom input test word is a good disambiguator and is
     returned. *)
  fun disambiguate choices =
    let
      val n = random ()
      val results = map (fn x => Eval.eval x n) choices
      val first = case results
                of x::_ => x
                 | _ => 0w0
    in
      case length (List.filter (fn x => x <> first) results)
      of 0 => NONE
       | _ => SOME n
    end

  (* Returns n points, each of which disambiguates at least two
     functions. *)
  fun ndisambig choices 0 = []
    | ndisambig choices n =
        case disambiguate choices
        of SOME x => x :: (ndisambig choices (n-1))
         | NONE => ndisambig choices (n-1)
  
  (* Quickly get n random inputs.  They might help, they might not -- but we
     have far too many functions to evaluate all of them right now.  *)
  fun quickn n = List.tabulate (n, fn _ => random ())

  datatype narrowed = Separated of BV.program list 
                    | Inseparable of BV.program list
  datatype narrowinput = KNOWN of Word64.word * Word64.word
                       | UNKNOWN of Word64.word list

  (* Filter down a bunch of functions into those that are consistent
     with the target function on `inputs`.

     Returns the filtered list wrapped in a `narrowed` datatype to
     signify whether it was only vacuously consistent with the target
     function (because `inputs` is `nil`).
   *)
  fun narrow choices input =
    let
      val pairs = 
        case input
        of KNOWN ks => [ks]
         | UNKNOWN [] => []
         | UNKNOWN inps => ListPair.zip (inps, ServerIO.eval inps)
      val possible =
          case pairs
           of nil => Inseparable choices
            | _ =>
              Separated $
                List.filter
                  (fn prog =>
                    List.all (fn (q, a) => (Eval.eval prog q) = a) pairs)
                  choices

      val len = case possible
                 of Separated ps => length ps
                  | Inseparable ps => length ps
      val possible' =
          case (possible, length choices = len)
          of (Separated ps, true) => Inseparable ps
           | (ps, _) => ps
      val _ = log ("Narrowed " ^ (Int.toString $ length choices) ^
                   " to " ^ (Int.toString $ len) ^ "\n")
    in
      possible'
    end

  fun solve a =
    let
      val _ = mt := (MT.init32 (!Flags.seed))
      fun rep (Separated []) = raise Fail "Uhm..."
        | rep (Inseparable []) = raise Fail "Double uhm..."

        | rep (Separated (p :: nil)) = ServerIO.guess p
        | rep (Separated ps) =
          let
            val _ = log ("solve: rep: creating inputs for "^(Int.toString $ length ps)^" programs...\n");
            val inputs =
              if (length ps) > 1500000
              then (log ((Int.toString $ length ps) ^ " is *far* too many to evaluate on...\n");
                    quickn $ Int.min (4, !Flags.nquestions))
              else ndisambig ps (!Flags.nquestions)
            val _ = log ("solve: rep: narrowing...\n");
          in
            rep (narrow ps (UNKNOWN inputs))
          end

        | rep (Inseparable (p :: nil)) = ServerIO.guess p
        | rep (Inseparable ps) =
          let
            val result = ServerIO.guess (List.hd ps)
          in
            case result
            of ServerIO.RIGHT => result
             | ServerIO.WRONG { input = inp,
                                exp = exp,
                                ours = ours } =>
               rep (narrow ps $ KNOWN (inp, exp))
          end
      val _ = log ("solve: generating...\n")
      val res = rep (Separated (Brute.generate a))
      val _ = Flags.seed := MT.rand32 (!mt)
    in
      case res
      of ServerIO.RIGHT => ()
       | ServerIO.WRONG _ =>
         raise Fail ("I'm pretty sure we're right, " ^
                     "but the server said we are wrong")
    end

  val solver : Solver.solver =
    { shortname = "brute",
      name = "brute force",
      f = solve
      }
end
