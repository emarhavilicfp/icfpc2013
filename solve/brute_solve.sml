structure BruteSolve =
struct
  infixr 0 $
  fun f $ x = f x
  
  val log = Flags.log
  
  structure MT = MersenneTwister
  val mt = ref (MT.init32 0w0)
  
  val w32w64 = Word64.fromLargeInt o Word32.toLargeInt
  
  fun random () =
    let
      val _ = mt := (MT.init32 (!Flags.seed))
      val rv = Word64.orb (
        Word64.<< (w32w64 $ MT.rand32 (!mt), 0w32),
        w32w64 $ MT.rand32 (!mt)
      )
      val _ = Flags.seed := MT.rand32 (!mt)
    in
      rv
    end

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

  (* The bool is true if we could narrow it down ("Seperable"). *)
  type narrowed = (bool * BV.program list)

                         (* Counterexample from the server. (input,output). *)
  datatype narrowinput = KNOWN of Word64.word * Word64.word
                         (* We want to ask the server about these. *)
                       | UNKNOWN of Word64.word list

  (* Filter down a bunch of functions into those that are consistent
     with the target function on `inputs`.
     
     Returns a tuple of (bool, programs); the boolean represents whether the
     set was actually shrunk.  If it wasn't, then the dataset is inseparable
     on the provided inputs.
   *)
  fun narrow choices (input: narrowinput) : narrowed =
    let
      val pairs = 
        case input
        of KNOWN ks => [ks]
         | UNKNOWN [] => []
         | UNKNOWN inps => ListPair.zip (inps, ServerIO.eval inps)
      val possible as (seperable, newchoices) =
          case pairs
           of [] => (false, choices)
            | _ =>
              (true,
                List.filter
                  (fn prog =>
                    List.all (fn (q, a) => (Eval.eval prog q) = a) pairs)
                  choices)

      val oldlen = length choices
      val newlen = length newchoices
      val possible' =
          case (possible, oldlen = newlen)
          of ((true, ps), true) => (false, ps)
           | (ps, _) => ps
      val _ = log ("solve: narrow: " ^ (Int.toString oldlen) ^
                   " -> " ^ (Int.toString newlen) ^ " programs\n")
    in
      possible'
    end

  fun get_inputs progs =
    if (length progs) > 1500000 then
      (log $ (Int.toString $ length progs) ^ " is *far* too many to evaluate on...\n";
       quickn $ Int.min (4, !Flags.nquestions))
    else ndisambig progs (!Flags.nquestions)

  exception NoSolution
  fun server ps =
    let
      fun rep (_, []) = raise NoSolution
      
          (* We successfully narrowed down -- run it again. *)
        | rep (true, ps) =
          let
            val _ = log ("solve: rep: creating inputs for "^(Int.toString $ length ps)^" programs...\n")
            val inputs = get_inputs ps
            val _ = log ("solve: rep: narrowing...\n")
          in
            rep (narrow ps (UNKNOWN inputs))
          end
          
          (* We didn't have any success narrowing it down -- time to guess one! *)
        | rep (false, ps) =
          let
            val _ = log ("solve: rep: had to guess one from a list of "^(Int.toString $ length ps)^" programs\n")
            val result = ServerIO.guess (List.hd ps)
          in
            case result
            of ServerIO.RIGHT
                 => result
             | ServerIO.WRONG { input = inp,
                                exp = exp,
                                ours = ours }
                 => rep (narrow ps $ KNOWN (inp, exp))
          end
      
      val _ = log ("solve: iterating...\n")
      val _ = rep (true, ps) (* effect: guesses the right answer, or throws an exception if we failed *)
    in
      ()
    end
  
  fun solve_with_size (a: Solver.spec) i =
    let
      val _ = log ("solve: generating of size " ^ (Int.toString i) ^ "...\n")
      val choices = Brute.generate {size = i, ops = (#ops a)}
    in
      server choices
        handle NoSolution => (log "no solution of that size\n")
    end

  fun solve_upto_size a 0 = ()
    | solve_upto_size a 6 = ()
    | solve_upto_size a n =
        let
          val _ = solve_upto_size a (n-1)
        in
          solve_with_size a n
        end

  fun solve a =
    let
      val sz = #size a
      val _ = log ("solve: solving up to " ^ (Int.toString sz) ^  " ...\n")
    in
      solve_upto_size a sz
    end

  val solver : Solver.solver =
    { shortname = "brute",
      name = "brute force",
      f = solve
      }
end
