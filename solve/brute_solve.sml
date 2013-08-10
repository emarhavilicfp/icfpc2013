structure BruteSolve =
struct
  open Haskell
  
  fun log s = let open TextIO in output (stdErr, s); flushOut stdErr end
  
  structure MT = MersenneTwister
  val mt = ref (MT.init32 0w0)
  
  val w32w64 = Word64.fromLargeInt o Word32.toLargeInt
  
  fun random () =
    Word64.orb (
      Word64.<< (w32w64 $ MT.rand32 (!mt), 0w32),
      w32w64 $ MT.rand32 (!mt)
    )
  
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
  
  fun ndisambig choices 0 = []
    | ndisambig choices n =
        case disambiguate choices
        of SOME x => x :: (ndisambig choices (n-1))
         | NONE => ndisambig choices (n-1)
  
  exception Narrowed
  fun narrow choices =
    let
      val vecs = ndisambig choices (!Flags.nquestions)
      val _ = case vecs of nil => raise Narrowed | _ => ()
      val results = ServerIO.eval vecs
      val pairs = ListPair.zip (vecs, results)
      val possible =
        List.filter
          (fn prog => 
            List.all (fn (q, a) => (Eval.eval prog q) = a) pairs)
          choices
      val _ = log ("Narrowed "^(Int.toString $ length choices)^" to "^(Int.toString $ length possible)^"\n")
    in
      possible
    end

  fun solve a =
    let
      val _ = mt := (MT.init32 (!Flags.seed))
      fun rep [] = raise Fail "Uhm..."
        | rep (p :: nil) = ServerIO.guess p
        | rep ps = rep (narrow ps)
                     handle Narrowed => ServerIO.guess (List.nth (ps, 1))
      val res = rep (Brute.generate a)
      val _ = Flags.seed := MT.rand32 (!mt)
    in
      case rep (Brute.generate a)
      of ServerIO.RIGHT => ()
       | ServerIO.WRONG _ => raise Fail "I'm pretty sure we're right, but the server said we are wrong"
    end

  val solver : Solver.solver =
    { shortname = "brute",
      name = "brute force",
      f = solve
      }
end
