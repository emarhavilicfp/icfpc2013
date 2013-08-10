(* L3 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *)

signature TOP =
sig
  (* main function for standalone executable
   * use with SMLofNJ.exportFn("heapfilename", Top.main)
   *)
  val main : string * string list -> OS.Process.status
  
  val check : string * string list -> OS.Process.status
end

structure Top :> TOP =
struct
  structure G = GetOpt  (* from $/smlnj-lib/Util/getopt-sig.sml *)
  open Assert
  open Haskell

  exception EXIT

  val solvers = [BruteSolve.solver, Solver.TotalNobe]
  val solver = case solvers
               of s :: _ => ref s
                | _ => raise Fail "no solvers"
  
  val ops : BV.operator list ref = ref []
  val len : int ref = ref 30
  
  val options = [{short = "v", long=["verbose"], 
                  desc=G.NoArg (fn () => Flag.set Flags.verbose),
                  help="verbose messages"},
                 {short = "", long = ["has-not-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Unop BV.Not :: !ops),
                  help=""},
                 {short = "", long = ["has-shl1-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Unop BV.Shl1 :: !ops),
                  help=""},
                 {short = "", long = ["has-shr1-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Unop BV.Shr1 :: !ops),
                  help=""},
                 {short = "", long = ["has-shr4-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Unop BV.Shr4 :: !ops),
                  help=""},
                 {short = "", long = ["has-shr16-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Unop BV.Shr16 :: !ops),
                  help=""},
                 {short = "", long = ["has-and-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Binop BV.And :: !ops),
                  help=""},
                 {short = "", long = ["has-or-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Binop BV.Or :: !ops),
                  help=""},
                 {short = "", long = ["has-xor-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Binop BV.Xor :: !ops),
                  help=""},
                 {short = "", long = ["has-plus-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Binop BV.Plus :: !ops),
                  help=""},
                 {short = "", long = ["has-if0-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Ifz :: !ops),
                  help=""},
                 {short = "", long = ["has-tfold-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Tfold :: !ops),
                  help=""},
                 {short = "", long = ["has-fold-op"],
                  desc=G.NoArg (fn () => ops := BV.O_Fold :: !ops),
                  help=""},
                 {short = "l", long = ["length"],
                  desc=G.ReqArg (fn s => len := (valOf $ Int.fromString s), "length"),
                  help=""},
                 {short = "q", long = ["questions"],
                  desc=G.ReqArg (fn s => Flags.nquestions := (valOf $ Int.fromString s), "questions"),
                  help="how many questions to ask the server in one go"},
                 {short = "s", long = ["seed"],
                  desc=G.ReqArg (fn s => Flags.seed := (valOf $ Word32.fromString s), "seed"),
                  help="RNG seed"}
                ] 
                @
                map
                  (fn (s: Solver.solver) =>
                    { short = "", long=["use-" ^ (#shortname s)],
                      desc = G.NoArg (fn () => solver := s),
                      help = "solution made by the "^(#name s)^" strategy" })
                  solvers

  fun stem s =
      let
          val (prefix, suffix) =
              Substring.splitr (fn c => c <> #".") (Substring.full s)
      in
          if Substring.isEmpty prefix (* no "." in string s *)
             then s (* return whole string *)
          else Substring.string (Substring.trimr 1 prefix)
      end
  
  exception Aaaaaaaaaaassssssssssssssssssss
  
  fun main (name, args) =
      let
        val header = "Usage: solve PARAM...\nwhere PARAM is"
        val usageinfo = G.usageInfo {header = header, options = options}
        fun errfn msg = (say (msg ^ "\n" ^ usageinfo) ; raise EXIT)

        (* val _ = Temp.reset (); (* reset temp variable counter *) *)
        val _ = Flags.reset (); (* return all flags to default value *)

        val (opts, files) =
            G.getOpt {argOrder = G.Permute,
                      options = options,
                      errFn = errfn}
                     args

        val _ = if List.length (!ops) = 0 then
                    errfn "no ops?"
                else ()

        val () =
            case files
              of [] => ()
               | _ => errfn "excessive arguments (when did Joshua walk back in?)"

        val () = #f (!solver) {size = !len, ops = !ops }

(*        val _ = print output *)
(*
        val afname = stem source ^ ".v"
        val _ = Flag.guard Flags.verbose say ("Writing assembly to " ^ afname ^ " ...")
        val _ = SafeIO.withOpenOut afname (fn afstream =>
                   TextIO.output (afstream, output))
*)
      in
          OS.Process.success
      end
      handle ErrorMsg.Error => ( say "Compilation failed" ; OS.Process.failure )
           | EXIT => OS.Process.failure
           | ErrorMsg.InternalError s => ( say ("Internal compiler error: "^s^"\n"); OS.Process.failure)
           | e => (say ("Unrecognized exception " ^ exnMessage e); OS.Process.failure)


  fun check _ = let
    val tests = [(BV_Test.test, "BV_Test"), (Brute.test, "Brute test")]
          : ((unit -> bool) * string) list
    fun run_test (testfn, name) = let
      val _ = say_nonewline $ name ^ "... "
      val r = testfn()
      val _ = if r then say_green "%% OK %%" else say_red "!! FAIL !!"
    in r
    end
  in
    if List.all run_test tests then
      (say_green "all tests passed"; OS.Process.success)
    else
      (say_red "some tests failed :-("; OS.Process.failure)
  end
end
