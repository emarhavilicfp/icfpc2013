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

  val solvers = [Solver.TotalNobe]
  val solver = case solvers
               of s :: _ => ref s
                | _ => raise Fail "no solvers"
  
  val options = [{short = "v", long=["verbose"], 
                  desc=G.NoArg (fn () => Flag.set Flags.verbose),
                  help="verbose messages"}
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
        val header = "Usage: solve [OPTION...] SOURCEFILE\nwhere OPTION is"
        val usageinfo = G.usageInfo {header = header, options = options}
        fun errfn msg = (say (msg ^ "\n" ^ usageinfo) ; raise EXIT)

        (* val _ = Temp.reset (); (* reset temp variable counter *) *)
        val _ = Flags.reset (); (* return all flags to default value *)

        val _ = if List.length args = 0 then
                    (say usageinfo; raise EXIT)
                else ()

        val (opts, files) =
            G.getOpt {argOrder = G.Permute,
                      options = options,
                      errFn = errfn}
                     args

        val source =
            case files
              of [] => errfn "Error: no input file"
               | [filename] => filename
               | _ => errfn "Error: more than one input file"


(*
        val _ = Flag.guard Flags.verbose say ("Enabled optimizations: " ^ String.concat (map (fn x => (#shortname x) ^ " ") (!enabledopts)))
*)
(*        val _ = Flag.guard Flags.verbose say ("Parsing... " ^ source)
        val ast = Parse.parse source
        val (_, funcs) = ast
        val _ = Flag.guard Flags.ast
                  (fn () => say (Ast.Print.pp_program ast)) ()

        val _ = Flag.guard Flags.verbose say "Checking..."
        val ast = TypeChecker.typecheck ast

        val _ = Flag.guard Flags.verbose say "Translating..."
        val ir = Trans.translate ast
        val _ = Flag.guard Flags.ir (fn () => say (TreeUtils.Print.pp_program ir)) ()

        val _ = Flag.guard Flags.verbose say "Performing CFA..."
        val cf = Cfa.cfa ir

        val _ = Flag.guard Flags.verbose say "Generating veriloog..."
        val output = Verilate.verilate cf
*)
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
    val tests = [(BV_Test.test, "BV_Test")]
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
      (say_red "(╯°□°）╯︵ ┻━┻"; OS.Process.failure)
  end
end
