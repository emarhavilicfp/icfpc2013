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
  
  (* test "arguments"; is the same as executing a saved
   * heap with arguments on the command line
   *)
  val test : string -> OS.Process.status
end

structure Top :> TOP =
struct
  structure G = GetOpt  (* from $/smlnj-lib/Util/getopt-sig.sml *)

  fun say s = TextIO.output (TextIO.stdErr, s ^ "\n")

  fun newline () = TextIO.output (TextIO.stdErr, "\n")

  exception EXIT
  
(*  val alloptimizations =
    [ConstantFold.optimizer]

  val enabledopts = ref alloptimizations
*)
  val options = [{short = "v", long=["verbose"], 
                  desc=G.NoArg (fn () => Flag.set Flags.verbose),
                  help="verbose messages"},
                 {short = "a", long=["dump-ast"],
                  desc=G.NoArg (fn () => Flag.set Flags.ast),
                  help="pretty print the AST"},
                 {short = "i", long=["dump-ir"],
                  desc=G.NoArg (fn () => Flag.set Flags.ir),
                  help="pretty print the IR"}
                ] 
                
                (* @
                map
                  (fn (opt : Optimizer.optimization) =>
                    { short = "", long=["disable-" ^ (#shortname opt)],
                      desc = G.NoArg (* This is nasty. *)
                        (fn () => enabledopts := List.filter (fn x => (#shortname x) <> (#shortname opt)) (!enabledopts)),
                      help = "disable optimization: " ^ (#description opt) })
                  alloptimizations
                *)

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
        val header = "Usage: compile [OPTION...] SOURCEFILE\nwhere OPTION is"
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

  fun test s = main ("", String.tokens Char.isSpace s)
end
