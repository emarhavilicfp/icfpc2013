signature FLAGS =
sig
  val verbose : Flag.flag
  val liveness : Flag.flag
  val ast : Flag.flag
  val ir : Flag.flag
  val assem : Flag.flag
  val color : Flag.flag
  val safe : Flag.flag

  val nquestions : int ref
  val seed : Word32.word ref
  
  val reset : unit -> unit	(* Anus... *)
  
  val log : string -> unit
end

structure Flags :> FLAGS =
struct
  val verbose = Flag.flag "verbose"
  val liveness = Flag.flag "liveness"
  val ast = Flag.flag "ast"
  val ir = Flag.flag "ir"
  val assem = Flag.flag "assem"
  val color = Flag.flag "color"
  val safe = Flag.flag "safe"
  val nquestions = ref 16
  val seed : Word32.word ref = ref 0w0
  
  fun reset () =
    (List.app Flag.unset [verbose, ast,
                          ir, assem, liveness, safe])
  
  (* Might as well go here, I guess. *)
  fun log s =
    let
      open TextIO
    in
      output (stdErr, s);
      flushOut stdErr
    end

end
  