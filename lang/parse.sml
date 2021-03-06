(* L1 Compiler
 * Parsing
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Glueing together the pieces produced by ML-Lex and ML-Yacc
 *)

signature PARSE =
sig
  (* parse filename = ast
   * will raise ErrorMsg.Error in case of lexing or parsing error
   *)
  val parse : string -> BVP.program
end

structure Parse :> PARSE =
struct 

  structure BVLrVals = BVLrValsFn (structure Token = LrParser.Token)
  structure BVLex = BVLexFn (structure Tokens = BVLrVals.Tokens)
  structure BVParse = Join (structure ParserData = BVLrVals.ParserData
                            structure Lex = BVLex
                            structure LrParser = LrParser)

  (* Main parsing function *)
  fun parse str =
      let
          val instream = TextIO.openString str
	  val _ = ErrorMsg.reset() (* no errors, no messages so far *)
	  val _ = ParseState.setfile "lol!" (* start at position 0 in filename *)
	  fun parseerror (s, p1, p2) = ErrorMsg.error (ParseState.ext (p1,p2)) s
	  val lexer = LrParser.Stream.streamify
			  (BVLex.makeLexer (fn _ => TextIO.input instream))
	  (* 0 = no error correction, 15 = reasonable lookahead for correction *)
	  val (absyn, _) = BVParse.parse(0, lexer, parseerror, ())
          val _ = if !ErrorMsg.anyErrors
		  then raise ErrorMsg.Error
		  else ()
      in
	  absyn
      end
      handle LrParser.ParseError => raise ErrorMsg.Error (* always preceded by msg *)
           | e as IO.Io _ => ( ErrorMsg.error NONE (exnMessage e);
                               raise ErrorMsg.Error )

end
