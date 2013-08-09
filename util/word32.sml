(* L1 Compiler
 * Utilities for signed modular arithmetic
 * Author: Frank Pfenning
 *)

(*
 * There are two useful structure in the SML Basis Library
 * Int32, with 2's complement arithmetic,
 *        but it raises Overflow instead of doing modular arithmetic
 * Word32, with unsigned modular arithmetic
 *
 * This structure implements some signed operations on Word32
 *)

signature WORD32_SIGNED =
sig

  val TMAX : Word32.word	(* largest signed positive word, 2^31-1  *)
  val TMIN : Word32.word	(* smallest signed negative word -2^31 *)
  val ZERO : Word32.word	(* 0 *)
  val fromString : string -> Word32.word option	(* parse from string, no sign *)
				(* raises Overflow if not 0 <= n < 2^32 *)
  val toString : Word32.word -> string (* print to string, with sign *)
  val abs : Word32.word -> Word32.word
  val adiv : Word32.word * Word32.word -> Word32.word
  val amod : Word32.word * Word32.word -> Word32.word
  val lt : Word32.word * Word32.word -> bool
  val gt : Word32.word * Word32.word -> bool
  val le : Word32.word * Word32.word -> bool
  val ge : Word32.word * Word32.word -> bool
end

structure Word32Signed :> WORD32_SIGNED =
struct

  val TMIN = Word32.<<(Word32.fromInt(1), Word.fromInt(Word32.wordSize-1))
  val TMAX = Word32.-(TMIN, Word32.fromInt(1))
  val ZERO = Word32.fromInt(0)
  fun neg w = Word32.>(w, TMAX)

  (* fromString does not allow leading "-" *)
  fun fromString (str) =
         (* scanString might also raise Overflow *)
	 StringCvt.scanString (Word32.scan StringCvt.DEC) str

  fun toString (w) =
      if neg w
	 then "-0x" ^ Word32.fmt StringCvt.HEX (Word32.~(w))
      else "0x" ^ Word32.fmt StringCvt.HEX w

  fun toInt32 w = Int32.fromLarge (Word32.toLargeInt w)
  fun fromInt32 i = Word32.fromLargeInt (Int32.toLarge i)

  fun abs w = if neg w then Word32.~ (w) else w
  fun adiv (a,b) = fromInt32 (Int32.div (toInt32 a, toInt32 b))
  fun amod (a,b) = fromInt32 (Int32.mod (toInt32 a, if neg a andalso neg b then toInt32 b
                                                    else if neg b then toInt32 (abs b)
                                                    else if neg a then toInt32 (Word32.~ b)
                                                    else toInt32 b))

  fun lt (a,b) = Int32.compare (toInt32 a, toInt32 b) = LESS
  fun gt (a,b) = Int32.compare (toInt32 a, toInt32 b) = GREATER
  fun le (a,b) = case Int32.compare (toInt32 a, toInt32 b) of LESS => true | EQUAL => true | _ => false
  fun ge (a,b) = case Int32.compare (toInt32 a, toInt32 b) of GREATER => true | EQUAL => true | _ => false

end
