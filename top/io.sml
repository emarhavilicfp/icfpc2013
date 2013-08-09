signature SERVER_IO =
sig
  datatype result = RIGHT | WRONG of {input: Word64.word, exp: Word64.word, ours: Word64.word }
  
  val guess : BV.program -> result
  val eval : Word64.word list -> Word64.word list
end

structure ServerIO : SERVER_IO = 
struct
  open Haskell

  datatype result = RIGHT | WRONG of {input: Word64.word, exp: Word64.word, ours: Word64.word }
  
  fun guess prog =
    let
      open TextIO
    in
      print ("GUESS:"^(BV.show prog)^"\n") ;
      case inputLine stdIn
      of SOME "RIGHT\n" => RIGHT
       | SOME "WRONG\n" =>
           let
             val inp = valOf $ scanStream (Word64.scan StringCvt.HEX) stdIn
             val exp = valOf $ scanStream (Word64.scan StringCvt.HEX) stdIn
             val ours = valOf $ scanStream (Word64.scan StringCvt.HEX) stdIn
           in
             WRONG {input = inp, exp = exp, ours = ours}
           end
       | _ => raise Fail "driver shell returned invalid result for guess"
    end
  
  fun eval args =
      let
        open TextIO
      in
        print ("GUESS "^(Int.toString (length args)) ^ "\n");
        foldl (fn (n, _) => print ((Word64.fmt StringCvt.HEX n) ^ "\n")) () args;
        case inputLine stdIn
        of SOME "OKAY\n" =>
             rev $ foldl (fn (_, l) => 
                      (valOf $ scanStream (Word64.scan StringCvt.HEX) stdIn) :: l)
                   [] args
         | _ => raise Fail "driver shell returned invalid result for eval"
      end
end
