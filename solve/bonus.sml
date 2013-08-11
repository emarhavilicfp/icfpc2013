(* Specialized bonus problem solver. *)
structure Bonus =
struct
  open BV;
  infixr 0 $
  fun f $ x = f x


  fun test() = List.all (fn x => x) [
    let 
      fun split_ifzs (Lambda(_,Ifz(Binop (And,f,One),g,h))) = (f,g,h)
        | split_ifzs (Lambda(_,Ifz(_,_,_))) = raise Fail "non-and1 bonus"
        | split_ifzs _ = raise Fail "non-ifz bonus"

      val progs = List.map (split_ifzs o TestUtil.Parse) [
        "(lambda (x_5) (if0 (and (and 1 (shr16 (shr16 x_5))) 1) (plus (shr4 (shr16 x_5)) 1) (and (shr1 x_5) (and (shl1 x_5) x_5))))", 
        "(lambda (x_7) (if0 (and (or x_7 (and (not (shr4 x_7)) 1)) 1) (xor x_7 (shr4 (not x_7))) (plus (shl1 (not x_7)) (not 0))))", 
        "(lambda (x_1) (if0 (and (xor x_1 (shr16 (shr4 (shr16 (shr16 x_1))))) 1) (plus (shr16 (shr16 x_1)) (plus x_1 1))  (if0 (shr4 x_1) 1 (xor x_1 1))))", 
        "(lambda (x_11) (if0 (and (not (shr16 (not (shr1 x_11)))) 1) (and (plus (plus 1 x_11) x_11) x_11) (plus x_11 (not (shr1 x_11)))))", 
        "(lambda (x_9) (if0 (and (plus (shr16 (shr16 (shl1 x_9))) (shr16 x_9)) 1) (plus x_9 (shl1 (and (shr4 x_9) x_9))) (plus 1 (plus x_9 x_9))))", 
        "(lambda (x_4) (if0 (and (and (not (shr1 x_4)) (shr16 (not 0))) 1) (xor (plus 1 x_4) 1) (or (plus x_4 (not (shr16 x_4))) x_4)))", 
        "(lambda (x_4) (if0 (and (and (shr1 (not x_4)) x_4) 1) (and x_4 (plus x_4 1))  (and (shr4 x_4) (plus (not x_4) (not 0)))))", 
        "(lambda (x_20) (if0 (and (or x_20 (and (not (shr4 x_20)) 1)) 1) (not (or x_20 (shr1 x_20))) (plus x_20 (shl1 (and (shl1 x_20) x_20)))))", 
        "(lambda (x_7) (if0 (and (xor x_7 (plus (shr16 (not x_7)) x_7)) 1) (plus (not 1) (if0 x_7 1 x_7)) (plus (or (not x_7) (shr4 x_7)) x_7)))", 
        "(lambda (x_8) (if0 (and (and 1 (not (xor (shr4 x_8) x_8))) 1) (and (shr4 (shr16 x_8)) x_8) (plus (shr4 (shr4 (shr16 (shr1 x_8)))) x_8)))", 
        "(lambda (x_20) (if0 (and (plus x_20 (shl1 (and (shr4 x_20) x_20))) 1) (xor (plus (plus x_20 1) 1) 1) (and (plus 1 (not (shr16 x_20))) x_20)))", 
        "(lambda (x_7) (if0 (and (xor (or (shr4 (shr16 x_7)) x_7) x_7) 1) (xor x_7 (shr4 (not x_7))) (xor (shr16 (shr4 x_7)) (plus 1 x_7))))", 
        "(lambda (x_16) (if0 (and (plus x_16 (plus (shr4 (shr4 x_16)) 1)) 1) (shl1 (plus 1 (shr1 x_16))) (plus (not 1) (if0 x_16 1 x_16))))", 
        "(lambda (x_15) (if0 (and (or (not 1) (if0 x_15 1 x_15)) 1) (xor (plus x_15 1) 1) (and x_15 (plus x_15 x_15))))", 
        "(lambda (x_11) (if0 (and (or (not 1) (if0 x_11 1 x_11)) 1) (plus x_11 (shr4 (shr16 (shr4 (shr4 x_11))))) (shl1 (shr16 (shr16 (not x_11))))))  "]

    in
      List.map (fn (f,g,h) => Assert.say_yellow $
          "|f| = " ^ Int.toString (size_expr f) ^
        "; |g| = " ^ Int.toString (size_expr g) ^
        "; |h| = " ^ Int.toString (size_expr h))
        progs;
      true
    end,
    true]
end
