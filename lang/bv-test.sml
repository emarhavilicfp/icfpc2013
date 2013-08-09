structure BV_Test =
struct
  open BV
  open Haskell

  fun test () = let
    (* tests AST -> str -> AST is a fixpoint *)
    fun test_from_prog prog = let
      val str = show prog
    in
      Assert.assert str $ prog = Parse.parse str
    end
    (* tests str -> AST -> str -> AST is a fixpoint *)
    fun test_from_str str = let
      val prog = Parse.parse str
    in
      Assert.assert str $ prog = Parse.parse (show prog)
    end

  in
    List.all (fn x => x) [
    test_from_prog (Lambda ((Symbol.symbol "x"), (Id (Symbol.symbol "x")))),
    test_from_str  "(lambda (x) x)",
    test_from_prog (Lambda ((Symbol.symbol "x"),
                    (Binop (Plus,(Id (Symbol.symbol "x")),
                                 (Id (Symbol.symbol "x")))))),
    true]
  end
  
end
