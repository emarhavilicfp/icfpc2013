structure BV_Test =
struct
  open BV

  fun test () = let
    fun assert_eq x y msg =
      print (if x = y then "\027[01;32m%% OK %%\027[00m\n"
             else "\027[01;31m!! FAIL: " ^ msg ^ " !!\027[00m\n")
    (* tests AST -> str -> AST is a fixpoint *)
    fun test_from_prog prog = let
      val str = show prog
    in
      assert_eq prog (Parse.parse str) str
    end
    (* tests str -> AST -> str -> AST is a fixpoint *)
    fun test_from_str str = let
      val prog = Parse.parse str
    in
      assert_eq prog (Parse.parse (show prog)) str
    end

    fun make_ident s = Ident (Symbol.symbol s)
  in
    test_from_prog (Lambda ((make_ident "x"), (Id (make_ident "x"))));
    test_from_str  "(lambda (x) x)";
    test_from_prog (Lambda ((make_ident "x"), (Binop (Plus,(Id (make_ident "x")),(Id (make_ident "x"))))));
    ()
  end
  
end
