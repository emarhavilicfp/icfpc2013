signature TESTUTIL =
sig
  val Parse : string -> BV.program
end

structure TestUtil =
struct
  fun Parse s =
    let
      val bvp = Parse.parse s
    in
      Elab.elab bvp
    end
end
