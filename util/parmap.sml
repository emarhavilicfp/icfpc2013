signature PARMAP =
sig
  type 'b chan = 'b MLton.Pacml.chan
  val par1 : (('a -> 'b) * 'a) -> 'b chan
  val map : (('a -> 'b) * 'a) list -> 'b list
end

structure ParMap : PARMAP =
struct
  open MLton.Pacml

  fun par1 (f, x) = let
      val ch = channel ()
      val _ = spawnParasite (fn () => aSend (ch, f(x)))
    in
      ch
    end

  fun map jobs = List.map (MLton.Pacml.recv o par1) jobs
end
