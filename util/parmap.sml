signature PARMAP =
sig
  val map : (('a -> 'b) * 'a) list -> 'b list
end

structure ParMap : PARMAP =
struct

fun map jobs =
    let
        val chans = List.map par1 jobs
    in
        List.map MLton.Pacml.recv chans
    end

end
