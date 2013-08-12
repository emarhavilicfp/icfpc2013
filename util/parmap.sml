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

  fun mapAccum _ s nil = (s, nil)
    | mapAccum f s (x::xs) = let
          val (s', ys) = mapAccum f s xs
          val (s'', y) = f (s', x)
        in
          (s'', y::ys)
        end

  fun map jobs = let
    val hosts = List.tabulate (List.length jobs, (fn x => x mod 64))
    val host_mappings = ListPair.zip (hosts, jobs)
    val (_, job_groups) =
      mapAccum (fn (hjs, h) => let
          val (yes, no) = List.partition (fn (h', _) => h' = h) hjs
          val js = List.map (fn (f, s) => s) yes
        in
          (no, (channel (), js))
        end) host_mappings (List.tabulate (64, (fn x => x)))
  in
      List.concat (List.map (fn (ch, js) =>
        let
          val _ = spawnHost (fn () => aSend (ch, List.map (MLton.Pacml.recv o par1) js))
        in
          recv ch
        end) job_groups)
  end
end
