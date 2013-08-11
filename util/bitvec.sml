signature BITVEC =
sig
  type bitvec
  val new : int -> bitvec
  val set : bitvec * int -> unit
  val orFills : (bitvec * bitvec) -> bool
  val doubleSize : bitvec -> bitvec
end

structure BitVec : BITVEC =
struct
  type bitvec = (int * Word.word Array.array)


  infixr 0 $
  fun f $ x = f x

  fun numWords bits = ((bits + (Word.wordSize - 1)) div Word.wordSize)
  fun wordOffset bNum = bNum div Word.wordSize
  fun bitOffset bNum = bNum mod Word.wordSize

  fun new size = (size, Array.array ((numWords size) * 2, 0w0))

  fun set ((_, a), i) =
    let
      val newVal = Word.orb(Array.sub (a, wordOffset i), Word.<<(0w1, Word.fromInt $ bitOffset i))
    in
      Array.update (a, wordOffset i, newVal)
    end

  val neg1 = Word.notb 0w0

  exception Size
  exception NOPE
  fun orFills ((s1, a1), (s2, a2)) =
    let
      val numFullWords = (wordOffset s1) - 1
      fun foldFulls (~1) curRes = curRes
        | foldFulls n curRes =
          if not curRes then raise NOPE else
            foldFulls (n-1) (
              (case Word.compare(neg1, Word.andb(Array.sub (a1, n), Array.sub (a2, n)))
                of EQUAL => true | _ => false) andalso curRes)

      fun checkLast () = (case Word.compare (Word.-(Word.<<(0w1, Word.fromInt $ bitOffset s1), 0w1),
                           Word.andb(Array.sub(a1, wordOffset s1), Array.sub(a2, wordOffset s1)))
                          of EQUAL => true | _ => false)
    in
      if s1 <> s2 then raise Size else
        if bitOffset s1 = 0 then foldFulls numFullWords true handle NOPE => false
        else
          let
            val fulls = foldFulls numFullWords true handle NOPE => false
            val last = checkLast ()
          in
            fulls andalso last
          end
    end

  fun doubleSize (s, a) = if (numWords s) < (Array.length a) then (2*s, a)
    else let
      val newSize = 2*s
      val newArr = Array.array (numWords newSize, 0w0)
      val _ = Array.copy {src=a, dst=newArr, di=0}
    in
      (newSize, newArr)
    end

end
