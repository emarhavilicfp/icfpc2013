signature BITVEC =
sig
  type t
  val new : int -> t
  val set : t * int -> t
  val orFills : (t * t) -> bool
  val test : unit -> bool
end

structure BitVec : BITVEC =
struct
  (* A BitVec is a tuple of a size -- in bits -- and the actual storage
     element.  BitVec is an imperative data store, except for the doubleSize
     operation.
    
     Note that we allocate more than necessary in new -- the size of the
     Array could actually be larger than (numWords bits) of the array.  This
     allows resizes to be lightweight.  *)
  type t = (int * Word.word Array.array)

  infixr 0 $
  fun f $ x = f x

  fun numWords bits = ((bits + (Word.wordSize - 1)) div Word.wordSize)
  fun wordOffset bNum = bNum div Word.wordSize
  fun bitOffset bNum = bNum mod Word.wordSize

  (* Start with double size to allow for quick resizes. *)
  fun new size = (size, Array.array ((numWords size) * 2, 0w0))

  (* doubleSize initially appears linear, if you only look at the else
     clause, but it's actually a linear function -- the input is consumed. *)
  fun doubleSize (s, a) = if (numWords (2*s)) <= (Array.length a) then (2*s, a)
    else let
      val newSize = 2*s
      val newArr = Array.array (numWords newSize, 0w0)
      val _ = Array.copy {src=a, dst=newArr, di=0}
    in
      (newSize, newArr)
    end
  
  fun set (arr as (siz, a), i) =
    if i >= siz then set (doubleSize arr, i) else
    let
      val newVal = Word.orb(Array.sub (a, wordOffset i), Word.<<(0w1, Word.fromInt $ bitOffset i))
    in
      Array.update (a, wordOffset i, newVal); arr
    end

  val neg1 = Word.notb 0w0

  exception Size
  (* Compare two bitvecs, returning true iff bv1 | bv2 == ~0. *)
  fun orFills ((s1, a1), (s2, a2)) =
    let
      val _ = if s1 <> s2 then raise Size else ()
      
      (* numFullWords is the index of the highest full word, or ~1 if no such exists.
       * Consider 0; wordOffset 0 = 0, numFullWords = ~1.
       * Consider size-1; wordOffset size-1 = 0; numFullWords = ~1.
       * Consider size; wordOffset size = 1; numFullWords = 0.
       *)
      val numFullWords = (wordOffset s1) - 1
      fun foldFulls (~1) = true
        | foldFulls n =
          if Word.orb (Array.sub (a1, n), Array.sub (a2, n)) <> neg1
          then false
          else foldFulls (n-1)

      val fulls = foldFulls numFullWords
      
      (* bitOffset 0 is 0; 1<<0 - 1 = 0; OK.
       * bitOffset 1 is 1; 1<<1 - 1 = 1; OK.
       * bitOffset 2 is 2; 1<<2 - 1 = 3; OK.
       *)
      val lasts = Word.-(Word.<<(0w1, Word.fromInt $ bitOffset s1), 0w1) =
                  Word.orb(Array.sub(a1, wordOffset s1), Array.sub(a2, wordOffset s1))
    in
      fulls andalso lasts
    end

  fun test () = let
    val v1 = new 64
    val bits = List.tabulate (64, fn x => x)
    val evens = List.filter (fn x => x mod 2 = 0) bits
    val odds = List.filter (fn x => x mod 2 <> 0) bits
    val v2 = List.foldr (fn (i, v) => set (v, i)) v1 bits
    val v3 = List.foldr (fn (i, v) => set (v, i)) v1 evens
    val v4 = List.foldr (fn (i, v) => set (v, i)) v1 odds
  in
    List.all (fn x => x) [
      Assert.assert "!(0 | 0)" (not(orFills(v1, v2))),
      Assert.assert "0 | ~0" (orFills(v1, v2)),
      Assert.assert "evens | odds" (orFills(v3, v4)),
      Assert.assert "!(evens | 0)" (not(orFills(v3, v1))),
      Assert.assert "odds | ~0" (orFills(v4, v2)),
      Assert.assert "!(0 | odds)" (not(orFills(v1, v4))),
      Assert.assert "~0 | evens" (orFills(v1, v3)),
      true
    ]
  end

end
