signature SUQ =
sig
  structure Word32 : sig
    type word = Word32.word
    val << : Word32.word * Word32.word -> Word32.word
    val ~>> : Word32.word * Word32.word -> Word32.word
  end

  structure Word64 : sig
    type word = Word64.word
    val << : Word64.word * Word32.word -> Word64.word
    val >> : Word64.word * Word32.word -> Word64.word
  end
end

structure Suq :> SUQ =
struct
  fun loseBit (x: Word32.word) : word = Word31.fromInt (Word32.toInt x)
  
  structure Word32 = struct
    type word = Word32.word
    fun << (a, b) = Word32.<< (a, loseBit b)
    fun ~>> (a, b) = Word32.~>> (a, loseBit b)
  end
  
  structure Word64 = struct
    type word = Word64.word
    fun << (a, b) = Word64.<< (a, loseBit b)
    fun >> (a, b) = Word64.>> (a, loseBit b)
  end
end
