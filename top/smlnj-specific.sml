signature SUQ =
sig
  val Word32_lsh : Word32.word * Word32.word -> Word32.word
  val Word32_rsh : Word32.word * Word32.word -> Word32.word
end

structure Suq :> SUQ =
struct
  fun loseBit (x: Word32.word) : word = Word31.fromInt (Word32.toInt x)
  fun Word32_lsh (a, b) = Word32.<< (a, loseBit b)
  fun Word32_rsh (a, b) = Word32.~>> (a, loseBit b)
end
