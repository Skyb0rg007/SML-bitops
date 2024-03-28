
signature WORD_EXTRAS =
sig
  type word

  val popCount : word -> int
  val trailingZeros : word -> int
  val leadingZeros : word -> int
  val bitReverse : word -> word
  val rotateLeft : word * Word.word -> word
  val rotateRight : word * Word.word -> word
end
