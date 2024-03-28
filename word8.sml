
structure Word8Extras : WORD_EXTRAS where type word = Word8.word =
struct
  structure W8 = Word8

  type word = W8.word

  fun popCount w =
    let
      val w = w - W8.andb (0wx55, W8.>> (w, 0w1))
      val w = W8.andb (0wx33, w) + W8.andb (0wx33, W8.>> (w, 0w2))
    in
      W8.toIntX (W8.andb (0wxf, w + W8.>> (w, 0w4)))
    end

  fun trailingZeros 0w0 = 8
    | trailingZeros w =
        let
          val w = W8.andb (W8.notb w, w - 0w1)
          val w = w - W8.andb (0wx55, W8.>> (w, 0w1))
          val w = W8.andb (0wx33, W8.>> (w, 0w2)) + W8.andb (0wx33, w)
        in
          W8.toIntX (W8.andb (0wxf, w + W8.>> (w, 0w4)))
        end

  fun leadingZeros 0w0 = 8
    | leadingZeros w =
        let
          val w = W8.orb (w, W8.>> (w, 0w1))
          val w = W8.orb (w, W8.>> (w, 0w2))
          val w = W8.orb (w, W8.>> (w, 0w4))
        in
          popCount (W8.notb w)
        end

  fun bitReverse w =
    let
      val w = W8.orb (W8.>> (w, 0w4),  W8.<< (w,  0w4))
      val w = W8.orb (W8.andb (0wx33, W8.>> (w, 0w2)), W8.<< (W8.andb (0wx33, w), 0w2))
      val w = W8.orb (W8.andb (0wx55, W8.>> (w, 0w1)), W8.<< (W8.andb (0wx55, w), 0w1))
    in
      w
    end

  fun rotateLeft (w, s) =
    W8.orb (W8.<< (w, Word.andb (0w7, s)), W8.>> (w, Word.andb (0w7, ~s)))

  fun rotateRight (w, s) =
    W8.orb (W8.>> (w, Word.andb (0w7, s)), W8.<< (w, Word.andb (0w7, ~s)))
end
