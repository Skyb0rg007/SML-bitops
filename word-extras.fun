(* Generic implementation *)

functor WordExtrasFn(W : WORD) : WORD_EXTRAS where type word = W.word =
struct
  type word = W.word

  val zero = W.fromInt 0
  val one = W.fromInt 1
  val highMask = W.<< (one, Word.fromInt (W.wordSize - 1))

  fun popCount w =
    let
      fun loop (w, acc) =
        if W.< (w, one)
          then acc
          else loop (W.andb (w, W.- (w, one)), acc + 1)
    in
      loop (w, 0)
    end

  fun trailingZeros w =
    let
      fun loop (i, mask) =
        if i >= W.wordSize
          then i
        else if W.> (W.andb (w, mask), zero)
          then i
        else loop (i + 1, W.<< (mask, 0w1))
    in
      loop (0, one)
    end

  fun leadingZeros w =
    let
      fun loop (i, mask) =
        if i >= W.wordSize
          then i
        else if W.> (W.andb (w, mask), zero)
          then i
        else loop (i + 1, W.>> (mask, 0w1))
    in
      loop (0, highMask)
    end

  fun bitReverse w =
    let
      fun loop (0, _, acc) = acc
        | loop (i, w, acc) =
            loop (i - 1, W.>> (w, 0w1), W.orb (W.<< (acc, 0w1), W.andb (w, one)))
    in
      loop (W.wordSize, w, zero)
    end

  val wordSize = Word.fromInt W.wordSize

  fun rotateLeft (w, s) =
    let
      val r = Word.mod (s, wordSize)
    in
      W.orb (W.<< (w, r), W.>> (w, wordSize - r))
    end

  fun rotateRight (w, s) =
    let
      val r = Word.mod (s, wordSize)
    in
      W.orb (W.>> (w, r), W.<< (w, wordSize - r))
    end
end
