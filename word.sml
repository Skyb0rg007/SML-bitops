(* Note: This only works if Word.wordSize = 63
 * 32-bit installations of SML/NJ (when Word.wordSize = 31)
 * require a different implementation
 *)
structure Word63Extras : WORD_EXTRAS where type word = Word.word =
struct
  structure W64 = Word64
  structure W = Word

  type word = W.word

  fun popCount w = Word64Extras.popCount (W64.fromLarge (W.toLarge w))

  fun leadingZeros w = Word64Extras.leadingZeros (W64.fromLarge (W.toLarge w)) - 1

  fun trailingZeros 0w0 = 63
    | trailingZeros w = Word64Extras.trailingZeros (W64.fromLarge (W.toLarge w))

  (* This method avoids conversion to 64-bit
   * https://matthewarcus.wordpress.com/2012/11/18/reversing-a-64-bit-word/ *)
  fun bitReverse w =
    let
      fun swapBits (m, k, p) =
        let
          val q = W.andb (m, W.xorb (p, W.>> (p, k)))
        in
          W.xorb (p, W.xorb (q, W.<< (q, k)))
        end
      val w = swapBits (0wx1249249249249249, 0w2, w)
      val w = swapBits (0wx01C0E070381C0E07, 0w6, w)
      val w = swapBits (0wx00001FF0000001FF, 0w18, w)
      val w = swapBits (0wx0000000007FFFFFF, 0w36, w)
    in
      w
    end

  fun rotateLeft (w, s) =
    let
      val r = Word.mod (s, 0w63)
    in
      W.orb (W.<< (w, r), W.>> (w, 0w63 - r))
    end

  fun rotateRight (w, s) =
    let
      val r = Word.mod (s, 0w63)
    in
      W.orb (W.>> (w, r), W.<< (w, 0w63 - r))
    end
end
