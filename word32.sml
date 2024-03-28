
structure Word32Extras : WORD_EXTRAS where type word = Word32.word =
struct
  structure W32 = Word32

  type word = W32.word

  fun popCount w =
    let
      val w = w - W32.andb (0wx55555555, W32.>> (w, 0w1))
      val w = W32.andb (0wx33333333, w) + W32.andb (0wx33333333, W32.>> (w, 0w2))
      val w = W32.andb (0wx0f0f0f0f, w + W32.>> (w, 0w4))
    in
      W32.toIntX (W32.>> (0wx01010101 * w, 0w24))
    end

  fun leadingZeros 0w0 = 32
    | leadingZeros w =
        let
          (* Set every bit after the highest bit *)
          val w = W32.orb (w, W32.>> (w, 0w1))
          val w = W32.orb (w, W32.>> (w, 0w2))
          val w = W32.orb (w, W32.>> (w, 0w4))
          val w = W32.orb (w, W32.>> (w, 0w8))
          val w = W32.orb (w, W32.>> (w, 0w16))
        in
          (* Count the number of remaining zeros *)
          popCount (W32.notb w)
        end

  (* http://supertech.csail.mit.edu/papers/debruijn.pdf *)
  local
    val deBruijn : W32.word = 0wx077cb531
    val shift : Word.word = 0w27
    (*
    val ctzTbl =
      let
        val arr = CharArray.array (32, #"\000")
        fun loop i =
          if i >= 32
            then ()
          else
            let
              val idx = W32.>> (W32.<< (deBruijn, Word.fromInt i), shift)
            in
              CharArray.update (arr, W32.toIntX idx, Char.chr i);
              loop (i + 1)
            end
      in
        loop 0;
        CharArray.vector arr
      end
    *)
    val ctzTbl = "\^@\^A\^\\^B\^]\^N\^X\^C\^^\^V\^T\^O\^Y\^Q\^D\b\^_\^[\r\^W\^U\^S\^P\a\^Z\f\^R\^F\v\^E\n\t"
  in
    fun trailingZeros 0w0 = 32
      | trailingZeros w =
      let
        val idx = W32.>> (W32.andb (w, ~w) * deBruijn, shift)
      in
        Char.ord (Unsafe.CharVector.sub (ctzTbl, W32.toIntX idx))
      end
  end

  fun bswap w =
    W32.orb (
      W32.orb (W32.<< (w, 0w24),
               W32.<< (W32.andb (0wxff00, w), 0w8)),
      W32.orb (W32.andb (0wxff00, W32.>> (w, 0w8)),
               W32.>> (w, 0w24)))

  fun bitReverse w =
    let
      (* Reverse the order of each byte
       * ABCDEFGH IJKLMNOP QRSTUVWX YZ012345
       * ->
       * YZ012345 QRSTUVWX IJKLMNOP ABCDEFGH *)
      val w = bswap w
      (* Swap each nibble
       * YZ01 2345 QRST UVWX IJKL MNOP ABCD EFGH
       * ->
       * 2345 YZ01 UVWX QRST MNOP IJKL EFGH ABCD *)
      val w = W32.orb (
        W32.andb (0wx0f0f0f0f, W32.>> (w, 0w4)),
        W32.<< (W32.andb (0wx0f0f0f0f, w), 0w4))
      (* Swap each crumb
       * 23 45 YZ 01 UV WX QR ST MN OP IJ KL EF GH AB CD
       * ->
       * 45 23 01 YZ WX UV ST QR OP MN KL IJ GH EF CD AB *)
      val w = W32.orb (
        W32.andb (0wx33333333, W32.>> (w, 0w2)),
        W32.<< (W32.andb (0wx33333333, w), 0w2))
      (* Swap each bit
       * 45 23 01 YZ WX UV ST QR OP MN KL IJ GH EF CD AB
       * ->
       * 54 32 10 ZY XW VU TS RQ PO NM LK JI HG FE DC BA *)
      val w = W32.orb (
        W32.andb (0wx55555555, W32.>> (w, 0w1)),
        W32.<< (W32.andb (0wx55555555, w), 0w1))
    in
      w
    end

  fun rotateLeft (w, s) =
    W32.orb (W32.<< (w, Word.andb (0wx1f, s)), W32.>> (w, Word.andb (0wx1f, ~s)))

  fun rotateRight (w, s) =
    W32.orb (W32.>> (w, Word.andb (0wx1f, s)), W32.<< (w, Word.andb (0wx1f, ~s)))
end
