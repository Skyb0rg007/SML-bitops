
structure Word64Extras : WORD_EXTRAS where type word = Word64.word =
struct
  structure W64 = Word64

  type word = W64.word

  fun popCount w =
    let
      val w = w - W64.andb (0wx5555555555555555, W64.>> (w, 0w1))
      val w = W64.andb (0wx3333333333333333, w) + W64.andb (0wx3333333333333333, W64.>> (w, 0w2))
      val w = W64.andb (0wx0f0f0f0f0f0f0f0f, w + W64.>> (w, 0w4))
    in
      W64.toIntX (W64.>> (0wx0101010101010101 * w, 0w56))
    end

  fun leadingZeros 0w0 = 64
    | leadingZeros w =
        let
          val w = W64.orb (w, W64.>> (w, 0w1))
          val w = W64.orb (w, W64.>> (w, 0w2))
          val w = W64.orb (w, W64.>> (w, 0w4))
          val w = W64.orb (w, W64.>> (w, 0w8))
          val w = W64.orb (w, W64.>> (w, 0w16))
          val w = W64.orb (w, W64.>> (w, 0w32))
        in
          popCount (W64.notb w)
        end

  (* http://supertech.csail.mit.edu/papers/debruijn.pdf *)
  local
    val deBruijn : W64.word = 0wx07EDD5E59A4E28C2
    val shift : Word.word = 0w58
    (*
    val ctzTbl =
      let
        val arr = CharArray.array (64, #"\000")
        fun loop i =
          if i >= 64
            then ()
          else
            let
              val idx = W64.>> (W64.<< (deBruijn, Word.fromInt i), shift)
            in
              CharArray.update (arr, W64.toIntX idx, Char.chr i);
              loop (i + 1)
            end
      in
        loop 0;
        CharArray.vector arr
      end
    *)
    val ctzTbl = "?\^@:\^A;/5\^B<'0\^[6!*\^C=3%(1\^R\^\\^T7\^^\"\v+\^N\^V\^D>9.4&\^Z )2$\^Q\^S\^]\n\r\^U8-\^Y\^_#\^P\t\f,\^X\^O\b\^W\a\^F\^E"
  in
    fun trailingZeros 0w0 = 64
      | trailingZeros w =
      let
        val idx = W64.>> (W64.andb (w, ~w) * deBruijn, shift)
      in
        Char.ord (Unsafe.CharVector.sub (ctzTbl, W64.toIntX idx))
      end
  end

  fun bswap w =
    W64.orb (
      W64.orb (
        W64.orb (
          W64.>> (W64.andb (0wxff00000000000000, w), 0w56),
          W64.>> (W64.andb (0wx00ff000000000000, w), 0w40)),
        W64.orb (
          W64.>> (W64.andb (0wx0000ff0000000000, w), 0w24),
          W64.>> (W64.andb (0wx000000ff00000000, w), 0w8))),
      W64.orb (
        W64.orb (
          W64.<< (W64.andb (0wx00000000ff000000, w), 0w8),
          W64.<< (W64.andb (0wx0000000000ff0000, w), 0w24)),
        W64.orb (
          W64.<< (W64.andb (0wx000000000000ff00, w), 0w40),
          W64.<< (W64.andb (0wx00000000000000ff, w), 0w56))))

  fun bitReverse w =
    let
      (* Reverse each byte *)
      val w = bswap w
      (* Swap each nibble *)
      val w = W64.orb (
        W64.andb (0wx0f0f0f0f0f0f0f0f, W64.>> (w, 0w4)),
        W64.<< (W64.andb (0wx0f0f0f0f0f0f0f0f, w), 0w4))
      (* Swap each crumb *)
      val w = W64.orb (
        W64.andb (0wx3333333333333333, W64.>> (w, 0w2)),
        W64.<< (W64.andb (0wx3333333333333333, w), 0w2))
      (* Swap each bit *)
      val w = W64.orb (
        W64.andb (0wx5555555555555555, W64.>> (w, 0w1)),
        W64.<< (W64.andb (0wx5555555555555555, w), 0w1))
    in
      w
    end

  fun rotateLeft (w, s) =
    W64.orb (W64.<< (w, Word.andb (0wx3f, s)), W64.>> (w, Word.andb (0wx3f, ~s)))

  fun rotateRight (w, s) =
    W64.orb (W64.>> (w, Word.andb (0wx3f, s)), W64.<< (w, Word.andb (0wx3f, ~s)))
end
