
functor TestAgainstStub(
  structure W : WORD
  structure Ex : WORD_EXTRAS where type word = W.word
  val random : unit -> W.word
  val randWord : unit -> word
) =
  struct
    structure D = WordExtrasFn(W)

    fun fail (f, w) =
      raise Fail
        (f ^ " failed on 0wx" ^ W.toString w ^ " (wordSize = " ^ Int.toString W.wordSize ^ ")")

    fun testPopCount 0 = ()
      | testPopCount n =
        let
          val w = random ()
          val a = D.popCount w
          val b = Ex.popCount w
        in
          if a = b
            then testPopCount (n - 1)
          else (
            print ("a = " ^ Int.toString a ^ "\n");
            print ("b = " ^ Int.toString b ^ "\n");
            fail ("popCount", w))
        end

    fun testTrailingZeros 0 = ()
      | testTrailingZeros n =
        let
          val w = random ()
          val a = D.trailingZeros w
          val b = Ex.trailingZeros w
        in
          if a = b
            then testTrailingZeros (n - 1)
          else fail ("trailingZeros", w)
        end

    fun testLeadingZeros 0 = ()
      | testLeadingZeros n =
        let
          val w = random ()
          val a = D.leadingZeros w
          val b = Ex.leadingZeros w
        in
          if a = b
            then testLeadingZeros (n - 1)
          else fail ("leadingZeros", w)
        end

    fun testBitReverse 0 = ()
      | testBitReverse n =
        let
          val w = random ()
          val a = D.bitReverse w
          val b = Ex.bitReverse w
        in
          if a = b
            then testBitReverse (n - 1)
          else fail ("bitReverse", w)
        end

    fun fail' (f, w, s, a, b) =
      raise Fail
        (f ^ " failed on w = 0wx" ^ W.toString w ^ " (wordSize = " ^ Int.toString W.wordSize ^ ") and s = 0w" ^ Word.fmt StringCvt.DEC s ^ ", " ^ W.toString a ^ ", " ^ W.toString b)

    fun testRotateLeft 0 = ()
      | testRotateLeft n =
        let
          val w = random ()
          val s = randWord ()
          val a = D.rotateLeft (w, s)
          val b = Ex.rotateLeft (w, s)
        in
          if a = b
            then testRotateLeft (n - 1)
          else fail' ("rotateLeft", w, s, a, b)
        end

    fun testRotateRight 0 = ()
      | testRotateRight n =
        let
          val w = random ()
          val s = randWord ()
          val a = D.rotateRight (w, s)
          val b = Ex.rotateRight (w, s)
        in
          if a = b
            then testRotateRight (n - 1)
          else fail' ("rotateRight", w, s, a, b)
        end

    fun test n =
      (testPopCount n;
       testTrailingZeros n;
       testLeadingZeros n;
       testBitReverse n;
       testRotateLeft n;
       testRotateRight n)
  end

structure Test =
  struct
    val rng = Random.rand (0xdeadbeef, 0xcafebabe)

    structure TestWord = TestAgainstStub(
      structure W = Word
      structure Ex = Word63Extras
      fun random () = Random.randWord rng
      fun randWord () = Random.randWord rng
    )
    structure TestWord8 = TestAgainstStub(
      structure W = Word8
      structure Ex = Word8Extras
      fun random () = Word8.fromInt (Random.randRange (0, 0x100) rng)
      fun randWord () = Random.randWord rng
    )
    structure TestWord32 = TestAgainstStub(
      structure W = Word32
      structure Ex = Word32Extras
      fun random () = Word32.fromInt (Random.randRange (0, 0x100000000) rng)
      fun randWord () = Random.randWord rng
    )
    structure TestWord64 = TestAgainstStub(
      structure W = Word64
      structure Ex = Word64Extras
      fun random () = Random.randNativeWord rng
      fun randWord () = Random.randWord rng
    )

    fun test n =
      (TestWord8.test n;
       TestWord32.test n;
       TestWord64.test n;
       TestWord.test n)
  end
