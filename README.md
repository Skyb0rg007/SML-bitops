
### Reference Implementation for SML bitops

This implements:

- `popCount : word -> int`
  + Implements [Hamming Weight](https://en.wikipedia.org/wiki/Hamming_weight)
  + Assembly primitive on many platforms: x86 (`popcnt`), arm (`cnt`), risc-v (`cpop`)
  + Already implemented in SML/NJ
  + Algorithm is pretty standard
- `leadingZeros : word -> int`
  + Implements [Count Leading Zeros](https://en.wikipedia.org/wiki/Find_first_set)
  + Returns `wordSize` on input `0w0`
  + Assembly primitive on many platforms: x86 (`lzcnt`), arm (`clz`), risc-v (`clz`)
  + Algorithm delegates to `popCount`
    - Implementation is used by `Clang` and `GCC` to implement `__builtin_popcount`
- `trailingZeros : word -> int`
  + Implements [Count Trailing Zeros](https://en.wikipedia.org/wiki/Find_first_set)
  + Returns `wordSize` on input `0w0`
  + Assembly primitive on many platforms: x86 (`tzcnt`), arm (`ctz`), risc-v (`ctz`)
  + Algorithm based on DeBruijn indices
    - Reference: http://supertech.csail.mit.edu/papers/debruijn.pdf
    - This is a pretty standard implementation strategy
- `rotateLeft : word * Word.word -> word`
- `rotateRight : word * Word.word -> word`
  + Implements [Circular Shifts](https://en.wikipedia.org/wiki/Circular_shift)
  + Assembly primitive on some platforms: x86 (`rol`, `ror`)
  + Not a complicated implementation, but requires some care to handle over-shifts
  + Used in many hashing algorithms
  + Implemented as an LLVM primitive, and efficient implementation relies
    on knowing `wordSize` and over-shift behavior.
  + Allows for the use of unsafe shift operator in 8,32,64 bit implementations
- `bitReverse : word -> word`
  + Implements [Bit-Reversal Permutation](https://en.wikipedia.org/wiki/Bit-reversal_permutation)
  + Used in certain algorithms such as FFT
  + Assembly primitive on some platforms: arm (`rbit`)
  + Algorithm uses bswap + swaps for 8, 32, 64 bits
    - This is used by `Clang` to implement `__builtin_bitreverse`
  + Use ternary swaps for 63 bits
    - This avoids conversion to 64 bits
    - Taken from this blog: https://matthewarcus.wordpress.com/2012/11/18/reversing-a-64-bit-word/
- `bswap : word -> word`
  + Only implemented for 32 and 64 bits
  + Assembly primitive on some platforms: x86 (`bswap`), arm (`rev`).
  + Used internally in `bitReverse` implementation
  + Useful, but only makes sense if `(wordSize mod 8) = 0` and `(wordSize div 8) mod 2 = 0`
    - Most uses are already taken care of from `PackWordN(Big|Little)` structures
