## Notes on the Material

- In `bits.hs`, note that function `isEven` only consumes the leading (least significant)
  bit of the bit string `bits n` (lazy evaluation).  In GHCi:

  ~~~
  > bs = bits 1904
  bs :: Bits
  > :sprint bs
  bs = _
  > head bs == 0       -- this is what isEven evaluates internally
  True
  it :: Bool
  › :sprint bs
  bs = 0 : _  
  ~~~

- File `mergesort.hs` implements the well-known MergeSort algorithm and 
  provides a larger example of list processing and
  pattern matching.  `mergesort (>) xs` uses comparison operator `>` to
  compare the elements of list `xs` (and will perform a descending sort,
  use `(<)` to sort in ascending order).

  - Built-in function `splitAt n xs` splits list `xs` at position `n`:
    `splitAt 3 [1..10] ≡ ([1,2,3],[4,5,6,7,8,9,10])`.
  - The pattern `[x]` matches single-element lists (whose only element
    is bound to variable `x`).
  - The **layered pattern** `l1@(x:xs)` matches non-empty lists only and
    1. binds `x` to the head of that list,
    2. binds `xs` to the tail of that list, and
    3. binds `l1` to the entire list.
