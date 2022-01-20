## Notes on the Material

- Files `validate.hs` and `Validation.hs` contain a sample  
  application of type class `Applicative` (module `Validation`
  is imported by `validate.hs`).

  To understand function `validateAccount` in `validate.hs`,
  consider (with curried function`g :: a -> b -> c`):

  ~~~
                    f a   f b                      f b
                     │     │                        │
          pure g <*> x <*> y  ≡  (pure g <*> x) <*> y
          └──┬─┘                 └──────┬─────┘
             │                          │
      f (a -> b -> c)               f (b -> c)  
  ~~~
