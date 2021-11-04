## Compiling and Executing Haskell Code

- Compile Haskell source file (extensions `.hs`) using the Haskell compiler GHC, 
  then execute the resulting program in the shell:

  ~~~
  $ ghc --make ‹file›.hs                     # compile source ‹file›.hs into executable program ‹file›
  $ ./‹file›                                 # run program ‹file› 
  ~~~

  Alternatively, load `‹file›.hs` into the Haskell REPL GHCi and evaluate at GHCI's prompt:

  ~~~
  $ ghci ‹file›.hs
  GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
  [1 of 1] Compiling Main             ( ‹file›.hs, interpreted )
  Ok, one module loaded.  
  *Main> main                                -- invoke function main 
  ~~~

  Another option:

  ~~~
  $ ghci
  GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
  Prelude› :l ‹file.hs›                       -- load source file ‹file.hs› into GHCi
  [1 of 1] Compiling Main             ( ‹file›.hs, interpreted )
  Ok, one module loaded.
  Main> main                                  -- invoke function main
  …
  Main> :r                                    -- after ‹file›.hs has been edited, reload into GHCi
  ~~~

- Compile C source files (extension `.c`, will rarely pop up in this course) using the C compiler `cc`,
  then execute the resulting program in the shell:

  ~~~
  $ cc ‹file›.c -o ‹file›                     # compile source ‹file›.c into executable program ‹file›
  $ ./‹file›                                  # run program ‹file›
  ~~~
