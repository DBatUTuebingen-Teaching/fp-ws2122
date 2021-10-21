## Compilation und Ausführung

- Haskell-Source-Files (Endung `.hs`) übersetzt ihr einfach mit dem Haskell-Compiler GHC und führt das Programm in der Shell aus:

  ~~~
  $ ghc --make ‹file›.hs                     # übersetzt Source ‹file›.hs in ausführbares Programm ‹file›
  $ ./‹file›                                 # Programm ‹file› ausführen
  ~~~

  Alternativ könnt ihr `‹file›.hs` auch in die Haskell-REPL GHCi laden und dort ausführen:

  ~~~
  $ ghci ‹file›.hs
  GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
  [1 of 1] Compiling Main             ( ‹file›.hs, interpreted )
  Ok, one module loaded.  
  *Main> main                                -- Funktion main aufrufen
  ~~~

  Weitere Option:

  ~~~
  $ ghci
  GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
  Prelude› :l ‹file.hs›                       -- Source ‹file.hs› in GHCi laden
  [1 of 1] Compiling Main             ( ‹file›.hs, interpreted )
  Ok, one module loaded.
  Main> main                                  -- Funktion main aufrufen
  …
  Main> :r                                    -- Source ‹file›.hs nach Änderung neu laden
  ~~~

- C-Source-Files (Ending `.c`, davon wird's kaum welche geben in dieser Vorlesung) einfach mit einem C-Compiler übersetzen und dann ausführen:

  ~~~
  $ cc ‹file›.c -o ‹file›                     # übersetzt Source ‹file›.c in ausführbares Programm ‹file›
  $ ./‹file›                                  # Programm ‹file› ausführen
  ~~~
