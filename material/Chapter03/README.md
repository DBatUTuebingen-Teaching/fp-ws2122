## Notes on the Material

Since this chapter features a variety of Haskell source files and
modules, here's a quick summary of which source files import
which module:


|      Haskell source       |                          Imported module(s)                             |
| ------------------------- | ----------------------------------------------------------------------- |
| `library-exposed.hs`      | —                                                                       |
| `rose.hs`                 | `M`                                                                     |
| `set-language-shallow.hs` | `SetLanguageShallow1`, `SetLanguageShallow2`, `SetLanguageShallow2Card` |
| `set-language-deep.hs`    | `SetLanguageDeep`, `SetLanguageDeepCard`                                |
| `expr-language.hs`        | `ExprDeep`                                                              |
| `expr-deep-num.hs`        | `ExprDeepNum`                                                           |
| `expr-language-typed.hs`  | `ExprDeepGADTTyped`                                                     |
| `expr-embedding.hs`       | `ExprEmbeddings`                                                        |
| `pattern-match.hs`        | `PatternMatching`                                                       |

