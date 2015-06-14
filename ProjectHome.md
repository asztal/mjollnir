Mjöllnir is an interpreter for the Fjölnir language, written in Haskell, which is now approaching usable. The original Fjölnir compiler is written for DOS and accepts input only in CP861. Mjöllnir has more modern features such as unicode source files, a REPL (actually, I broke that. I'll put it back¹.), and eventually² compilation to LLVM bytecode, while keeping as close to the original language as possible.

This program has more or less been superseded by my new project, my [Fjolnir compiler](http://code.google.com/p/fjolnir-compiler).

¹ Probably not any time soon.
² Never.

## About Fjölnir ##

Fjölnir is an Icelandic programming language once taught in the Icelandic University, _Haskólí Íslands_. The entire programming language and all of its documentation is in Icelandic. This makes it somewhat hard to implement.

The language's main features are algebraic package combinators and heterogeneous lists.

## Compiling ##

Compile with `ghc --make Mjöllnir.hs -O -W -fwarn-orphans -fwarn-tabs -fwarn-type-defaults -funbox-strict-fields -fexcess-precision` (preferably on GHC 6.10, but it should probably work on 6.8 -- at the time of writing it doesn't work with GHC 6.12) after installing these cabbages:
  * convertible
  * utf8-string
  * haskeline

```
*
"GRUNNUR"
;
```