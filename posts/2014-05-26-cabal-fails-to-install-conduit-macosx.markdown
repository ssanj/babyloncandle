---
title: Cabal Fails to Install Conduit on Macosx
author: sanjiv sahayam
description: You may need alternate GCC to compile the conduit library.
tags: cabal, haskell, macosx
---

When trying to install [Hakyll](http://jaspervdj.be/hakyll) on GHC 7.6.3 with cabal 1.20.0.0 on Mac OSX 10.9.2 the __conduit__ dependency failed with the following error:

    Failed to install conduit-1.1.3

After googling around for a bit I came across this thread on [Stackoverflow](https://github.com/snoyberg/conduit/issues/147) which lead me to to [Manuel's blog](http://justtesting.org/post/64947952690/the-glasgow-haskell-compiler-ghc-on-os-x-10-9) article which explains how to get things working. Basically there are some incompatibilities between Apple's version of gcc and that used by GHC:

> Apple finally dropped the GNU C Compiler (GCC) from its developer tools and only supports the LLVM-based clang compiler. This causes the Glasgow Haskell Compiler (GHC) some grief, mainly due to its use of the C pre-processor (cpp) as a cheap macro system for Haskell.


To overcome this issue you need to do the following:

1. Download Luke Iannini’s [gcc](http://www.cse.unsw.edu.au/~chak/haskell/clang-xcode5-wrapper) wrapper

2. Copy it into /usr/local/bin

3. Make it executable

4. Edit the GHC settings file found at:

```
/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/ghc-7.6.3/settings
```


5. Change the "C compiler command" to use the wrapper:

```
("C compiler command", "/usr/local/bin/clang-xcode5-wrapper"),
```

And you're done.

You should now be able to install conduit in peace:

    cabal install conduit
