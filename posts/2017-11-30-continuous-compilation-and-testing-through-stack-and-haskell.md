---
title: Continuous Compilation and Testing through Stack and Haskell
author: sanjiv sahayam
description: How to enable continuous running of commands on file changes.
tags: haskell, stack
comments: true
---

One of the features I missed when coming from a Scala/[SBT](https://www.scala-sbt.org) toolset to a [Stack](https://docs.haskellstack.org)/Haskell toolset was the continuous running of tasks in SBT, through the __~__ operator. For example to continuously compile production sources we could use:

```{.command .scrollx}
~compile
```

from within SBT.

For a while there I was stumped about how to do the same for Stack. That was until I stumbled across it in the documentation under [Build Synonyms](https://docs.haskellstack.org/en/stable/GUIDE/#the-build-synonyms):

```{.command .scrollx}
stack build --pedantic --haddock --test --exec "echo Yay, it succeeded" --file-watch
```

The flag of interest is __--file--watch__. Now I can continuously watch for file changes against library or test sources and run commands accordingly:

```{.command .scrollx}
stack build --file-watch
```

or

```{.command .scrollx}
stack test --file-watch
```

respectively.