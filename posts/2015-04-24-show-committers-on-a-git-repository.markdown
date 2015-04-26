---
title: Show Committers on a Git Repository
author: sanjiv sahayam
description: How to show the committers on a git repository along with the number of commits they have made.
tags: git
---

How is git so awesome? Today I was wondering how I'd go about listing all the unique committers to a git repository. As usual there was some git fu that I had yet to learn:

```{.command}
git shortlog -ns
```

Running this on the Scala [Wartremover repository](https://github.com/puffnfresh/wartremover) returns the following:

```{.terminal}
   126  Brian McKenna
    15  Michal Rus
     9  Michał Rus
     7  Rob Norris
     7  George Leontiev
     6  Nikolay Stanchenko
     6  Pedro Furlanetto
     3  Maxwell Swadling
     3  Jonathan Ferguson
     2  xuwei-k
     2  James Earl Douglas
     2  Dale Wijnand
     2  Cody Allen
     2  Zach Weatherby
     1  Leif Warner
     1  Peter Hausel
     1  Lars Hupel
     1  Jason Zaugg
     1  ChrisNeveu
     1  Vladimir Kostyukov
```

If you want to further order by email use:

```{.command}
git shortlog -nes
```

enjoy! :)