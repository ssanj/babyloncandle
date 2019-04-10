---
title: What is SBT Doing?
author: sanjiv sahayam
description: If SBT just sits there doing nothing, then this might help you figure out why.
tags: sbt, scala
comments: true
---

Sometimes when you start up SBT and run a command, it sits there doing nothing for a long while. Wouldn't it be nice to see what's going or where it's spending all its time?

It turns out you can. Simply start SBT in debug mode:

```{.command .scrollx}
sbt --debug
```

Then rerun your command and watch all the information fly by about what SBT is up to.