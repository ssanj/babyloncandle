---
title: Set SBT log level to Debug
author: sanjiv sahayam
description: How to set SBT to log at Debug level.
tags: sbt, scala
comments: true
---

If you need to set the log level for [SBT](http://www.scala-sbt.org) in the console you can do:

```{.terminal .scrollx}
set logLevel := Level.Debug
```

Alternatively within an .sbt file you can:

```{.terminal .scrollx}
logLevel := Level.Debug
```

The [available levels](http://www.scala-sbt.org/release/api/index.html#sbt.Level$) are:

```{.terminal .scrollx}
Debug
Info
Warn
Error
```