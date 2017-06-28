---
title: Re-run Compilation with Deprecations Turned on from within SBT
author: sanjiv sahayam
description: How to re-run compilation from within SBT with deprecations turned on.
tags: sbt, scala,
comments: true
---

Often while compiling some Scala project from with SBT, I'd see the following error:

```{.command .scrollx}
re-run with -deprecation for details
```

And then I would have to go and update my __build.sbt__ file with with following:

```{.scala .scrollx}
scalacOptions ++= Seq(
                      "-deprecation",
                      //other options
                   )
```

and then relaunch SBT and perform the compilation again. That's a little painful.

I stumbled across this [incantation](https://stackoverflow.com/questions/9578521/scala-sbt-how-to-re-run-with-deprecation?rq=1) to achieve the same result from right within SBT:

```{.scala .scrollx}
set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")
```

You can then kick off a new compilation with:

```{.scala .scrollx}
;clean;compile
```

to see any deprecation warnings. And that's all there is to it. :)