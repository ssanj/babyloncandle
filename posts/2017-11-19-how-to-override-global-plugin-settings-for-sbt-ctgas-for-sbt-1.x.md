---
title: How to Override Global Plugin Settings for for SBT 1.x
author: sanjiv sahayam
description: Overriding global plugin settings in SBT 1.x is different to SBT 0.13.x. This post shows how to do that for SBT 1.x.
tags: sbt, scala, sublimeide, sublime
comments: true
---

The way you configure global plugins for SBT 1.x is different to how it used to be for SBT 0.13.x. In a previous article on [How to Browse Scala Sources of your Dependencies from Sublime](http://sanj.ink/posts/2015-08-22-how-to-browse-scala-sources-of-your-dependencies-from-sublime.html) I recommended creating global plugin to override the __dependencySrcUnzipDir__ setting by creating a __CustomCtagsSrcDir.scala__ file under __~/.sbt/0.13/plugins__:

```{.scala .scrollx}
import sbt._
import Keys._
import net.ceedubs.sbtctags.CtagsKeys._

object CustomCtagsSrcDir extends Plugin {
  override def settings = Seq(
    dependencySrcUnzipDir := baseDirectory.value / ".ctags_srcs"
  )
}
```

This different to how it needs to be done in SBT 1.x.

To use [sbt-ctags](https://github.com/ceedubs/sbt-ctags) with SBT 1.x first clone the [SBT 1x Compatibility](https://github.com/ceedubs/sbt-ctags/pull/20) PR that publishes sbt-ctags for SBT 1.x.

Next we need to publish this locally as the PR has not been merged as of this writing. This will install the sbt-ctags-0.2.1-SNAPSHOT:

```{.scala .scrollx}
sbt publishLocal
```

Add the sbt-ctags plugin to your __~/.sbt/1.0/plugins/plugins.sbt__ file to enable it globally:

```{.scala .scrollx}
addSbtPlugin("net.ceedubs" %% "sbt-ctags" % "0.2.1-SNAPSHOT")
```

Next override the location of your __dependencySrcUnzipDir__ directory in __~/.sbt/1.0/global.sbt__ globally:

```{.scala .scrollx}
SbtCtagsKeys.dependencySrcUnzipDir := baseDirectory.value / ".ctags_srcs"
```


This is really neat. No more creating unnecessary classes to override settings.