---
title: Cross-compile Multiple Versions of Scala via SBT
author: sanjiv sahayam
description: How to build a Scala project cross multiple versions.
tags: sbt, scala
comments: true
---

To start supporting multiple versions of Scala within a project you need to cross compile your sources for the various supported Scala versions.

You can achieve that with the __crossScalaVersions__ directive within your SBT build file specifying all the Scala versions you want to support:

```{.scala .scrollx}
crossScalaVersions := Seq("2.11.8", "2.12.2")
```

But how do you go about supporting APIs that exist in one version but not the other version of Scala within the same project?

Since SBT 0.13.8 (thanks to this [PR](https://github.com/sbt/sbt/pull/1799)) you can simply separate your __src__ directories to versioned equivalents and have everything just work as expected.

For example to support custom code for Scala 2.11 and 2.12, you'd create the following directories:

1. src/{main,test}/__scala-2.11__ //for 2.11 specific changes
1. src/{main,test}/__scala-2.12__ //for 2.12 specific changes

The default src/{main,test}/__scala__ directory will be used for common changes.

When compiling your project simply use the __+__ directive before a task to denote cross building the project across all Scala versions for that task. For example to run tests across all versions use:

```{.command .scrollx}
+ test
```

To switch to a specific Scala version use __++__. For example to switch to Scala 2.12 use:

```{.command .scrollx}
++ 2.12
```

When compiling source for the different Scala versions, they will be written out to the respective target directories:

1. target/__scala-2.11__ //for 2.11 classes
1. target/__scala-2.12__ //for 2.12 classes

For more information read the [SBT documentation on cross building](http://www.scala-sbt.org/0.13/docs/Cross-Build.html).