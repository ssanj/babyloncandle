---
title: How to Run a Specific Main Class with Parameters through SBT
author: sanjiv sahayam
description: How to run a specific main class through SBT when you've got multiple main classes.
tags: sbt, scala
comments: true
---

Say you've got a number of main classes in your SBT project. Issuing a

```{.command .scrollx}
> run
```
from SBT in [interactive mode](http://www.scala-sbt.org/0.13.5/docs/Howto/interactive.html) will display a menu with options to run one of the many main classes detected:

```{.terminal .scrollx}
 [1] net.ssanj.stm.refs.OneOfTwoValuesChangedAfterOneRead
 [2] net.ssanj.stm.tmap.AReadValueHasChangesAndAnotherKeyIsRead
 [3] net.ssanj.stm.tmap.AReadValueHasChangesAndIsReadAgain
 [4] net.ssanj.stm.tmap.AReadValueHasChangesNothingElseIsRead
 [5] net.ssanj.stm.tmap.AValueIsReadAndAnotherValueIsAdded
 [6] net.ssanj.stm.tmap.LostUpdateAdditionalRead
 [7] net.ssanj.stm.tmap.LostUpdateNoAdditionalRead
 [8] net.ssanj.stm.tmap.RetainTest
 [9] net.ssanj.stm.tmap.ThrowsException
```

You can also do the same from the command line with:

```{.terminal .scrollx}
sbt run
```

What if you needed to run a specific class but with some parameters?
You can use:

```{.command .scrollx}
> runMain package.path.to.main.class param1 param2
```
or

```{.command .scrollx}
> run-main package.path.to.main.class param1 param2
```

in interactive mode.

To run from the command line you can use:

```{.terminal .scrollx}
sbt "runMain package.path.to.main.class param1 param2"
```

or

```{.terminal .scrollx}
sbt "run-main package.path.to.main.class param1 param2"
```

Although the above methods work, they are a bit tedious because you have to copy the full class path to the class you want to run. It would be nice to run a main class directly through menu with the some parameters.
Something of the sort:

```{.command .scrollx}
> run
 [1] net.ssanj.stm.refs.OneOfTwoValuesChangedAfterOneRead
 [2] net.ssanj.stm.tmap.AReadValueHasChangesAndAnotherKeyIsRead
 [3] net.ssanj.stm.tmap.AReadValueHasChangesAndIsReadAgain
 [4] net.ssanj.stm.tmap.AReadValueHasChangesNothingElseIsRead
 [5] net.ssanj.stm.tmap.AValueIsReadAndAnotherValueIsAdded
 [6] net.ssanj.stm.tmap.LostUpdateAdditionalRead
 [7] net.ssanj.stm.tmap.LostUpdateNoAdditionalRead
 [8] net.ssanj.stm.tmap.RetainTest
 [9] net.ssanj.stm.tmap.ThrowsException

> 2 "param1 param2"
```