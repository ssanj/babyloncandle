---
title: Run a Test Method of a Test Class from SBT
author: sanjiv sahayam
description: How to run a single test method of a test class using SBT.
tags: sbt, scala, sublimeide
comments: true
---

As [I use Sublime Text for most of my Scala development](http://sanj.ink/posts/2015-07-15-using-sublime-for-scala-development.html) at the mo, I've had to forego some of the niceties that Intellij brought to the table. One of those niceties was running a single test method of a test class. 

I do all my compilation and running-of-tests through an interactive SBT session. I knew how to run only a single test with __testOnly__. 

Here's an example of running only the [ListTest](https://github.com/puffnfresh/wartremover/blob/latest-release/core/src/test/scala/wartremover/warts/ListTest.scala) class in the [Wartremover project](https://github.com/puffnfresh/wartremover):

```{.terminal .scrollx}
> testOnly org.brianmckenna.wartremover.test.ListTest
[info] ListTest:
[info] - can't use List#head on List
[info] - can't use List#tail on List
[info] - can't use List#init on List
[info] - can't use List#last on List
[info] - can't use List#reduce on List
[info] - can't use List#reduceLeft on List
[info] - can't use List#reduceRight on List
[info] - ListOps wart obeys SuppressWarnings
[info] Run completed in 211 milliseconds.
[info] Total number of tests run: 8
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 8, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
```

What I did not know was how to run a single test method within a test class. This functionality depends on the test framework in use.

## ScalaTest ##


The incantation for [ScalaTest](http://www.scalatest.org) is:

```{.terminal .scrollx}
testOnly *YourTestClass -- -z "name of the test you want to run"
```

To run only the __can't use List#reduce on List__ test within the ListTest class:

```{.terminal .scrollx}
> testOnly org.brianmckenna.wartremover.test.ListTest -- -z "can't use List#reduce on List"
[info] ListTest:
[info] - can't use List#reduce on List
[info] Run completed in 90 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
```

## Specs2 ##

The incantation for [Specs2](https://etorreborre.github.io/specs2) is:

```{.terminal .scrollx}
testOnly *YourTestClass -- -ex "name of the test you want to run"
```

Taking the [pirate](https://github.com/markhibberd/pirate) project as an example, we can run only the [NondetTSpec](https://github.com/markhibberd/pirate/blob/master/src/test/scala/pirate.internal/NondetTSpec.scala) with:

```{.terminal .scrollx}
> testOnly pirate.internal.NondetTSpec
[info] NondetTSpec
[info]
[info]   NondetT Laws
[info]   ============
[info]
[info]   + NondetT is an equal (for tests)
[info]   + NondetT is a monad
[info]   + NondetT is a monad plus
[info]   + NondetT is a strong monad plus
[info]
[info]
[info] Total for specification NondetTSpec
[info] Finished in 9 ms
[info] 4 examples, 400 expectations, 0 failure, 0 error
[info] Passed: Total 4, Failed 0, Errors 0, Passed 4
```

Then to run only the __NondetT is a monad plus__ test:

```{.terminal .scrollx}
> testOnly pirate.internal.NondetTSpec -- -ex "NondetT is a monad plus"
[info] NondetTSpec
[info]
[info]
[info]   NondetT Laws
[info]   ============
[info]
[info]
[info]
[info]   + NondetT is a monad plus
[info]
[info]
[info]
[info] Total for specification NondetTSpec
[info] Finished in 9 ms
[info] 1 example, 100 expectations, 0 failure, 0 error
[info] Passed: Total 1, Failed 0, Errors 0, Passed 1
```

Having differing parameters to each test frameworks is laborious. It would be nice if SBT provided some support for at least ScalaTest and Specs2 out of the box.

_I got some clues on how to make these work from [here](https://github.com/sbt/sbt/issues/911)_.