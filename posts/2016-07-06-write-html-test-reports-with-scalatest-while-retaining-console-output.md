---
title: Write Html Test Reports in ScalaTest While Retaining Console Output
author: sanjiv sahayam
description: Turning on Html output for your ScalaTests will turn off your console output. This posts outlines how to turn them both on.
tags: sbt,scala,scalatest
comments: true
---

If you are using [ScalaTest](http://www.scalatest.org) with [SBT](http://www.scala-sbt.org) and need to write out Html reports for your tests, add the following incantation to your build.sbt:

```{.scala .scrollx}
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
```

The above will write out Html reports to your __target/test-reports__ directory.

Unfortunately this stops the writing of any test successes or failures to the console. You just get a message of the type: "x Tests Failed". You have to then rummage around the Html reports to figure out what happened. Not ideal.

To get both the console output and the Html output for your tests, add the following incantation to your build.sbt:

```{.scala .scrollx}
testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest, "-o"), Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports"))
```

Read the [full list of ScalaTest options](http://www.scalatest.org/user_guide/using_the_runner) for more configurations.

If you get the following NoClassDefFoundError error when generating reports:

```{.scala .scrollx}
[error] (test:executeTests) java.lang.NoClassDefFoundError: org/pegdown/PegDownProcessor
```

add the pegdown library to your dependencies:

```{.scala .scrollx}
  "org.pegdown"    %  "pegdown"     % "1.6.0"  % "test"
```

Now you can have the best of both worlds!