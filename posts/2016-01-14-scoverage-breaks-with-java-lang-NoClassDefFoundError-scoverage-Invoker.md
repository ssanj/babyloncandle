---
title: Scoverage Breaks with java.lang.No Class Def Found Error&colon; scoverage &sol; Invoker&dollar;
author: sanjiv sahayam
description: Running instrumented code with Scoverage through SBT throws a java.lang.NoClassDefFoundError&colon;&nbsp;scoverage&sol;Invoker&dollar; error.
tags: sbt, scala, scoverage
comments: true
---

If you've been using [Scoverage](https://github.com/scoverage/sbt-scoverage) with [SBT](http://www.scala-sbt.org) to get some code coverage on your Scala code, you may come across an error while trying to run a main class on instrumentation turned on:

```{.scala .scrollx}
java.lang.NoClassDefFoundError: scoverage/Invoker$
```

This is quite puzzling as the tests run fine. I came across a possible [Github issue](https://github.com/scoverage/sbt-scoverage/issues/84) while hunting around for a solution. It sounds like there is a runtime dependency on Scoverage classes.

One way to overcome this issue, is to turn off coverage when running your main class through SBT:

```{.terminal .scrollx}
;clean;coverageOff;run
```

If you're running SBT externally you can use:

```{.terminal .scrollx}
sbt clean coverageOff run
```
When you want your coverage back on for your tests, you can use:

```{.terminal .scrollx}
;clean;coverage;test
```
Or externally:

```{.terminal .scrollx}
sbt clean coverage test
```

[&commat;alexflav23](https://github.com/alexflav23) recommends turning off coverage when publishing or packaging:

 > You need to disable coverage with coverageOff before publishing or packaging and everything will be just fine. I've ran into this issue myself and that was the quick fix.