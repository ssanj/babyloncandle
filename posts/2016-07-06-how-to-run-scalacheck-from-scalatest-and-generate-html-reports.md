---
title: How to Run ScalaCheck from ScalaTest and Generate HTML Reports
author: sanjiv sahayam
description: Running ScalaCheck from within ScalaTest gives you some niceties such as HTML report generation. Here's how to get started.
tags: sbt, scala, scalacheck, scalatest
comments: true
---

It would seem that ScalaCheck does not have a way to generate HTML output for tests, as [ScalaTest](http://scalatest.org) or [Specs2](http://etorreborre.github.io/specs2) does. In order to get some of this functionality I decided to run ScalaCheck through ScalaTest and get the HTML reporting for free.

## The Problem

Reading the [ScalaTest documentation for Property-Based Testing](http://www.scalatest.org/user_guide/property_based_testing) left me a little baffled as to which classes to use as there didn't seem to be a full example of a property-based specification in the documentation. This is quite unusual given the high quality of ScalaTest documentation.

## Example

In any event [I managed to find a working example](https://github.com/oscarrenalias/scalacheck-cookbook/blob/master/markdown/scalacheck-integration.md#using-scalacheck-with-scalatest) and it turned out it was quite straightforward.

I thought I'd use a simple example in ScalaCheck and then discuss how to convert that example into ScalaTest's property-based tests.

I've taken the example from the [Quick Start section of the ScalaCheck website](http://scalacheck.org/#quickstart) (and fixed the broken property):

```{.scala .scrollx}
package net.ssanj.blog

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringProp extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length >= a.length && (a+b).length >= b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }
}
```

You can run this by issuing a __test__ command in SBT:

```{.terminal .scrollx}
[info] + String.substring: OK, passed 100 tests.
[info] + String.startsWith: OK, passed 100 tests.
[info] + String.concatenate: OK, passed 100 tests.
```

Errors are displayed as:

```{.scala .scrollx}
[info] ! String.concatenate: Falsified after 0 passed tests.
[info] > ARG_0: ""
[info] > ARG_1: ""
```

How can we convert the above property test into ScalaTest property test?

ScalaTest has two flavours of property-based testing:

1. The ScalaTest Style (Generator-Driven)
1. ScalaCheck Style

Here are some guidelines on how to convert the above example to the ScalaTest style:

1. Extend PropSpec
1. Extend PropertyChecks
1. Extend Matchers (or other matching DSL)
1. Convert each property assignment to a property method
1. Convert each Boolean operation/Prop to a match

```{.scala .scrollx}
package net.ssanj.blog

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

final class StringSpec extends PropSpec with PropertyChecks with Matchers {

  property("startsWith") {
    forAll { (a: String, b: String) =>
      (a+b) should startWith (a)
    }
  }

  property("concatenate") {
    forAll { (a: String, b: String) =>
      (a+b).length should (be >= a.length)
      (a+b).length should (be >= b.length)
    }
  }

  property("substring") {
    forAll { (a: String, b: String, c: String) =>
      (a+b+c).substring(a.length, a.length+b.length) should be (b)
    }
  }
}
```

Running the above gives us:

```{.terminal .scrollx}
[info] StringSpec:
[info] - startsWith
[info] - concatenate
[info] - substring
```

Errors are displayed as:

```{.terminal .scrollx}
[info] - concatenate *** FAILED ***
[info]   TestFailedException was thrown during property evaluation.
[info]     Message: 0 was not greater than 0
[info]     Location: (StringSpec.scala:16)
[info]     Occurred when passed generated values (
[info]       arg0 = "",
[info]       arg1 = ""
[info]     )
```

In ScalaCheck any property that returns a Boolean is automatically converted into a Prop and then evaluated when executed. When using the ScalaTest flavour of property-based testing, you need to use matchers instead of Boolean properties.

Here are some guidelines on how to convert the example to the ScalaCheck style:

1. Extend PropSpec
1. Extend Checkers
1. Convert each property assignment to a property method
1. Wrap the outer forAll method call with the check method.

```{.scala .scrollx}
package net.ssanj.blog

import org.scalacheck.Prop

import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

final class StringCheck extends PropSpec with Checkers {

  property("startsWith") {
    check(Prop.forAll { (a: String, b: String) =>
      (a+b).startsWith(a)
    })
  }

  property("concatenate") {
    check(Prop.forAll { (a: String, b: String) =>
      (a+b).length >= a.length && (a+b).length >= b.length
    })
  }

  property("substring") {
    check(Prop.forAll { (a: String, b: String, c: String) =>
      (a+b+c).substring(a.length, a.length+b.length) == b
    })
  }
}
```

Running the above gives us:

```{.terminal .scrollx}
[info] StringCheck:
[info] - startsWith
[info] - concatenate
[info] - substring
```

Errors as displayed as:

```{.scala .scrollx}
[info] StringCheck:
[info] - String.startsWith
[info] - concatenate *** FAILED ***
[info]   GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
[info]    (StringCheck.scala:17)
[info]     Falsified after 0 successful property evaluations.
[info]     Location: (StringCheck.scala:17)
[info]     Occurred when passed generated values (
[info]       arg0 = "",
[info]       arg1 = ""
[info]     )
[info] - substring
```

As you can see, this style is much closer to the default ScalaCheck property style as properties can be left as Boolean expressions.

The diagram below outlines the two different styles and some of the basic methods provided by each of the traits.

![Summary of the different property-based testing styles](/images/scalatest_scalacheck_summary_of_different_property-based_testing_styles.png)

## HTML Output

1. StringSpec has the following HTML output:

![StringSpec](/images/scalacheck_scalatest_stringspec.jpg)

2. StringCheck has the following HTML output:

![StringCheck](/images/scalacheck_scalatest_stringcheck.jpg)

The output is identical irrespective of the property-based style used.

## Caveats with Imports

If you use ScalaCheck version 0.13.x with ScalaTest 2.2.x you'll run into an IncompatibleClassChangeError:

```{.terminal .scrollx}
[info] Exception encountered when attempting to run a suite with class name: org.scalatest.DeferredAbortedSuite *** ABORTED ***
[info]   java.lang.IncompatibleClassChangeError: Implementing class
[info]   at java.lang.ClassLoader.defineClass1(Native Method)
[info]   at java.lang.ClassLoader.defineClass(ClassLoader.java:760)
[info]   at java.security.SecureClassLoader.defineClass(SecureClassLoader.java:142)
[info]   at java.net.URLClassLoader.defineClass(URLClassLoader.java:467)
[info]   at java.net.URLClassLoader.access$100(URLClassLoader.java:73)
[info]   at java.net.URLClassLoader$1.run(URLClassLoader.java:368)
[info]   at java.net.URLClassLoader$1.run(URLClassLoader.java:362)
[info]   at java.security.AccessController.doPrivileged(Native Method)
[info]   at java.net.URLClassLoader.findClass(URLClassLoader.java:361)
[info]   at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
[info]   ...
```

To solve this ensure that you use ScalaCheck version 0.12.x with ScalaTest 2.2.x

The examples use the following combination:

```{.scala .scrollx}
libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"   % "2.2.6"  % "test",
  "org.scalacheck" %% "scalacheck"  % "1.12.5" % "test"
)
```

The full [source](https://github.com/ssanj/scalacheck-on-scalatest) can be found on Github.