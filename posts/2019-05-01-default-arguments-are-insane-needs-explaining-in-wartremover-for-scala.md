---
title: &quot;Default arguments are insane&quot; needs explanation in Wartremover for Scala
author: sanjiv sahayam
description: What are the pitfalls of using default arguments?
tags: scala, linting, wartremover
comments: true
---

There is a very long issue under [Wartremover](https://github.com/wartremover) titled: [&quot;Default arguments are insane&quot; needs explanation](https://github.com/wartremover/wartremover/issues/116). I personally love using Wartremover as it definitely increases the quality of any Scala code I write. The issues around using Default Arguments or
[Default Parameter Values](https://docs.scala-lang.org/tour/default-parameter-values.html) (as Scala refers to them) are somewhat subtle. The Wartemover issue seems to go on forever, but there are lots of really great ideas in there and I though I could summarise some of them here.

## What is a Default Parameter Value?

> Scala provides the ability to give parameters default values that can be used to allow a caller to omit those parameters.

Here's a quick example of Default Parameter Values (DPV):

```{.scala .scrollx}
def log(message: String, level: String = "INFO") = println(s"$level: $message")

log("System starting")  // prints INFO: System starting
log("User not found", "WARNING")  // prints WARNING: User not found
```

One of the main benefits of DPV are that you as the programmer don't need to supply all the parameters to a function - just the ones that are mandatory. This sounds like a really useful idea, so why are people recommending that we don't use it?

## Issues

### 1. Unclear Code

Here's an example of some code that uses DPV:

```{.scala .scrollx}
val streamName: String = ...
KinesisStream.fromUrl(streamName)
```

Now if you just read the above function it looks like it might be doing the wrong thing. Why are we supplying a Stream name to a function that clearly states that it needs a Url?

The function is defined as:

```{.scala .scrollx}
KinesisStream.fromUrl(streamName: String, url: Option[String] = None) ...
```

The default value for `url` hides the true nature of what this function needs. The `url` parameter has been made optional because under some circumstances it is not needed.

### 2. ~~Breaks Currying and Partial Application~~

[Rob Norris](https://twitter.com/tpolecat) has a nice article on [Methods are not Functions](https://tpolecat.github.io/2014/06/09/methods-functions.html) which covers why currying and partial Application of Functions is broken with DPV. Here's a simple example:

```{.scala .scrollx}
def log(message: String, level: String = "INFO") = println(s"$level: $message")

scala> log("hello!") //one param
INFO: hello!

scala> log("hello!", "WARN") //both params
WARN: hello!

//can we map over log?
scala> val messages = List("hello", "world")
messages: List[String] = List(hello, world)

 //does not work
scala> messages.map(log)
<console>:14: error: type mismatch;
 found   : (String, String) => Unit
 required: String => ?
       messages.map(log)
                    ^
 //does not work
scala> messages.map(log _)
<console>:14: error: type mismatch;
 found   : (String, String) => Unit
 required: String => ?
       messages.map(log _)

//works!
scala> messages.map(log(_))
INFO: hello
INFO: world
res6: List[Unit] = List((), ())

//also works
scala> messages.map(x => log(x))
INFO: hello
INFO: world
res7: List[Unit] = List((), ())
```

Weird. So it seems like you can use Currying and Partial Application if you tweak the syntax a little.

Let's have a go with Rob's example:

```{.scala .scrollx}
scala> def foo(n: Int = 3, s: String) = s * n
foo: (n: Int, s: String)String

scala> foo _
res17: (Int, String) => String = $$Lambda$1191/1542722356@4e216b7e

//does not work
scala> foo(42)
<console>:13: error: not enough arguments for method foo: (n: Int, s: String)String.
Unspecified value parameter s.
       foo(42)

//works
scala> foo(42, _:String)
res19: String => String = $$Lambda$1192/1172016038@6c826924

//can we get defaults for free?
scala> val f1 = foo _
f1: (Int, String) => String = $$Lambda$1193/1526085518@205b3a1a

//does not work
scala> f1(10)
<console>:13: error: not enough arguments for method apply: (v1: Int, v2: String)String in trait Function2.
Unspecified value parameter v2.
       f1(10)

//have to supply all arguments
scala> f1(10, "*")
res21: String = **********
```

This seems to be a moot issue. While the syntax is more awkward than necessary, Currying and Partial Application is certainly possible with DPV.

Another way to partially apply functions with default parameters is create a wrapper function with only the mandatory fields:

```{.scala .scrollx}
scala> def foo(n: Int = 3, s: String) = s * n
foo: (n: Int, s: String)String

//wrap foo with foo2
scala> def foo2(s: String) = foo(s = s)
foo2: (s: String)String

scala> foo2("#")
res29: String = ###

//now we can use foo2 in higher-order functions
scala> messages.map(foo2)
res30: List[String] = List(hellohellohello, worldworldworld)
```

### 3. Bugs of Convenience

In a project I worked on we had some asynchronous tasks that split a workload into chunks using a sliding window of ten elements. Here's a simplified version of the issue:

```{.scala .scrollx}
scala> val elements = (1 to 30).toList
elements: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

scala> val it = elements.sliding(10)
it: Iterator[List[Int]] = non-empty iterator

scala> it.next
res22: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

it.next ??? //what does this print?
it.next ??? //what does this print?
```

We witnessed a subtle bug where performance of our processing pipeline was terrible. What was going on?

The actual definition of `sliding` is:

```{.scala .scrollx}
def sliding[B >: A](size: Int, step: Int = 1): GroupedIterator[B]
```

Notice the default **step** of 1. When we used the `sliding` function we assumed that the `size` supplied would also be the step. Everything compiled and there were no warnings. Here is the output of the above example:

```{.scala .scrollx}
scala> it.next
res22: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

scala> it.next
res23: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

scala> it.next
res24: List[Int] = List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
```

As you can see we were processing the same items multiple times. If the `step` parameter were explicit this would not have happened. Once the mistake was corrected:

```{.scala .scrollx}
scala> val it = elements.sliding(10, 10)
it: Iterator[List[Int]] = non-empty iterator

scala> it.next
res25: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

scala> it.next
res26: List[Int] = List(11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

scala> it.next
res27: List[Int] = List(21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
```

we can see it sliding over ten elements at a time as expected. These kind of bugs are hard to find. We lean on the compiler a lot to point out our mistakes. With DPV while our lives are more convenient because we have less parameters to supply our functions, the compiler fails to see our errors and omissions and can't help.

### 4. Bugs due to Refactoring

Consider an API at version 1.x that has the following function:

```{.scala .scrollx}
object Database
  def fromUrl(url: String): ...
}
```

Now in the client of the library goes ahead and uses the above function:

```{.scala .scrollx}
Database.fromUrl("someUrl")
```

The developer of the library decides to add a `tableName` parameter as the `url` optional when run locally.
Not wanting to introduce an additional method for this our developer then decides to make `url` a DPV.

As this is a breaking change, he bumps the version of the library to 2.x.

```{.scala .scrollx}
object Database {
  def fromUrl(tableName: String, url: Option[String] = None): ...
}
```

Now this all seems fine. The major version of the library has been bumped so it indicates potential for a breaking change.

Unfortunately, the client code still compiles after moving to version 2.x of the library:

```{.scala .scrollx}
Database.fromUrl("someUrl")
```

But now we have problem. Since the location of the `url` parameter has changed, we are supplying a url as the `tableName` parameter, and the compiler can't inform us that anything is broken. We have to find out at runtime that we have a problem.

## Alternate Designs

Here are some alternate ways not to use DPV.

### Supply all Parameters

Take away developer convenience for software correctness. Get the developer to supply all parameters. We can change the `log` function from:

```{.scala .scrollx}
def log(message: String, level: String = "INFO") = println(s"$level: $message")
```

to:

```{.scala .scrollx}
def log(message: String, level: String) = println(s"$level: $message")
```

Callers of the `log` method have to now supply both the `message` and the `level`:

```{.scala .scrollx}
log("System starting", "INFO")
```
Better yet, convert the `level` parameter to an ADT so that the callers can't pass through invalid values.

However this technique can get tedious if you have a lot of parameters that you don't really care about.


### Breakout Separate Methods

If you don't want to supply all the parameters each time, consider creating separate methods for the parameters you do care about.

In the case of the bug with the Database refactoring, we could have pulled out some extra methods:

```{.scala .scrollx}
object Database {
  def remote(url: DatabaseUrl): ...
  def remoteWithTable(tableName: String, url: DatabaseUrl): ...
  def local(tableName: String): ...
}
```

Rob Norris on splitting out methods:

>  If you have a method with a single default arg you could reasonably suggest splitting it into two methods (as you might do with a single option or boolean arg)


### Have a Default Config Object

If you have a lot of parameters to your function (and this might be a problem by itself) you could use a config default object.

Take `tcpConnect` as an example:

```{.scala .scrollx}
def tcpConnect(host: String, port: Int = 80, sslEncryption: Boolean = false, localAddress: Option[String] = None):  String = "connected"
```

This could be re-written with a default object:


```{.scala .scrollx}
//original method with defaults removed
def tcpConnect(host: String, port: Int, sslEncryption: Boolean, localAddress: Option[String]):  String = "connected"

//config class
case class TcpConnection(host: String, port: Int, sslEncryption: Boolean, localAddress: Option[String])

//function that calls tcpConnect
def fromTcpConnection(tcpConnection: TcpConnection): String =
  tcpConnect(tcpConnection.host, tcpConnection.port, tcpConnection.sslEncryption, tcpConnection.localAddress)

//default object
def defaultTcpConnection: TcpConnection = TcpConnction(host: String = "localhost", port: Int = 80, sslEncryption: Boolean = false, localAddress: Option[String] = None)

//usage for a specific URL
fromTcpConnection(defaultTcpConnection.copy(host = "http://...."))

//usage for a specific port
fromTcpConnection(defaultTcpConnection.copy(port = 8080))

//usage for a secure URL
fromTcpConnection(defaultTcpConnection.copy(host = "https://...", port = 443, sslEncryption = true))
```

Rob Norris on using default config objects:

> ... right, but then you have the awful copy method to contend with, then you add lenses, then you add phantom types to ensure that options haven't been set more than once, etc., etc., and I'm not convinced that the complexity is warranted, given the lack thus far of any convincing reason not to use default args

If none of these alternates seem attractive, go ahead and use DPV but think hard about how it may introduce bugs into your code base.

## Parting Thoughts

The following is a [summary](https://github.com/wartremover/wartremover/issues/116#issuecomment-51326211) of why not to use DPV by [Mark Hibberd](https://twitter.com/markhibberd):

1. Allocation of resources (there are even examples of this in scalaz) - which is utterly wrong. Anything that is allocated by a default argument has no reasonable lifecycle and is unlikely (or impossible) to be closed properly.
1. Default configurations - these are a developer convenience that lead to operational bugs. There is no such thing as a "safe" default, where it could mean forgetting to set something in production leads to an incorrect value rather than an error (this is closely related to what Minsky says as mentioned by Eric above).
1. Common arguments through delegating methods - these are representative of what @maxpow4h originally stated. That if you have multiple methods with optional arguments, it is extremely easy for incorrect code to compile by forgetting to delegate one of the arguments.
1. Faux overloading - it is cool to hate on overloading so I will avoid it by using named arguments with defaults, ending up with the exact same situation. Code that is subtly wrong (such as forgetting to pass argument) still compiles. This is not an acceptable situation.

Eric Torreborre on when to use DPV:

> So my own conclusion is that default arguments (and overloading) still have some value (for non-critical DSLs) but you need to be very careful where you use them.

Mark Hibberd on focussing on correct programs:

> But the most troublesome part of this thread, is that almost all of the discussion is about what developers find "convenient" and aesthetically pleasing, when we should be asking how a language feature adds or removes from our ability to build robust, correct programs - and, as quickly as possible. When held in this light, default arguments do not hold up. They are a mere syntactic convenience - that does not help us with this goal. This might be ok, if they didn't come with risk or issues, but even the gentler arguments in this thread should be enough to highlight their use in a linting tool - especially given their inherent lack of motivation to begin with.

> But yeh. Everyone gets to live in their own teams codebases. I just prefer mine without these undue risks.