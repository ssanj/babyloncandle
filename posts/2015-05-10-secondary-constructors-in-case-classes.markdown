---
title: Secondary Constructors in Scala Case Classes
author: sanjiv sahayam
description: How to define a secondary constructor on/for a Scala case class.
tags: scala
---

Recently I needed to add a secondary constructor to a case class and came across a strange problem. The secondary constructor was not visible.

Given the following case class:

```{.scala}
final case class SomeThing(x:String)
```

I can call it as follows:

```{.scala}
SomeThing("test")
```

I add a secondary constructor that takes in an Int:

```{.scala}
final case class SomeThing(x:String) {
    def this(n:Int) = this(n.toString)
}
```
When I try to use use the secondary constructor:

```{.scala}
SomeThing(3)
```

I get the following compiler error:

```{.terminal}
Error:(19, 23) type mismatch;
 found   : Int(3)
 required: String
    SomeThing(3)
              ^
```

It looks like the primary constructor that takes in a String can be seen but not the secondary one that takes in an Int. What's the problem?

This post on [case class auxiliary constructors](http://www.scala-lang.org/old/node/976) explains how to work around it:

> at present you have to use "new" on any constructor except the primary.

Right. So we can call the secondary constructor like so:

```{.scala}
new SomeThing(3)
```

That's a bit inconsistent.

[This SO article](http://stackoverflow.com/questions/2400794/overload-constructor-for-scalas-case-classes) explains that defining a secondary constructor on the companion object would remove any need for using the __new__ keyword from the calling site:

```{.scala}
 object SomeThing {
    def apply(b:Boolean) = new SomeThing(b.toString)
 }

 SomeThing(true)
```

The above workaround gives the impression that the secondary constructor behaves much like the primary constructor. This seems to be the "cleanest" solution for using secondary constructors, at least for now.