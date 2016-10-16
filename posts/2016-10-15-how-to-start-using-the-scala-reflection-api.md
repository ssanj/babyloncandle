---
title: How to Start Using the Scala Reflection Api
author: sanjiv sahayam
description: Getting started with the scala reflection api can be confusing and foreign. This provides you with some simple ways to start using the reflection api productively.
tags: reflection, scala
comments: true
---

I've found learning the [scala reflection API](http://www.scala-lang.org/api/2.11.1/scala-reflect/index.html#scala.reflect.api.package) somewhat confusing. Having used Java reflection pretty easily back in the day, the Scala api seemed somewhat foreign.

So let's start with a simple exercise to learn how to use the api.

# A Simple Exercise #

How would we go about using the scala reflection api to find out what methods are declared on a type?

## 1. Import the reflection universe##

First, you need to import the reflection runtime universe:

```{.scala .scrollx}
import scala.reflect.runtime.universe._
```

Most methods on the reflection api are centred around a __Type__:

```{.scala .scrollx}
reflect.runtime.universe.Type
```

_I'll refer to Type as universe.Type from now on, to distinguish it from a normal type_.

## 2. Get the universe.Type ##

To get the universe.Type of a type, you can use the __typeOf__ api method:

```{.scala .scrollx}
typeOf[Option[_]]
res1: reflect.runtime.universe.Type = scala.Option[_]
```

## 3. Get the declared methods ##

Now that we have a universe.Type for our type, we can get the methods defined on it by using the __decls__ method:

```{.scala .scrollx}
res1.decls
res9: reflect.runtime.universe.MemberScope = SynchronizedOps(constructor Option, method isEmpty, method isDefined, method get, method getOrElse, method orNull, method map, method fold, method flatMap, method flatten, method filter, method filterNot, method nonEmpty, method withFilter, class WithFilter, method contains, method exists, method forall, method foreach, method collect, method orElse, method iterator, method toList, method toRight, method toLeft)
```

You might notice that __decls__ returns a __MemberScope__. What's that? It's handy to realise that a __MemberScope__ is a __Traversable__:

```{.scala .scrollx}
res9.isInstanceOf[Traversable[_]]
res12: Boolean = true
```

You can use the [methods available on any Traversable](http://www.scala-lang.org/files/archive/nightly/docs/library/index.html#scala.collection.Traversable) instance to process the MemberScope.

For instance, we could easily format the list of method declarations like so:

```{.scala .scrollx}
res1.decls.mkString("\n")
res11: String =
def <init>(): Option[A]
def isEmpty: Boolean
def isDefined: Boolean
def get: A
final def getOrElse[B >: A](default: => B): B
final def orNull[A1 >: A](implicit ev: <:<[Null,A1]): A1
final def map[B](f: A => B): Option[B]
final def fold[B](ifEmpty: => B)(f: A => B): B
final def flatMap[B](f: A => Option[B]): Option[B]
def flatten[B <: <?>](implicit ev: <?>): Option[B]
final def filter(p: A => Boolean): Option[A]
final def filterNot(p: A => Boolean): Option[A]
final def nonEmpty: Boolean
final def withFilter(p: A => Boolean): Option.this.WithFilter
class WithFilter extends AnyRef
final def contains[A1 <: <?>](elem: <?>): Boolean
final def exists(p: A => Boolean): Boolean
final def forall(p: A => Boolean): Boolean
final def foreach[U](f: A => U): Unit
final def collect[B](pf: PartialFunction[A,B]): Option[B]
final def orElse[B >: A](alternative: => Option[B]): Option[B]
def iterator: Iterator[A]
def toList: List[A]
final def toRight[X](left: => X): Product with Serializable with scala.util.Either[X,A]
final def toLeft[X](right: => X): Product with Serializable with scala.util.Either[A,X]
```

# Other useful methods #

Let's use the reflection api to figure out what other methods are available on universe.Type. We use the __members__ method to list methods defined either directly or indirectly on universe.Type:

```{.scala .scrollx}
typeOf[Type]
res5: reflect.runtime.universe.Type = scala.reflect.runtime.universe.Type

res5.members.mkString("\n")
res7: String =
final def ##(): Int
def contains(sym: <?>): Boolean
def exists(p: <?>): Boolean
def find(p: <?>): Option[Types.this.Type]
def foreach(f: <?>): Unit
def map(f: <?>): Types.this.Type
def substituteTypes(from: <?>,to: <?>): Types.this.Type
def substituteSymbols(from: <?>,to: <?>): Types.this.Type
def orElse(alt: <?>): Types.this.Type
def finalResultType: Types.this.Type
def resultType: Types.this.Type
def typeParams: List[Types.this.Symbol]
def paramLists: List[List[Types.this.Symbol]]
def paramss: List[List[Types.this.Symbol]]
def typeArgs: List[Types.this.Type]
def dealias: Types.this.Type
def widen: Types.this.Type
def erasure: Types.this.Type
def asSeenFrom(pre: <?>,clazz: <?>): Types.this.Type
def baseType(clazz: <?>): Types.this.Type
def baseClasses: List[Types.this.Symbol]
def =:=(that: <?>): Boolean
def weak_<:<(that: <?>): Boolean
def <:<(that: <?>): Boolean
def etaExpand: Types.this.Type
def normalize: Types.this.Type
def typeConstructor: Types.this.Type
def takesTypeArgs: Boolean
def companion: Types.this.Type
def members: Types.this.MemberScope
def member(name: <?>): Types.this.Symbol
def decls: Types.this.MemberScope
def declarations: Types.this.MemberScope
def decl(name: <?>): Types.this.Symbol
def declaration(name: Types.this.Name): Types.this.Symbol
def typeSymbol: Types.this.Symbol
def termSymbol: Types.this.Symbol
```

# Getting a universe.Type from an Instance #

What if you have an instance of a type and want to get a universe.Type for that? It looks like there is no built in method to do that. The [recommended way](http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html) is to write your own method for it:

```{.scala .scrollx}
def getType[T: TypeTag](obj: T) = typeOf[T]
getType: [T](obj: T)(implicit evidence$1: reflect.runtime.universe.TypeTag[T])reflect.runtime.universe.Type
```

The scala compiler will supply our __getType__ method with an implicit for __TypeTag[T]__.

So What is a [TypeTag](http://www.scala-lang.org/api/2.11.1/scala-reflect/index.html#scala.reflect.api.TypeTags)?

> A TypeTag[T] encapsulates the runtime type representation of some type T. Like scala.reflect.Manifest, the prime use case of TypeTags is to give access to erased types.

As with Java, Scala generic types which are present at compile time are erased at runtime (erasure). TypeTags are a way of having access to that lost compile time information at runtime.

With __getType__ we can now extract the universe.Type of an instance:

```{.scala .scrollx}
getType(List(1,2,3))
res4: reflect.runtime.universe.Type = List[Int]
```

Hopefully this has given you a taste for some of the information provided by the scala reflection api and a starting point to explore it further.