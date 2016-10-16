---
title: Output in Scala REPL is Truncated
author: sanjiv sahayam
description: How to stop the Scala REPL truncating your output.
tags: repl, scala
comments: true
---

The scala REPL truncates long output that exceeds a certain set limit. Take the example below of displaying the members of a Traversable:

```{.scala .scrollx}
ru.typeOf[Traversable[_]].members.mkString("\n")
res46: String =
override def seq: Traversable[A]
override def companion: scala.collection.generic.GenericCompanion[Traversable]
def $init$(): Unit
def transpose[B](implicit asTraversable: A => scala.collection.GenTraversableOnce[B]): CC[CC[B] @scala.annotation.unchecked.uncheckedVariance]
def flatten[B <: <?>](implicit asTraversable: <?>): CC[B]
def unzip3[A1 <: <?>, A2 <: <?>, A3 <: <?>](implicit asTriple: <?>): (CC[A1], CC[A2], CC[A3])
def unzip[A1 <: <?>, A2 <: <?>](implicit asPair: <?>): (CC[A1], CC[A2])
def genericBuilder[B <: <?>]: scala.collection.mutable.Builder[B,CC[B]]
protected[this] def newBuilder: scala.collection.mutable.Builder[A,CC[A]]
class WithFilter extends FilterMonadic[A,Repr]
def withFilter(p: A => Boolean): scala.collection.generic.FilterMonadic[A,Repr]
def view(f...
```

The method list of Traversable has been truncated. How do we go about increasing this limit?

We can find out the [current maximum printable String length](http://stackoverflow.com/questions/9516567/settings-maxprintstring-for-scala-2-9-repl) by going into __:power__ mode and then accessing the __vals.isettings__ variable:

```{.scala .scrollx}
:power
vals.isettings

res50: scala.tools.nsc.interpreter.ISettings =

 ISettings {
   deprecation = false
  maxAutoprintCompletion = 250
  maxPrintString = 800
  unwrapStrings = true

 }
```

The __maxPrintString__ setting is at 800 characters. This means that any output over 800 characters is truncated within the REPL. To increase this limit, simply set a new __maxPrintString__ value:

```{.scala .scrollx}
vals.isettings.maxPrintString = Int.MaxValue
vals.isettings.maxPrintString: Int = 2147483647

vals.isettings
res51: scala.tools.nsc.interpreter.ISettings =

 ISettings {
   deprecation = false
  maxAutoprintCompletion = 250
  maxPrintString = 2147483647
  unwrapStrings = true

 }
```

Now if we interrogate the members of Traversable again, we get the full list:

```{.scala .scrollx}
scala> ru.typeOf[Traversable[_]].members.mkString("\n")
res53: String =
override def seq: Traversable[A]
override def companion: scala.collection.generic.GenericCompanion[Traversable]
def $init$(): Unit
def transpose[B](implicit asTraversable: A => scala.collection.GenTraversableOnce[B]): CC[CC[B] @scala.annotation.unchecked.uncheckedVariance]
def flatten[B](implicit asTraversable: A => scala.collection.GenTraversableOnce[B]): CC[B]
def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3])
def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2])
def genericBuilder[B]: scala.collection.mutable.Builder[B,CC[B]]
protected[this] def newBuilder: scala.collection.mutable.Builder[A,CC[A]]
class WithFilter extends FilterMonadic[A,Repr]
def withFilter(p: A => Boolean): scala.collection.generic.FilterMonadic[A,Repr]
def view(from: Int,until: Int): scala.collection.TraversableView[A,Repr]
def view: scala.collection.TraversableView[A,Repr]
def stringPrefix: String
override def toString(): String
override def to[Col[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing,A,Col[A @scala.annotation.unchecked.uncheckedVariance]]): Col[A @scala.annotation.unchecked.uncheckedVariance]
def toStream: Stream[A]
def toIterator: Iterator[A]
def toTraversable: Traversable[A]
def copyToArray[B >: A](xs: Array[B],start: Int,len: Int): Unit
def inits: Iterator[Repr]
def tails: Iterator[Repr]
def splitAt(n: Int): (Repr, Repr)
def span(p: A => Boolean): (Repr, Repr)
def dropWhile(p: A => Boolean): Repr
def takeWhile(p: A => Boolean): Repr
private[package scala] def sliceWithKnownBound(from: Int,until: Int): Repr
private[package scala] def sliceWithKnownDelta(from: Int,until: Int,delta: Int): Repr
def slice(from: Int,until: Int): Repr
def drop(n: Int): Repr
def take(n: Int): Repr
def init: Repr
def lastOption: Option[A]
def last: A
override def tail: Repr
def headOption: Option[A]
def head: A
def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def find(p: A => Boolean): Option[A]
def exists(p: A => Boolean): Boolean
def forall(p: A => Boolean): Boolean
def groupBy[K](f: A => K): scala.collection.immutable.Map[K,Repr]
def partition(p: A => Boolean): (Repr, Repr)
def collect[B, That](pf: PartialFunction[A,B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def filterNot(p: A => Boolean): Repr
def filter(p: A => Boolean): Repr
def flatMap[B, That](f: A => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def map[B, That](f: A => B)(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def ++:[B >: A, That](that: Traversable[B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def ++:[B >: A, That](that: scala.collection.TraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def ++[B >: A, That](that: scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[Repr,B,That]): That
def hasDefiniteSize: Boolean
def isEmpty: Boolean
protected[this] def parCombiner: scala.collection.parallel.Combiner[A,scala.collection.parallel.ParIterable[A]]
protected[this] def toCollection(repr: Repr): Traversable[A]
protected[this] def thisCollection: Traversable[A]
final def isTraversableAgain: Boolean
def repr: Repr
protected[this] type Self = Repr
def par: ParRepr
def addString(b: StringBuilder): StringBuilder
def addString(b: StringBuilder,sep: String): StringBuilder
def addString(b: StringBuilder,start: String,sep: String,end: String): StringBuilder
def mkString: String
def mkString(sep: String): String
def mkString(start: String,sep: String,end: String): String
def toMap[T, U](implicit ev: <:<[A,(T, U)]): scala.collection.immutable.Map[T,U]
def toVector: Vector[A]
def toSet[B >: A]: scala.collection.immutable.Set[B]
def toBuffer[B >: A]: scala.collection.mutable.Buffer[B]
def toIndexedSeq: scala.collection.immutable.IndexedSeq[A]
def toSeq: Seq[A]
def toIterable: Iterable[A]
def toList: List[A]
def toArray[B >: A](implicit evidence$1: scala.reflect.ClassTag[B]): Array[B]
def copyToArray[B >: A](xs: Array[B]): Unit
def copyToArray[B >: A](xs: Array[B],start: Int): Unit
def copyToBuffer[B >: A](dest: scala.collection.mutable.Buffer[B]): Unit
def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A
def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A
def max[B >: A](implicit cmp: Ordering[B]): A
def min[B >: A](implicit cmp: Ordering[B]): A
def product[B >: A](implicit num: Numeric[B]): B
def sum[B >: A](implicit num: Numeric[B]): B
def aggregate[B](z: => B)(seqop: (B, A) => B,combop: (B, B) => B): B
def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1
def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]
def reduce[A1 >: A](op: (A1, A1) => A1): A1
def reduceRightOption[B >: A](op: (A, B) => B): Option[B]
def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
def reduceRight[B >: A](op: (A, B) => B): B
def reduceLeft[B >: A](op: (B, A) => B): B
def foldRight[B](z: B)(op: (A, B) => B): B
def foldLeft[B](z: B)(op: (B, A) => B): B
def :\[B](z: B)(op: (A, B) => B): B
def /:[B](z: B)(op: (B, A) => B): B
def collectFirst[B](pf: PartialFunction[A,B]): Option[B]
def count(p: A => Boolean): Int
def nonEmpty: Boolean
def size: Int
protected[this] def reversed: List[A]
final def $asInstanceOf[T0](): T0
final def $isInstanceOf[T0](): Boolean
final def synchronized[T0](x$1: T0): T0
final def ##(): Int
final def !=(x$1: Any): Boolean
final def ==(x$1: Any): Boolean
final def ne(x$1: AnyRef): Boolean
final def eq(x$1: AnyRef): Boolean
final def notifyAll(): Unit
final def notify(): Unit
protected[package lang] def clone(): Object
final def getClass(): Class[_]
def hashCode(): Int
def equals(x$1: Any): Boolean
final def wait(): Unit
final def wait(x$1: Long): Unit
final def wait(x$1: Long,x$2: Int): Unit
protected[package lang] def finalize(): Unit
final def asInstanceOf[T0]: T0
final def isInstanceOf[T0]: Boolean
def foreach[U](f: A => U): Unit
```
An alternative is to set the __scala.repl.maxprintstring__ JVM parameter externally when invoking the scala REPL:

```{.command .scrollx}
scala -Dscala.repl.maxprintstring=64000
```
