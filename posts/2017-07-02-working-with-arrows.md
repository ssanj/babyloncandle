---
title: Working with Arrows
author: sanjiv sahayam
description: ???
tags: arrows, scala,
comments: true
---

In the [last article](http://sanj.ink/posts/2017-06-12-reading-configuration-with-kleisli-arrows.html) we looked at how we could read configuration with a Kleisli Arrow similar to a Reader Monad.

We've been using Arrows for the last couple of articles but haven't defined what an Arrow is exactly.

An Arrow is a computation that runs within a context which takes in an input and returns an output.

In [Cats](http://typelevel.org/cats/) the [Arrow typeclass](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/arrow/Arrow.scala#L8) is defined with the type constructor F which has two type holes:

```{.scala .scrollx}
Arrow[F[_, _]] //simplified
```

These two type holes correspond to the input and output types of the Arrow. F can be any type constructor that takes two types and performs a mapping between them. A __scala.Function1__ is an example of F, as is the __Kleisli Arrow__ we saw in previous articles. It might be helpful to think of Arrows as simple functions from one type to another for the moment.

Lets now go through some of the functions defined on Arrow and how they are used. For the remainder of the article lets assume that the type constructor supplied to Arrow is a [__scala.Function1__](http://www.scala-lang.org/api/current/scala/Function1.html):

```{.scala .scrollx}
trait Function1[-T1, +R] extends AnyRef
```

and the resulting Arrow is:

```{.scala .scrollx}
val fa = Arrow[Function1]
```

## [lift](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/arrow/Arrow.scala#L13)

This is a simple function to construct an Arrow given its input and output types. This is defined in Cats as:

```{.scala .scrollx}
def lift[A, B](f: A => B): F[A, B]
```

For example to lift a function that goes from a __String__ to an  __Int__ into __F__ we'd do:

```{.scala .scrollx}
val findLength: String => Int = _.length
fa.lift(f) //Function1[String, Int]
```

Since __findLength__ is already a __scala.Function1__ it is a little pointless to lift it into a __scala.Function1__ but hopefully its usage is clear.

In [Scalaz](https://github.com/scalaz) this function is defined as [arr](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L16) but essentially lifts a function into an Arrow:

```{.scala .scrollx}
def arr[A, B](f: A => B): A =>: B
```

## [id](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/arrow/Category.scala#L11)

The __id__ function is defined as:

```{.scala .scrollx}
def id[A]: F[A, A]
```

The type signature of the above tells us that F returns the input type A as its output, essentially giving us the [identity](http://www.scala-lang.org/api/2.11.11/index.html#scala.Predef$@identity[A](x:A):A) function.

```{.scala .scrollx}
val intF1 = fa.id[Int] //Function1[Int, Int]
intF1(10) //returns 10
```

## [first](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/functor/Strong.scala#L24)

The __first__ function is defined as:

```{.scala .scrollx}
def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
```

The __first__ function takes Arrow __fa__ from __A__ => __B__ and returns another Arrow (__A__, __C__) => (__B__, __C__). It applies the function in __fa__ to the first parameter of the tuple, which is an __A__ and converts it to a __B__. The second parameter of the tuple it leaves untouched and returns a (__B__, _C_).

![First](/images/arrow-functions/arrow-first3.jpg)

For the remaining examples we have the following definitions at our disposal:

```{.scala .scrollx}
final case class Name(first: String, last: String)
final case class Age(age: Int)
final case class Person(name: Name, age: Age)

val name = Name("Nagate", "Tanikaze")
val age = Age(22)

def upperFirstName: String => String = _.toUpperCase
def doubleNumber: Int => Int = _ * 2

def upperName: Name => Name = n => Name(upperFirstName(n.first), n.last)
def doubleAge: Age => Age = a => Age(doubleNumber(a.age))
```

For example if we wanted to apply a function to the __Name__ element of a __Name__ and __Age__ pair and but wanted to leave the __Age__ element untouched we could do:

```{.scala .scrollx}
val onlyNameF: ((Name, Age)) => (Name, Age) = fa.first[Name, Name, Age](upperName)
val toPersonF: ((Name, Age)) => Person = onlyNameF andThen (Person.apply _).tupled
toPersonF(name, age) //returns Person(Name(NAGATE,Tanikaze),Age(22))
```

Notice how the __Age__ value of the input is unchanged.

## [second](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/functor/Strong.scala#L39)

The __second__ function is very similar to __first__ only with its parameters switched. It is defined as:

```{.scala .scrollx}
def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
```

The __second__ function takes Arrow __fa__ from __A__ => __B__ and returns another Arrow with takes in a tuple of (__C__, __A__) => (__C__, __B__). It applies the function in __fa__ to the second parameter of the tuple __A__ and converts it to a __B__. The first parameter of the tuple it leaves untouched and returns a (_C_, __B__).

![Second](/images/arrow-functions/arrow-second2.jpg)

For example if we wanted to apply function to the __Age__ element of a __Name__ and __Age__ pair and but wanted to leave the __Name__ element untouched we could do:

```{.scala .scrollx}
val onlyAgeF: ((Name, Age)) => (Name, Age) = fa.second[Age, Age, Name](doubleAge)
val toPersonF: ((Name, Age)) => Person = onlyAgeF andThen (Person.apply _).tupled
toPersonF(name, age) //returns Person(Name(Nagate,Tanikaze),Age(44))
```

Notice how the __Name__ value of the input is unchanged.

## [split/spread/\*\*\*](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/arrow/Split.scala)

The __split__ function is an application of __first__ and __second__. It is defined as:

```{.scala .scrollx}
def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)]
```

The __split__ function takes Arrow __f__ from __A__ => __B__ and an Arrow __g__ from __C__ => __D__ and returns another Arrow with takes in a tuple of (__A__, __C__) => (__B__, __D__). It applies the function in __f__ to the first parameter of the tuple __A__ and converts it to a __B__. It also applies the function in __g__ to the second parameter of the tuple __C__ and converts it to a __D__ returning a final result of (__B__, __D__). Split has the symbolic representation of __\*\*\*__ and is sometimes referred to as the __spread__ function because it applies multiple functions to multiple inputs.

![Split](/images/arrow-functions/arrow-split3.jpg)

For example if we wanted to apply function to the __Name__ and __Age__ element of a __Name__ and __Age__ pair at once we could do:

```{.scala .scrollx}
val bothNameAndAgeF: ((Name, Age)) => (Name, Age) = fa.split[Name, Name, Age, Age](upperName, doubleAge)
val toPersonF: ((Name, Age)) => Person = bothNameAndAgeF andThen (Person.apply _).tupled
toPersonF(name, age)//Person(Name(NAGATE,Tanikaze),Age(44))
```

## [combine/merge/&&&](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L55)

__combine__ is defined as:

```{.scala .scrollx}
def combine[A, B, C](fab: F[A, B], fac: => F[A, C]): F[A, (B, C)]
```

Although Cats does not define combine, [scalaz does](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L55). For the purpose of this post I've created an implementation of combine in the example source.

The __combine__ function takes Arrow __fab__ from __A__ => __B__ and an Arrow __fac__ from __A__ => __C__ and returns another Arrow with takes in an input of __A__, and returns a tuple of (__B__, __C__). It's important to note that the same input __A__ is supplied to both arrows __fab__ and __fac__.

![Combine](/images/arrow-functions/arrow-combine.jpg)

For example given a __Person__ if we want to break it into primitive representations of its __Name__ and __Age__ fields we could do:

```{.scala .scrollx}
val person = Person(name, age)
val combineName: Person => String = {
  case Person(Name(first, last), _) => s"$first $last"
}
val combineAge: Person => Int = _.age.age
val combineF: Person => (String, Int) = ArrowFuncs.combine(combineName, combineAge)
combineF(person) // ("Nagate Tanikaze",22): (String, Int)
```

__combine__ has a symbolic representation of __&&&__ and is sometimes referred to as the __merge__ function.


## liftA2

## compose/andThen



//Why do Arrows return a pair on first and second?

