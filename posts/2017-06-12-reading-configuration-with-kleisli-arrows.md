---
title: Reading Configuration with Kleisli Arrows
author: sanjiv sahayam
description: How to use Kleisli Arrows supply an environment to a Monadic context similar to the Reader and ReaderT Monads.
tags: arrow, fp, kleisli, scala
comments: true
---

In a [previous article](http://sanj.ink/posts/2017-06-07-composing-monadic-functions-with-kleisli-arrows.html) we looked at how Kleisli Arrows compose functions in a Monadic context.

A Kleisli Arrow is defined as follows:

```{.scala .scrollx}
Kleisli[F[_], A, B](run: A => F[B])
```

![Kleisli Arrow ](/images/kleisli-composition/kleisli-type.jpg)

In essence it wraps a function:

```{.scala .scrollx}
A => F[B]
```

Given some _A_, it returns a result _B_ in a context F.

## Reader and ReaderT

If we look at the signature of the Reader Monad, we see it wraps a somewhat similar function:

```{.scala .scrollx}
A => B
```

![Reader](/images/kleisli-config/reader.jpg)

Given some _A_, it returns a _B_ without any context.

A [ReaderT Monad Transformer](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#v:ReaderT) wraps the same function as that of a Kleisli Arrow:

```{.scala .scrollx}
ReaderT[F[_], A, B](A => F[B])
```

![ReaderT](/images/kleisli-config/readert-type.jpg)

Given some A, it returns a B, in a context F.

A Kleisli Arrow has the same shape as (is isomorphic to) the ReaderT Monad. But what of its relationship to the Reader Monad?

An __Id__ type constructor is a similar construct to the __identity__ function, except that it returns the type supplied to it as opposed to the value:

```{.scala .scrollx}
type Id[A] = A // returns the type supplied
def identity[A](value: A): A = value //returns the value supplied
```

![Id](/images/kleisli-config/id-type.jpg)

Armed with the __Id__ type constructor we can define the Reader Monad in terms of the ReaderT Monad (and the Kleisli Arrow):

```{.scala .scrollx}
ReaderT[F[_], A, B]  ==
ReaderT[Id, A, B]    == //specialising for Id
ReaderT(A => Id[B])  ==
Reader(A => B)       == //since Id[B] == B
Kleisli(A => B)         //since ReaderT == Kleisli
```

In [Cats](http://typelevel.org/cats) both the Reader and ReaderT Monads are defined in terms of a Kliesli Arrow:

```{.scala .scrollx}
type Reader[A, B]        = Kleisli[Id, A, B]
type ReaderT[F[_], A, B] = Kleisli[F, A, B]
```

## Using Kleisli Arrows as a ReaderT

Lets try and use a Kleisli Arrow to read some configuration from the environment to yield something useful. In the following example we want to create a __Person__ object from a __Name__ and __Age__ obtained from a __Config__ object that holds both values:

```{.scala .scrollx}
final case class Name(first: String, last: String)

final case class Age(age: Int)

final case class Person(name: Name, age: Age)

final case class Config(name: String, age: Int)
```

The creation of __Name__ and __Age__ have rules surrounding them and can fail if the rules are not met:

```{.scala .scrollx}
  def readName: Config => Option[Name] = c => {
    val parts = c.name.split(" ")
    if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
  }

  def readNameK = Kleisli(readName)

  def readAge: Config => Option[Age] = c => {
    val age = c.age
    if (age >= 1 && age <= 150) Option(Age(age)) else None
  }

  def readAgeK = Kleisli(readAge)
```

__readNameK__ and __readAgeK__ require a __Config__ object to retrieve their values and are wrapped in a Kleisli Arrow. The Kleisli Arrow has to supply the same __Config__ object to both functions. This is distinctly different to Kleisli composition where the output from one function was fed into the next. In this instance there is no composition between the two functions.

How would we go about combining these functions?

Since Kleisli Arrows map over functions in a Monadic context:

```{.scala .scrollx}
Kleisli[F[_], A, B] //F has a Monad instance
```

we can use a for-comprehension to solve our problem:

```{.scala .scrollx}
import cats.implicits._

val personK: Kleisli[Option, Config, Person] =
  for {
    name <- readNameK
    age  <- readAgeK
  } yield Person(name, age)

//Some(Person(Name(Bojack,Horseman),Age(42)))
val result1 = personK(Config("Bojack Horseman", 42))

//None - Name is not space-separated
val result2 = personK(Config("Jake", 20))

//None - age is not between 1 and 150
val result3 = personK(Config("Fred Flintstone", 50000))
```

## Using Applicatives to Read Configuration

You might have noticed that the __readAgeK__ function does not directly depend on the output of __readNameK__. This implies that we don't have to use a Monad here (for-comprehesion) and can use something a little less powerful like [Apply](https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/Apply.scala#L11). _The Apply typeclass is an Applicative without the __pure__ function_.  The Kleisli data type has an [instance for Apply](https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/data/Kleisli.scala#L170) with the following signature:

```{.scala .scrollx}
Apply[Kleisli[F, A, ?]]
```

Let's have a go at rewriting the Monadic code snippet with an Apply instead. We can use the [__ap2__](https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/Apply.scala#L25) function which has the following definition:

```{.scala .scrollx}
def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z]
```

Using __ap2__ we can create a __Person__ instance as follows:

```{.scala .scrollx}
import cats.Apply
import cats.implicits._

type KOptionConfig[A] = Kleisli[Option, Config, A]
type PersonFunc = (Name, Age) => Person

val config = Config("mr peanutbutter", 30)
val readNameKOC: KOptionConfig[Name] = readNameK
val readAgeKOC: KOptionConfig[Age] = readAgeK
val personKOC: KOptionConfig[PersonFunc] = Kleisli( (_: Config) => Option(Person(_, _)))

//Kleisli[Option, Config, Person]
val personK = Apply[KOptionConfig].ap2(personKOC)(readNameKOC, readAgeKOC)

//Some(Person(Name(mr,peanutbutter),Age(30)))
personK(config)
```

This solution while "less powerful" than the Monadic version, is somewhat uglier in Scala due to the type ascriptions on the individual functions.

We can also do something very similar using the [__map2__](https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/Apply.scala#L33) method:

```{.scala .scrollx}
def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
```

which might be easier to reason about than __ap2__, but essentially they achieve the same result:

```{.scala .scrollx}
import cats.Apply
import cats.implicits._

type KOptionConfig[A] = Kleisli[Option, Config, A]

val config = Config("Diane Nguyen", 27)
val readNameKOC: KOptionConfig[Name] = readNameK
val readAgeKOC: KOptionConfig[Age] = readAgeK

//Kleisli[Option, Config, Person]
val personK = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person(_, _) }

//Some(Person(Name(Diane,Nguyen),Age(27)))
personK(config)
```

## Kleisli Arrows with Different Inputs

One thing to note is that we keep aligning the input type, __Config__ in this case, through all Kleisli Arrows.

How would we go about combining Kleisli Arrows with different input types?

This is where the [__local__](https://github.com/typelevel/cats/blob/155f7f534993c30d6e757de990330ac796dad5da/core/src/main/scala/cats/data/Kleisli.scala#L48) function comes into play. It is defined as:

```{.scala .scrollx}
def local[AA](f: AA => A): Kleisli[F, AA, B]
```

![Local](/images/kleisli-config/kleisli-local.jpg)

It basically converts a ```Kleisli[F, A, B]``` to a ```Kleisli[F, AA, B]``` as long as we supply it a function to convert an _AA_ to _A_. The function __f__ here converts some other input type _AA_ into our required input type of _A_. This allows us to combine Kleisli Arrows with different input types as the __local__ function, widens the input type to each Kleisli Arrow as _AA_.

Lets rewrite our previous example with Kleisli Arrows that take a __String__ as input to __readName__ and an __Int__ as an input to __readAge__ functions:

```{.scala .scrollx}
  def readName: String => Option[Name] = name => {
    val parts = name.split(" ")
    if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
  }

  def readAge: Int => Option[Age] = age => {
    if (age >= 1 && age <= 150) Option(Age(age)) else None
  }

  def readNameK: Kleisli[Option, String, Name] = Kleisli(readName)

  def readAgeK: Kleisli[Option, Int, Age] = Kleisli(readAge)
```

We then widen the input types with the __local__ function that takes a __Config__ object:

```{.scala .scrollx}
import cats.implicits._

val personK: Kleisli[Option, Config, Person] =
  for {
    name <- readNameK.local[Config](_.name)
    age  <- readAgeK.local[Config](_.age)
  } yield Person(name, age)

//Some(Person(Name(Bojack,Horseman),Age(42)))
personK(Config("Bojack Horseman", 42))

//None
personK(Config("Jake", 20))

//None
personK(Config("Fred Flintstone", 50000))
```

And using __map2__ we get the same results:

```{.scala .scrollx}
import cats.Apply
import cats.implicits._

type KOptionConfig[A] = Kleisli[Option, Config, A]

val config = Config("Diane Nguyen", 27)
val readNameKOC: KOptionConfig[Name] = readNameK.local[Config](_.name)
val readAgeKOC: KOptionConfig[Age] = readAgeK.local[Config](_.age)

val personK = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person(_, _) }

//Some(Person(Name(Diane,Nguyen),Age(27)))
personK(config)
```