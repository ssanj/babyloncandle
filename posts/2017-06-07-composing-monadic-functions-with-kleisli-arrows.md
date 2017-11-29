---
title: Composing Monadic Functions with Kleisli Arrows
author: sanjiv sahayam
description: How to use Kleisli Arrows to compose functions in a Monadic context.
tags: arrow, fp, kleisli, scala
comments: true
---

Function composition is great isn't it? It's one of the corner-stones of Functional Programming. Given a function ```g: A => B``` and a function  ```f: B => C``` we can compose them (join them together) as ```f compose g``` to return a function ```A => C```. The composition hides the intermediate steps of ```A => B``` and ```B => C```, instead letting us focus on the initial input (_A_) and final output (_C_). This is the glue that lets us write many small functions and combine them into larger, more useful functions.

![Function Composition](/images/kleisli-composition/function-composition.jpg)

Function composition works from right to left, where the first function called is the one on the right. This can be confusing when learning about composition, as we are used reading from left to right. If you find this confusing you can use the __andThen__ function instead which orders the functions from left to right: ```g andThen f```as opposed to ```f compose g```.

_In this article we use the [Scala](http://www.scala-lang.org/) language and the [Cats](http://typelevel.org/cats/) functional programming library to illustrate the main concepts. The source code for this article is available on [Github](https://github.com/ssanj/kleisli)_.

To make this more concrete with a simple example, lets start with the following functions:

```{.scala .scrollx}
  def mul2: Int => Int = _ * 2

  def power2: Int => Double = Math.pow(_, 2)

  def doubleToInt: Double => Int = _.toInt

  def intToString: Int => String = _.toString
```

While these simple functions work in isolation, we can also combine them (compose them) together to create a more powerful function that does what all of the functions do:

```{.scala .scrollx}
val pipeline: Int => String = intToString compose mul2 compose doubleToInt compose power2

pipeline(3)//returns "18"
```

The __pipeline__ function, combines all the functions together to create a new function that:

1. Raises a supplied number to the power of 2
1. Converts the result to an Int value
1. Multiplies the result value by 2
1. Converts the result to a String

We can do this because the types align all the way down:

```{.scala .scrollx}
Int => Double //power2
       Double => Int //doubleToInt
                 Int => Int //mul2
                        Int => String //intToString

Int => String //pipeline
```

Now we can use and pass around the __pipeline__ function without thinking about all the small functions comprise it.

## Monadic Functions

Things get a little more interesting when we have functions that return values in a context:

```{.scala .scrollx}
  def stringToNonEmptyString: String => Option[String] = value =>
    if (value.nonEmpty) Option(value) else None

  def stringToNumber: String => Option[Int] = value =>
    if (value.matches("-?[0-9]+")) Option(value.toInt) else None
```

If we try to compose __stringToNonEmptyString__ and __stringToNumber__:

```{.scala .scrollx}
val pipeline: String => Option[Int] = stringToNumber compose stringToNonEmptyString
```

we get the following compilation error:

```{.scala .scrollx}
[error]  found   : String => Option[String]
[error]  required: String => String
[error]     val pipeline: String => Option[Int] = stringToNumber compose stringToNonEmptyString
```

Oh dear! When we compose __stringToNonEmptyString__ with __stringToNumber__, the __stringToNumber__ function expects a __String__ but instead __stringToNonEmptyString__ is supplying it an Option[String]. The types don't align any more and we can't compose:

```{.scala .scrollx}
//the types don't align
String => Option[String] //stringToNonEmptyString
          String => Option[Int] //stringToNumber
```

It would be nice if we didn't have to think about the context of the result type (Option[String] in this instance) and just continue to compose on the plain type (String in this instance).

## Kleisli Composition

Kleisli is a type of [Arrow](https://wiki.haskell.org/Arrow_tutorial#Kleisli_Arrows) for a [Monad](https://wiki.haskell.org/Monad)ic context. It is defined as:

```{.scala .scrollx}
final case class Kleisli[F[_], A, B](run: A => F[B])
```

![Kleisli Type Signature](/images/kleisli-composition/kleisli-type.jpg)

The Kleisli type is a wrapper around ```A => F[B]```, where F is some context that is a Monad. What helps us with our composition of contextual results, is that Kleisli has a compose function with the following signature (simplified for clarity):

```{.scala .scrollx}
def compose(g: A => F[B], f: B => F[C])(implicit M: Monad[F]): A => F[C]
```

What the above signature tells us is that we can join together functions that return results in a context F (for which we have a Monad instance) with functions that work on the simple uncontextualised value:

```{.scala .scrollx}
A => F[B] //g
       B => F[C] //f

A => F[C] //f compose g
```

![Kleisli Composition](/images/kleisli-composition/kleisli-composition.jpg)

For the __stringToNonEmptyString__ and __stringToNumber__ functions, the Monadic context used is [Option](http://www.scala-lang.org/api/current/scala/Option.html) (both functions return an optional value).

So why does the Kleisli __compose__ method need a Monadic instance for F? Under the covers Kleisli composition uses Monadic bind ([>>=](https://github.com/non/cats/blob/master/core/src/main/scala/cats/FlatMap.scala#L26)) to join together the Monadic values. Bind is defined as:

```{.scala .scrollx}
def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
```

### Using Kleisli Composition

Let's try and compose the __stringToNonEmptyString__ and __stringToNumber__ functions again but this time using Kleisli composition:

```{.scala .scrollx}
import cats.data.Kleisli
import cats.implicits._ //Brings in a Monadic instance for Option

val stringToNonEmptyStringK = Kleisli(stringToNonEmptyString) //Kleisli[Option, String, String]
val stringToNumberK = Kleisli(stringToNumber) //Kleisli[Option, String, Int]

val pipeline = stringToNumberK compose stringToNonEmptyStringK //Kleisli[Option, String, Int]

pipeline("1000") //Some(1000)
pipeline("") //None
pipeline("A12B") //None
```

And now we can successfully compose the two functions! In addition, notice how when we use different inputs, the Monadic result changes; The same rules apply for composing these Monadic values through Kleisli composition as for Monadic bind. If a value of None is returned from one of the intermediate functions, the the pipeline returns a None. If all the functions succeed with __Some__ values, then the pipeline returns a __Some__ as well.

### Using Plain Monads

Given that Kleisli composition, needs a Monadic instance to do its magic, could we simply replace Kleisli composition with straight Monads?
Let's give it a shot:


```{.scala .scrollx}
import KleisliComposition._
import cats.implicits._

val pipeline: String => Option[Int] = Option(_) >>= stringToNonEmptyString >>= stringToNumber
pipeline("1000") //Some(1000)
pipeline("")// None
pipeline("A12B")// None
```

Or if we have the input up front:

```{.scala .scrollx}
import cats.implicits._

Option("1000") >>= stringToNonEmptyString >>= stringToNumber //Some(1000)
Option("") >>= stringToNonEmptyString >>= stringToNumber //None
Option("A12B") >>= stringToNonEmptyString >>= stringToNumber //None
```

And it looks like we can.

### Benefits of Kleisli Composition

So what does Kleisli Composition really give us over using plain old Monads?

1. Allows programming in a more composition like style.
1. Abstracts away the lifting of values into a Monad.

And if we squint, ```A => F[B]``` looks a lot like the [Reader Monad](http://adit.io/posts/2013-06-10-three-useful-monads.html#the-reader-monad). More on that later.