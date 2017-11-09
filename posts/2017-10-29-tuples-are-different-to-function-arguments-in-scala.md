---
title: Tuples are Different to Function Arguments in Scala
author: sanjiv sahayam
description: Although Scala function arguments look like tuples, they are not. In this post I investigate how convert to and from tuples and function arguments.
tags: scala
comments: true
---

In Scala Tuples and function arguments look similar but they can't be used interchangeably.

A Tuple of two Ints can be defined as:

```{.scala .scrollx}
scala> val t2 = (2, 4)
t2: (Int, Int) = (2,4)
```

Given a simple __sum__ method that takes in two Ints:

```{.scala .scrollx}
def sum(n1: Int, n2: Int) = n1 + n2
```

you might think that you could simply pass in your tuple __t2__ to invoke the __sum__ method:

```{.scala .scrollx}
scala> sum(t2)
<console>:15: error: not enough arguments for method sum: (n1: Int, n2: Int)Int.
Unspecified value parameter n2.
       sum(t2)
```

Unfortunately this does not work as you can't simply replace an argument list with a similarly-sized tuple. __t2__ is taken as the first argument __n1__, hence the error indicating that __n2__ has not been supplied.

One way to get this to work is to do the following:

```{.scala .scrollx}
scala> sum _ tupled t2
res0: Int = 6
```

Let's break that incantation down step-by-step to make it more digestible.

1. Convert the __sum__ method into a [Function](https://github.com/scala/scala/blob/v2.12.4/src/library/scala/Function.scala#L1):

```{.scala .scrollx}
scala> val f1 = sum _
f1: (Int, Int) => Int = $$Lambda$1447/998900406@31452c9
```

2. Convert the function into its tupled variant:

```{.scala .scrollx}
scala> val f2 = f1.tupled
f2: ((Int, Int)) => Int = scala.Function2$$Lambda$227/234698513@3f891cfe
```

Tupling the __sum__ function is merely going from `(Int, Int) => Int` to `((Int, Int)) => Int`. Notice the extra parenthesis around the arguments.

3. Apply the tupled function to the tupled input __t2__:

```{.scala .scrollx}
scala> f2(t2)
res21: Int = 6
```

Looking back that does look very difficult but it's not very intuitive.

## Using Underscores with Currying

I had a similar problem recently where I had a __contains__ method defined as:

```{.scala .scrollx}
def contains[A](values: List[A], value: A, pred: A => A  => Boolean): Boolean = {
    values.exists(pred(value))
}
```

And a List __l1__ defined as:

```{.scala .scrollx}
val l1 = List(1, 2, 3)
```

I tried to call the __contains__ method using underscores for the values of the __pred__ parameter and got the following error:

```{.scala .scrollx}
scala> contains[Int](l1, 3, _ == _)
<console>:17: error: missing parameter type for expanded function ((x$1: <error>, x$2) => x$1.$eq$eq(x$2))
       contains[Int](l1, 3, _ == _)
                            ^
<console>:17: error: missing parameter type for expanded function ((x$1: <error>, x$2: <error>) => x$1.$eq$eq(x$2))
       contains[Int](l1, 3, _ == _)

```


You can use underscores to represent positional arguments in an argument list where you don't need to bind it to a name. So why did this fail?

I can get the __contains__ method to work with:

```{.scala .scrollx}
scala> contains[Int](l1, 3, x => y => x == y)
res24: Boolean = true
```

Conversely, why did this work?

Another interesting variant is if I change the definition of __contains__ to __contains2__ that takes in an uncurried __pred__ function:

```{.scala .scrollx}
def contains2[A](values: List[A], value: A, pred: (A, A)  => Boolean): Boolean = {
    values.exists(pred(value, _))
}
```

I can invoke it with the underscore syntax:

```{.scala .scrollx}
scala> contains2[Int](l1, 3,  _ == _)
res59: Boolean = true
```

One of the main reasons for using a curried version of __pred__ was that I could partially apply it with the [exists](https://github.com/scala/scala/blob/c2a5883891a68180b143eb462c8b0cebc8d3b021/src/library/scala/collection/GenTraversableOnce.scala#L459) method on List without having to use underscores to convert the method to a function. I can still achieve the same result by currying __pred__ where it is applied:

```{.scala .scrollx}
def contains3[A](values: List[A], value: A, pred: (A, A)  => Boolean): Boolean = {
    values.exists(pred.curried(value))
}
```

The reason I couldn't use underscores to represent the parameters of the __contains__ method is that the curried function __pred__, represents two argument lists; One that takes an `A` and returns another function that takes another `A` and returns a Boolean:

```{.scala .scrollx}
(A) => (A => Boolean)
```

Underscores can only used to represent positional arguments of a single argument list, since we have two in the curried variation of __pred__ in __contains__ we can't use it.

## Changing the shape of the Input Function

If I define a uncurried function __isEqual__ as:

```{.scala .scrollx}
def isEqual[A](a1: A, a2: A): Boolean  = a1 == a2
```

I can call __contains2__ as:

```{.scala .scrollx}
scala> contains2[Int](l1, 3, isEqual)
res32: Boolean = true
```

If I define an __isEqual2__ as:

```{.scala .scrollx}
def isEqual2[A]: A => A => Boolean = a1 => a2 => a1 == a2
```

I can call __contains__ as:

```{.scala .scrollx}
scala> contains[Int](l1, 3, isEqual2)
res33: Boolean = true
```

But if I try to call __contains2__ with __isEqual2__ we get:

```{.scala .scrollx}
scala> contains2[Int](l1, 3, isEqual2[Int])
<console>:18: error: type mismatch;
 found   : Int => (Int => Boolean)
 required: (Int, Int) => Boolean
       contains2[Int](l1, 3, isEqual2[Int])
```

And we can fix that by uncurrying __isEqual2__:

```{.scala .scrollx}
scala> contains2(l1, 3, Function.uncurried(isEqual2))
res65: Boolean = true
```

If we define __isEqual3__ with a [Tuple2](http://www.scala-lang.org/api/2.12.4/scala/Tuple2.html) as:

```{.scala .scrollx}
def isEqual3[A]: Tuple2[A, A] => Boolean = t => t._1 == t._2
```

And we try to invoke __contains2__ with __isEqual3__ we get:

```{.scala .scrollx}
scala> contains2(l1, 3, isEqual3[Int])
<console>:15: error: type mismatch;
 found   : ((Int, Int)) => Boolean
 required: (?, ?) => Boolean
       contains2(l1, 3, isEqual3[Int])
```

And we can easily fix that by untupling the parameters to __isEqual3__:

```{.scala .scrollx}
scala> contains2(l1, 3, Function.untupled(isEqual3))
res69: Boolean = true

```

## Case Class Constructors

And one last example invoking a constructor of a case class:

```{.scala .scrollx}
scala> case class Person(name: String, age: Int)
defined class Person

scala> val nameAge = ("Katz", 20)
nameAge: (String, Int) = (Katz,20)

scala> val pc = Person.apply _
pc: (String, Int) => Person = $$Lambda$1565/1849401610@5417f849
```

If I try to invoke __pc__ with __nameAge__ I get an error as expected:

```{.scala .scrollx}
scala> pc nameAge
<console>:13: error: value nameAge is not a member of (String, Int) => Person
       pc nameAge
```

And we can solve that by tupling the constructor:

```{.scala .scrollx}
scala> pc tupled nameAge
res21: Person = Person(Katz,20)
```

Or more succinctly:

```{.scala .scrollx}
scala> Person.tupled(nameAge)
res22: Person = Person(Katz,20)
```

Hopefully this has given you some insight into the various ways to invoke functions that takes tuples, curried arguments or uncurried variants.

Some references:

- [scala-correct-syntax-to-use-underscore-in-function-literal-with-tuple](https://stackoverflow.com/questions/23449757/scala-correct-syntax-to-use-underscore-in-function-literal-with-tuple)
- [how-to-apply-a-function-to-a-tuple](https://stackoverflow.com/questions/1987820/how-to-apply-a-function-to-a-tuple?rq=1)
- [scala-unpacking-tuple-as-part-of-argument-list](https://stackoverflow.com/questions/24196827/scala-unpacking-tuple-as-part-of-argument-list?noredirect=1&lq=1)