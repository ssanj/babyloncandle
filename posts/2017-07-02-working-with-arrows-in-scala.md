---
title: Working with Arrows in Scala
author: sanjiv sahayam
description: An overview of functions on the Arrow typeclass in Scala with Cats and Scalaz
tags: arrows, scala,
comments: true
---

In the [last article](http://sanj.ink/posts/2017-06-12-reading-configuration-with-kleisli-arrows.html) we looked at how we could read configuration with a Kleisli Arrow similar to using a Reader Monad.

We've been using Arrows for the last couple of articles but haven't defined what an Arrow is exactly.

An Arrow is a computation that runs within a context which takes in an input and returns an output. A more detailed explanation from [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Arrow) states:

> The Arrow class represents another abstraction of computation, in a similar vein to Monad and Applicative. However, unlike Monad and Applicative, whose types only reflect their output, the type of an Arrow computation reflects both its input and output. Arrows generalize functions: if arr is an instance of Arrow, a value of type b `arr` c can be thought of as a computation which takes values of type b as input, and produces values of type c as output. In the (->) instance of Arrow this is just a pure function; in general, however, an arrow may represent some sort of “effectful” computation

In [Cats](http://typelevel.org/cats/) the [Arrow typeclass](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Arrow.scala#L8) is defined with the type constructor F which has two type holes:

```{.scala .scrollx}
Arrow[F[_, _]] //simplified
```

These two type holes correspond to the input and output types of the Arrow respectively. __F__ can be any type constructor that takes two types and performs a mapping between them. A __scala.Function1__ is an example of __F__, as is the __Kleisli Arrow__ we saw in previous articles. It might be helpful to think of Arrows as simple functions from one type to another for the moment.

Lets now go through some of the functions defined on Arrow and how they are used. For the remainder of the article lets assume that the type constructor supplied to Arrow is a [__scala.Function1__](http://www.scala-lang.org/api/current/scala/Function1.html):

```{.scala .scrollx}
trait Function1[-T1, +R] extends AnyRef
```

and the resulting Arrow is:

```{.scala .scrollx}
val fa = Arrow[Function1]
```

## [lift](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Arrow.scala#L13)/[arr](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L16)

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

In [Scalaz](https://github.com/scalaz) this function is defined as arr:

```{.scala .scrollx}
def arr[A, B](f: A => B): A =>: B
```
where __=>:__ is a typeconstructor similar to __F__.

## [id](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Category.scala#L11)

The __id__ function is defined as:

```{.scala .scrollx}
def id[A]: F[A, A]
```

The type signature of the above tells us that __F__ returns the input type A as its output, essentially giving us the [identity](http://www.scala-lang.org/api/2.11.11/index.html#scala.Predef$@identity[A](x:A):A) function.

```{.scala .scrollx}
val intF1 = fa.id[Int] //Function1[Int, Int]
intF1(10) //returns 10
```

## [first](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/functor/Strong.scala#L24)

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

## [second](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/functor/Strong.scala#L39)

The __second__ function is very similar to __first__ only with its parameters switched. It is defined as:

```{.scala .scrollx}
def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
```

The __second__ function takes Arrow __fa__ from __A__ => __B__ and returns another Arrow with takes in a tuple of (__C__, __A__) => (__C__, __B__). It applies the function in __fa__ to the second parameter of the tuple __A__ and converts it to a __B__. The first parameter of the tuple it leaves untouched and returns a (_C_, __B__).

![Second](/images/arrow-functions/arrow-second2.jpg)

For example if we wanted to apply a function to the __Age__ element of a __Name__ and __Age__ pair and but wanted to leave the __Name__ element untouched we could do:

```{.scala .scrollx}
val onlyAgeF: ((Name, Age)) => (Name, Age) = fa.second[Age, Age, Name](doubleAge)
val toPersonF: ((Name, Age)) => Person = onlyAgeF andThen (Person.apply _).tupled
toPersonF(name, age) //returns Person(Name(Nagate,Tanikaze),Age(44))
```

Notice how the __Name__ value of the input is unchanged.

## [split/product/\*\*\*](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Split.scala)

The __split__ function is an application of __first__ and __second__. It is defined as:

```{.scala .scrollx}
def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)]
```

The __split__ function takes Arrow __f__ from __A__ => __B__ and an Arrow __g__ from __C__ => __D__ and returns another Arrow with takes in a tuple of (__A__, __C__) => (__B__, __D__). It applies the function in __f__ to the first parameter of the tuple __A__ and converts it to a __B__. It also applies the function in __g__ to the second parameter of the tuple __C__ and converts it to a __D__ returning a final result of (__B__, __D__). Split has the symbolic representation of __\*\*\*__ and is sometimes referred to as the __product__ function because it applies multiple functions to multiple inputs.

![Split](/images/arrow-functions/arrow-split3.jpg)

For example if we wanted to apply a function to the __Name__ and __Age__ element of a __Name__ and __Age__ pair at once we could do:

```{.scala .scrollx}
val bothNameAndAgeF: ((Name, Age)) => (Name, Age) = fa.split[Name, Name, Age, Age](upperName, doubleAge)
val toPersonF: ((Name, Age)) => Person = bothNameAndAgeF andThen (Person.apply _).tupled
toPersonF(name, age)//Person(Name(NAGATE,Tanikaze),Age(44))
```

## [combine/fanout/&&&](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L55)

__combine__ is defined as:

```{.scala .scrollx}
def combine[A, B, C](fab: F[A, B], fac: => F[A, C]): F[A, (B, C)]
```

Although Cats does not define __combine__, [scalaz does](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Arrow.scala#L55). For the purpose of this post I've created an implementation of __combine__ in the [example source](https://github.com/ssanj/arrows/blob/master/src/main/scala/net/ssanj/arrow/ArrowFuncs.scala#L8).

The __combine__ function takes Arrow __fab__ from __A__ => __B__ and an Arrow __fac__ from __A__ => __C__ and returns another Arrow which takes in an input of __A__, and returns a tuple of (__B__, __C__). It's important to note that the same input __A__ is supplied to both arrows __fab__ and __fac__.

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

__combine__ has a symbolic representation of __&&&__ and is sometimes referred to as the __fanout__ function.


## liftA2

__liftA2__ is defined as:

```{.scala .scrollx}
def liftA2[A, B, C, D](fab: F[A, B], fac: F[A, C])(f: B => C => D): F[A, D] //simplified
```

I could not find a definition of __liftA2__ in either Cats nor Scalaz. I've referenced it here directly from the [Generalising monads to arrows paper by John Hughes](https://www.researchgate.net/publication/222520426_Generalising_monads_to_arrows) in Haskell:

```{.haskell .scrollx}
liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
```

A sample of implementation of this can be found in the [example source](https://github.com/ssanj/arrows/blob/master/src/main/scala/net/ssanj/arrow/ArrowFuncs.scala#L13).

The __liftA2__ function is very similar to the __combine__ function with the addition of running a function on the result of __combine__.

The __liftA2__ function takes an Arrow __fab__ from __A__ => __B__, an Arrow __fac__ from __A__ => __C__ and a function __f__ from __B__ => __C__ => __D__ and returns another Arrow with takes in an input of __A__, and returns a __D__.

![liftA2](/images/arrow-functions/arrow-liftA2-2.jpg)

For example given a __Person__ if we want to break it into primitive representations of its __Name__ and __Age__ fields and then apply a function on the separated bits we could do:

```{.scala .scrollx}
val person = Person(name, age)
val combineName: Person => String = {
  case Person(Name(first, last), _) => s"$first $last"
}
val combineAge: Person => Int = _.age.age
def makePersonString: String => Int => String = name => age => s"person[name='$name', age=$age]"
val lifta2: Person => String = ArrowFuncs.liftA2(combineName, combineAge)(makePersonString)
lifta2(person) //"person[name='Nagate Tanikaze', age=22]"
```

## [compose/<<<](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Compose.scala#L12) and [andThen/>>>](https://github.com/typelevel/cats/blob/series/0.8.x/core/src/main/scala/cats/arrow/Compose.scala#L15)

__compose__ is defined as:

```{.scala .scrollx}
def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]
```

and has the symbolic representation of __<<<__.

__andThen__ is defined as:

```{.scala .scrollx}
def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C]
```

and has the symbolic representation of __>>>__.

__compose__ and __andThen__ are basically the same function with the first and second arguments swapped.

These functions combine arrows passing on the output of one arrow as the input to the next one, similar to regular function composition.

For example given a __Name__ and __Age__, if we wanted to convert them to a __Person__ and then covert the __Person__ to a String we could do:

```{.scala .scrollx}
def personA: Tuple2[Name, Age] => Person = na => Person(na._1, na._2)
val makePersonStringA: Person  => String = p =>  s"person[name='${p.name.first}' ${p.name.last}, age=${p.age} yrs]"

val composeF: Tuple2[Name, Age] => String = personA >>> makePersonStringA
val andThenF: Tuple2[Name, Age] => String =  makePersonStringA <<< personA

composeF(name, age) //person[name='Nagate' Tanikaze, age=Age(22) yrs]
andThenF(name, age) //person[name='Nagate' Tanikaze, age=Age(22) yrs]
```

## A Worked Example

We've learned a lot of functions which are somewhat cryptic until you start to use them. To make their usage a little clearer lets look at an example.

Assume we have the following functions at our disposal:

```{.scala .scrollx}
  final case class ItemId(id: Long)
  final case class ItemDescReq(itemId: Long, userId: String)
  final case class ItemDetail(itemId: Long, value: Double, desc: String)
  final case class User(name: String, id: String)
  final case class ValuableItemsResponse(expensive: List[ItemDetail], veryExpensive: List[ItemDetail])
  final case class Price(price: Double)

  type UserData = Map[String, List[ItemId]]
  type ItemData = Map[Long, ItemDetail]
  type Results  = List[Either[String, ItemDetail]]

  val userData = Map[String, List[ItemId]](
    "1000" -> List(ItemId(1001), ItemId(1002), ItemId(1003), ItemId(1007), ItemId(1004)),
    "2000" -> List(ItemId(2001), ItemId(2002))
  )

  val itemData = Map[Long, ItemDetail](
    1001L -> ItemDetail(1001, 2000.00,  "Couch"),
    1002L -> ItemDetail(1002, 100.00,   "Apple TV"),
    1003L -> ItemDetail(1003, 75000.00, "Luxury Car"),
    1004L -> ItemDetail(1004, 3000,     "Laptop"),
    2001L -> ItemDetail(2001, 1500.00,  "Coffee Machine"),
    2002L -> ItemDetail(2002, 500.00,   "DLSR")
  )

  val getSavedItems: User => UserData => List[ItemId] = user => data => data.getOrElse(user.id, Nil)

  val idToDesc: User => ItemId => ItemDescReq = user => itemId => ItemDescReq(itemId.id, user.id)

  val getDetails: ItemDescReq => ItemData => Either[String, ItemDetail] = itemDescReq => data =>
    data.get(itemDescReq.itemId).toRight(s"could not find item with id: ${itemDescReq.itemId}")

  val isExpensive: Range => ItemDetail => Boolean = range => item => range.contains(item.value)

  val valuableItemsResponse : Tuple2[Range, Range] => List[ItemDetail] => ValuableItemsResponse = prices => items =>
    ValuableItemsResponse(items.filter(isExpensive(prices._1)), items.filter(isExpensive(prices._2)))

  val valuableItemsResponseString: ValuableItemsResponse => String = items => {
    s"expensive:${itemDetailString(items.expensive)},veryExpensive:${itemDetailString(items.veryExpensive)}"
  }

  val itemDetailString: List[ItemDetail] => String = _.map(id => s"${id.desc}=$$${id.value}").mkString(",")

  val errorString: List[Either[String, ItemDetail]] => String = itemsE =>
    itemsE.collect { case Left(error) => error } mkString("\n")
```

We now want to use the above functions to do the following:

1. Get the saved items for a User.
1. Convert each item to a item request.
1. Look up the details of each item requested. (this may fail)
1. Filter the successful requests against two price ranges, one for _expensive_ and the other for very _expensive_.
1. The filtered items should then be put into a ValuableItemsResponse object.
1. At the end we need to print out a description of the valuable items found and any errors that were generated.

We can then glue these functions together to give us the output we desire:

```{.scala .scrollx}
  // User => (UserData => List[ItemId], (ItemId => ItemDescReq))
  val f1 = ArrowFuncs.combine(getSavedItems, idToDesc)

  // User => List[ItemDescReq]
  val f2 = f1 >>> { case (fi, fd) =>  fi(userData) map fd }

  //User => (Results, Results)
  val f3 = f2 >>> (_ map getDetails) >>> (_ map (_(itemData))) >>> (_.partition(_.isLeft))

  //(Results, Results) => (Results, List[ItemDetail])
  val f4 = fa.second[Results, List[ItemDetail], Results](_ collect { case Right(value) => value })

  //User => (Results, List[ItemDetail])
  val f5 = f3 >>> f4

  //(Results, List[ItemDetail]) => (Results, Tuple2[Range, Range] => ValuableItemsResponse)
  val f6 =
    fa.second[List[ItemDetail],
              Tuple2[Range, Range] => ValuableItemsResponse,
              Results](
      items => prices => valuableItemsResponse(prices)(items)
    )

   //User => (Results, Tuple2[Range, Range] => ValuableItemsResponse)
   val f7 = f5 >>>  f6

   //(Results, Tuple2[Range, Range] => ValuableItemsResponse) => (Results, ValuableItemsResponse)
   val f8 =
    fa.second[
      Tuple2[Range, Range] => ValuableItemsResponse,
      ValuableItemsResponse,
      Results](_(Range(500, 3000), Range(10000, 100000)))

  //User => (Results, ValuableItemsResponse)
  val f9 = f7 >>> f8

  //(Results, ValuableItemsResponse) => (String, String)
  val f10 = fa.split[Results, String, ValuableItemsResponse, String](
    errorString, valuableItemsResponseString
  )

  //User => (String, String)
  val f11 = f9 >>> f10

  val (errors, values) = f11(User("Guybrush threepwood", "1000"))
```

which outputs:

```{.terminal .scrollx}
expensive:Couch=$2000.0,veryExpensive:Luxury Car=$75000.0, errors: could not find item with id: 1007
```

or more succinctly:

```{.scala .scrollx}
val pipeline =
    ArrowFuncs.combine(getSavedItems, idToDesc) >>>
    { case (fi, fd) =>  fi(userData) map fd } >>>
    (_ map getDetails) >>>
    (_ map (_(itemData))) >>>
    (_.partition(_.isLeft)) >>>
    fa.second[Results, List[ItemDetail], Results](_ collect { case Right(value) => value }) >>>
    fa.second[List[ItemDetail], Tuple2[Range, Range] => ValuableItemsResponse, Results](
      items => prices => valuableItemsResponse(prices)(items)
    ) >>>
    fa.second[Tuple2[Range, Range] => ValuableItemsResponse, ValuableItemsResponse, Results](
      _(Range(500, 3000), Range(10000, 100000))
    ) >>>
    fa.split[Results, String, ValuableItemsResponse, String](
      errorString, valuableItemsResponseString
    )

    val (errors, values) = pipeline(User("Guybrush threepwood", "1000"))
```

Hopefully this has given you a gentle introduction into the world of Arrows.