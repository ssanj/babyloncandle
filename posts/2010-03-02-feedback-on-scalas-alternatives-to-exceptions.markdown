---
title: Feedback on Scala&apos;s Alternatives to Exceptions
author: sanjiv sahayam
description: Some useful feedback on my presentation on Exceptions.
tags: fp, scala
---

Here is some of the feedback I received on the questions posed and the solutions provided in my [Scala's Alternatives to Exceptions](http://files.meetup.com/1443989/Scala%27s%20Alternatives%20to%20Exceptions.zip) presentation at the [BFPG](http://www.meetup.com/Brisbane-Functional-Programming-Group-BFG) by [Kristian Domagala](http://kristian-domagala.blogspot.com):

> It looks like Sanjiv raised some interesting questions at the end of his slides. As I wasn't at the meeting, I don't know to what extent the questions were answered, but I thought I would chime in with my 2 cents.

> I've found the Either type to be useful for error conditions that can be resolved either through code (eg, using a fallback), or by re-directing back to the user to address. Often this means the Left and Right types are mapped to the same type, and the Either type is merged into a single value (ie, Either[A,A] => A). In the examples you present, you're essentially doing that all together at the end with the fold function.

> In terms of chaining exceptions, in Scala terms, this can be handled using flatMap function on the right-projection of Either, assuming you use a common type on the left. Sometimes you will need to map the left types to a common type, but I've found it's usually a String or something representing an exception message, and little additional effort is required.

> Going to the particular examples in the slides, I note that the PersistentOutcome type is very similar to Either; ie, Either[String, Unit]. In fact, you could remove the need for pattern matching on the type by aliasing it to Either and re-using the functions defined there:

```{.scala}
type PersistentOutcome = Either[String, Unit]
```

> Alternatively, you could achieve the same thing using an implicit def, so that the pattern matching is done in one place and whenever you see a PersistentOutcome, you can treat it as an Either[String, Unit]:

```{.scala}
implicit def poToStringUnit(po:PersistentOutcome):Either[String,Unit] = po match {
  case Failure(x) => Left(x)
  case Success => Right(())
}
```

> note that you've started to go down the latter path in the reloaded example. The above gives you something you can re-use anywhere. Once you've got Either[String,Unit], it's only one left-map away from becoming Either[Exception,Unit]:

```{.scala .scrollx}
def stringUnitToExUnit(e:Either[String,Unit]):Either[Exception,Unit] = e.left.map(new Exception(_))
```

> Now we're dealing with more consistent types throughout the execution (Either[Exception,_]), and the addSpends function can be reduced to:

```{.scala}
def addSpends(date:Sdate, f:(DailySpend) => Unit):Either[Exception, Unit] =
    Either.joinRight(
      (spender < date).right.flatMap(
        ds => { f(ds); spender > ds }
      ).right.map(stringUnitToExUnit)
    )
```

> To see that I'm not cheating, here's my working:

```{.scala}
def addSpends(date:Sdate, f:(DailySpend) => Unit):Either[Exception, Unit] = {
    val eds:Either[Exception,DailySpend] = spender < date
    val epo:Either[Exception,PersistentOutcome] =
        eds.right.flatMap(ds => {f(ds); spender > ds})
    val esu:Either[Exception,Either[String,Unit]] = epo
    val eeu:Either[Exception,Either[Exception,Unit]] =
        esu.right.map(stringUnitToExUnit)
    val eu:Either[Exception,Unit] = Either.joinRight(eeu)
    eu
  }
```

> Going to the main function, I notice that you're folding twice on the same type (Either[Exception,Unit]). You can get rid of the first fold, and with a little bit of point-free style come down to:

```{.scala}
def main(args: Array[String]) {
  import Function.const
  addSpends(yesterday, addItems1).right.flatMap(
  const(addSpends(today, addItems2))).fold(
  printError, const(printAllSpends))
}
```

> Once again, the long-hand:

```{.scala}
def main(args: Array[String]) {
  val eu1:Either[Exception,Unit] = addSpends(yesterday, addItems1)
  val eu2:Either[Exception,Unit] =
  eu1.right.flatMap(_ => addSpends(today, addItems2))
  val u:Unit = eu2.fold(e => printError(e), _ => printAllSpends)
  u
}
```

> I hope this gives some food for thought and I haven't misinterpreted what you are trying to achieve!

> Going to your chaining solution:

```{.scala}
val spender = new Spender with LedgeredMemoryPersister
type Result = Either[Exception, Unit]

  def main(args: Array[String]) {
    failFast(List(()=> addSpends(yesterday, addItems1),
                  ()=> addSpends(today, addItems2),
                  ()=> addSpends(SomeDay(18, February(), 2010), addItems2),
                  ()=> addSpends(SomeDay(19, February(), 2010), addItems1),
                  ()=> addSpends(SomeDay(20, February(), 2010), addItems1))).
            fold(ex => printError(ex), r => printAllSpends)
  }

  def failFast(funcs:List[Function0[Result]]) : Result = {
    if (funcs.isEmpty) Right({})
    else (funcs.head.apply()).fold(l => Left(l), r => failFast(funcs.tail))
  }

  def addSpends(date:Sdate, f:(DailySpend) => Result) : Result = {
    (spender < date).fold(l => Left(l), ds => f(ds))
  }

```

> I think it is more complicated than it needs to be. Obviously, if you need to support a variable-length list of functions to execute, then you need something similar to what you wrote. Actually, probably what you want is more along the lines of:

```{.scala}
val params = List((yesterday, addItems1), (today, addItems2). ...)
  params.foldLeft(Right(())((r, p) => r.right.flatMap(addSpends(p))
```

> But if you are just using it as a language feature, then there is something already there for you. First I will write out the long-hand version:

```{.scala}
addSpends(yesterday, addItems1).right.flatMap(
    r1 => addSpends(today, addItems2)).right.flatMap(
    r2 => addSpends(SomeDay(18, February(), 2010), addItems2)).right.flatMap(
    r3 => addSpends(SomeDay(19, February(), 2010), addItems1)).right.flatMap(
    r4 => addSpends(SomeDay(20, February(), 2010), addItems1)))).
        fold(ex => printError(ex), r5 => printAllSpends)
}
```

> Using Scala's for comprehensions, this is equivalent to:

```{.scala}
def main(args: Array[String]) {
   for (r1 <- addSpends(yesterday, addItems1).right;
        r2 <- addSpends(today, addItems2)).right;
        r3 <- addSpends(SomeDay(18, February(), 2010), addItems2).right;
        r4 <- addSpends(SomeDay(19, February(), 2010), addItems1).right;
        r5 <- addSpends(SomeDay(20, February(), 2010), addItems1).right)
   yield ()).fold(ex => printError(ex), r => printAllSpends)
}
```

> With an implicit conversion from Either to RightProjection, using '_' for the ignored success return values, moving the success case into the yield statement, and mapping the Left type to Unit, that comes down to:

```{.scala}
def main(args: Array[String]) {
implicit def EtoRP[A,B](e:Either[A,B]) = e.right
Either.merge(
  for (_ <- addSpends(yesterday, addItems1);
       _ <- addSpends(today, addItems2);
       _ <- addSpends(SomeDay(18, February(), 2010), addItems2);
       _ <- addSpends(SomeDay(19, February(), 2010), addItems1);
       _ <- addSpends(SomeDay(20, February(), 2010), addItems1))
  yield (printAllSpends)).left.map(printError))
}
```

> The Either[A,A] type I mentioned is what you need to use Either.merge. In the previous case, it is obtained by calling left.map to convert Either[Exception,Unit] to Either[Unit,Unit].

> Another thing to note is that the examples I've provided are a way to show how to better use Either; not necessarily the best way to solve your problem. I would strongly argue that you have way too much in the way of side-effects in the original code. By isolating the side-effects from the purely functional (read: referentially transparent) parts of the code, you could potentially end up with a much more elegant and (de)composable solution. Learning a language that forces you to deal with side-effects will greatly help with working out the isolation.

> Finally, I haven't run any of the above through a compiler, so don't be surprised if I've left something out/stuffed something up!


Given all the great solutions given by Kristian above I decided to go with the following:

```{.scala .scrollx}
type Result = Either[Exception, Unit]
def addSpends(date:Sdate, f:(DailySpend) => Result) : Result

  ...

  type AddByDate = Tuple2[Sdate, Function1[DailySpend, Result]]
  val params = List[AddByDate](
                    (SomeDay(16, February(), 2010), addItems1),
                    (SomeDay(17, February(), 2010), addItems2),
                    (SomeDay(18, February(), 2010), addItems2),
                    (SomeDay(19, February(), 2010), addItems1),
                    (SomeDay(20, February(), 2010), addItems1))
   params.foldLeft(Right():Result)((r, p) => r.right.flatMap(r1 => addSpends(p._1, p._2))).
           fold(ex => printError(ex), r => printAllSpends)
```

Can the syntax be made cleaner I wonder? Anyway, it's been a great learning experience!