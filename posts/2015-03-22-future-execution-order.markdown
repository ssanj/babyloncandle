---
title: Future Execution Order
author: sanjiv sahayam
description: An experiment to determine two things&#58; 1. When does a Future execute. 2. The order of Future execution within a for-comprehension.
tags: future,scala
---

Futures had me confused. For some reason I had assumed that the execution of Futures were somehow linked to for-comprehensions. I had somehow assumed that you compose your Futures first and then run them when you were good and ready. Something similar to the Reader Monad in Haskell. I couldn't have been more wrong.

Defining Futures outside a for-comprehension
--------------------------------------------

Let's take a simple example. First I define an __iterate__ method that takes in a name and a number. The name is used to track the Future that is iterating. The method basically loops form 1 to the number passed in while printing out its current iteration. It then sleeps for 250 ms. It prints out when it is done at the end. All very simple.

```{.scala .scrollx}

  final def iterate(name:String, n:Int): Int = {
    println(s"defining $name")
    (1 to n).foreach { n =>
      println(s"$name sleeping for item: $n")
      Thread.sleep(250)
    }

    println(s"$name done")
    n
  }
```

Let's take Scenario 1. This scenario creates two Futures: __f1__ and __f2__ with ten and two iterations respectively. It then sleeps for ten seconds after which it processes the results of the Futures and sums them up to give another Future: __f3__. We then wait for a maximum of one minute for __f3__ to complete and print out "done".

```{.scala .scrollx}
    val f1: Future[Int] = Future {
      iterate("f1", 10)
    }
    val f2: Future[Int] = Future {
      iterate("f2", 2)
    }

    Thread.sleep(10000) //sleep for 10 seconds

    println("before comprehension")

    val f3:Future[Int] = for {
      x <- {println("f1 ->"); f1}
      y <- {println("f2 ->"); f2}
    } yield x + y

    f3.onComplete(_ => println("f3 done"))

    println("after comprehension")

    val result = Await.result(f3, Duration(1, TimeUnit.MINUTES))

    println(s"done with $result")
```

What is the output of the above program?

```{.terminal}
defining f1
defining f2
f2 sleeping for item: 1
f1 sleeping for item: 1
f2 sleeping for item: 2
f1 sleeping for item: 2
f1 sleeping for item: 3
f2 done
f1 sleeping for item: 4
f1 sleeping for item: 5
f1 sleeping for item: 6
f1 sleeping for item: 7
f1 sleeping for item: 8
f1 sleeping for item: 9
f1 sleeping for item: 10
f1 done
before comprehension
f1 ->
f2 ->
after comprehension
f3 done
done with 12
```

What does this tell us? This tells us that the Futures: __f1 __ and  __f2 __ start executing almost immediately after they are defined and they run concurrently.  __f2 __ finishes before  __f1 __.  __f1 __ finishes before the for-comprehension is reached. The for-comprehension executes and creates a Future __f3 __ which starts executing in the background. The values of  __f1 __ and  __f2 __ are extracted in that order and we are done when __f3 __ finishes.

The point in time when a Future starts executing has nothing to do with for-comprehensions - as long as the Futures are defined outside the for-comprehension (more on this below.) The main thread of execution continues along its merry way without blocking at the for-comprehension and waiting for a result. The for-comprehension also creates a new Future which then immediately begins running the computation asynchronously. All very curious.

Defining Futures inside a for-comprehension
-------------------------------------------

Let's look at Scenario 2. This scenario basically creates two Futures within the for-comprehension: __f1__ and __f2__ with ten and two iterations respectively, as in Scenario 1. It then processes the results of the Futures within the for-comprehension and sums them up to give another Future: __f3__. We then wait for a maximum of one minute for __f3__ to complete and print out "done".

 but this time define the Futures f1 and f2 within the for-comprehension.

```{.scala .scrollx}
    println("before comprehension")

    val f3:Future[Int] = for {
      x <- Future { println("f1 ->"); iterate("f1", 10) }
      y <- Future { println("f2 ->"); iterate("f2", 2) }
    } yield x + y

    f3.onComplete(_ => println("f3 done"))

    println("after comprehension")

    val result = Await.result(f3, Duration(1, TimeUnit.MINUTES))

    println(s"done with $result")

```

What is the output of the above program?

```{.terminal}
before comprehension
f1 ->
after comprehension
defining f1
f1 sleeping for item: 1
f1 sleeping for item: 2
f1 sleeping for item: 3
f1 sleeping for item: 4
f1 sleeping for item: 5
f1 sleeping for item: 6
f1 sleeping for item: 7
f1 sleeping for item: 8
f1 sleeping for item: 9
f1 sleeping for item: 10
f1 done
f2 ->
defining f2
f2 sleeping for item: 1
f2 sleeping for item: 2
f2 done
f3 done
done with 12
```

We can see that Future: __f1__, starts executing only after the for-comprehension defined. We can see that the main thread of execution does not block on the for-comprehension and continues executing. __f2__ has not started executing at all. It is only once __f1__ completes that __f2__ starts executing. We now have synchronous execution as opposed to asynchronous execution of __f1__ and __f2__. No concurrency here. Once __f2__ completes, __f3__ finishes straight after and we are done.

So it looks like these are the general rules of Future execution:

1. Define Futures you want to run concurrently outside a for-comprehension.
2. Define Futures you want to run sequentially inside a for-comprehension.

What about zip?
---------------

Just when we had everything nice and tidy I came across the __zip__ method on [scala.concurrent.Future](http://www.scala-lang.org/files/archive/nightly/docs/library/index.html#scala.concurrent.Future). Let's run Scenario 2 with the zip method and call it Scenario 3.

```{.scala .scrollx}
    println("before comprehension")

    val f3:Future[Int] = for {
      (x, y) <- Future { println("f1 ->"); iterate("f1", 10) } zip Future {
        println("f2 ->"); iterate("f2", 2) }
    } yield x + y

    f3.onComplete(_ => println("f3 done"))

    println("after comprehension")

    val result = Await.result(f3, Duration(1, TimeUnit.MINUTES))

    println(s"done with $result")
```

What is the output of the above program?

```{.terminal}
before comprehension
f1 ->
f2 ->
defining f2
defining f1
f1 sleeping for item: 1
f2 sleeping for item: 1
after comprehension
f2 sleeping for item: 2
f1 sleeping for item: 2
f2 done
f1 sleeping for item: 3
f1 sleeping for item: 4
f1 sleeping for item: 5
f1 sleeping for item: 6
f1 sleeping for item: 7
f1 sleeping for item: 8
f1 sleeping for item: 9
f1 sleeping for item: 10
f1 done
f3 done
done with 12
```

It looks very much like Scenario 2, where we had __f1__ and __f2__ running concurrently. So how does this work? The __zip__ function is defined on Future as:

```{.scala .scrollx}
  def zip[U](that: Future[U]): Future[(T, U)] = {
    implicit val ec = internalExecutor
    val p = Promise[(T, U)]()
    onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[(T, U)]]
      case Success(s) => that onComplete { c => p.complete(c map { s2 => (s, s2) }) }
    }
    p.future
  }
```

So how do __f1__ and __f2__ run concurrently? The answer lies in the how the second Future is passed to the __zip__ method. __f1__ starts executing immediately before its __zip__ method is called. Since __zip__ takes in a ```(that: Future[U])``` instead of a ```(that: => Future[U])```, __that__ starts executing immediately as a side-effect when passed to the __zip__ method. Talk about yucky.

So if I wrote function:  __zip2__ that took __that__ as a function we should see the same results as Scenario two. Let's call this Scenario 4.

```{.scala .scrollx}
  private def zip2[T,U](one: Future[T])(two: => Future[U])(implicit ec:ExecutionContext): Future[(T, U)] = {
    val p = Promise[(T, U)]()
    one.onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[(T, U)]]
      case Success(s) => two onComplete { c => p.complete(c map { s2 => (s, s2) }) }
    }
    p.future
  }


    println("before comprehension")

    val f3:Future[Int] = for {
      (x, y) <- zip2(Future { println("f1 ->"); iterate("f1", 10) })(Future {println("f2 ->"); iterate("f2", 2) })
    } yield x + y

    f3.onComplete(_ => println("f3 done"))

    println("after comprehension")

    val result = Await.result(f3, Duration(1, TimeUnit.MINUTES))

    println(s"done with $result")
```

We can see that __f1__ has to complete before __f2__:

```{.terminal}
before comprehension
f1 ->
defining f1
after comprehension
f1 sleeping for item: 1
f1 sleeping for item: 2
f1 sleeping for item: 3
f1 sleeping for item: 4
f1 sleeping for item: 5
f1 sleeping for item: 6
f1 sleeping for item: 7
f1 sleeping for item: 8
f1 sleeping for item: 9
f1 sleeping for item: 10
f1 done
f2 ->
defining f2
f2 sleeping for item: 1
f2 sleeping for item: 2
f2 done
f3 done
done with 12
```

If we __zip__ within a for-comprehension, then Futures run concurrently right? That depends. We still need to define the __zip__ call in the first step of the for-comprehension. Take Scenario 5 as an example.

```{.scala .scrollx}
    println("before comprehension")

    val f4:Future[Int] = for {
      a <- Future {println("f1 ->"); iterate("f1", 5) }
      (x, y) <- Future { println("f2 ->"); iterate("f2", 10) } zip Future {
        println("f3 ->"); iterate("f3", 2) }
    } yield a + x + y

    f4.onComplete(_ => println("f4 done"))

    println("after comprehension")

    val result = Await.result(f4, Duration(1, TimeUnit.MINUTES))

    println(s"done with $result")
```

If we look at the output, we see that __f1__ has to complete before __f2__ and __f3__ start executing concurrently.


```{.terminal}
before comprehension
f1 ->
after comprehension
defining f1
f1 sleeping for item: 1
f1 sleeping for item: 2
f1 sleeping for item: 3
f1 sleeping for item: 4
f1 sleeping for item: 5
f1 done
f2 ->
defining f2
f2 sleeping for item: 1
f3 ->
defining f3
f3 sleeping for item: 1
f3 sleeping for item: 2
f2 sleeping for item: 2
f3 done
f2 sleeping for item: 3
f2 sleeping for item: 4
f2 sleeping for item: 5
f2 sleeping for item: 6
f2 sleeping for item: 7
f2 sleeping for item: 8
f2 sleeping for item: 9
f2 sleeping for item: 10
f2 done
f4 done
done with 17
```

Where should we Define Futures?
-------------------------------

Yikes. What a minefield. So the rules seem to be:

1. If you defined Futures outside a for-comprehension they will run immediately and concurrently.
2. If you define Futures inside a for-comprehension they will run sequentially (if not zipped).
3. If you zip Futures in the first step of the for-comprehension they will run immediately and concurrently.
4. If you zip Futures in secondary steps of the for-comprehension, they will not run until all preceding steps have completed successfully. They will then run concurrently.

The source code for the above scenarios can be found on [github](https://github.com/ssanj/future-execution-order-blogpost-example)
