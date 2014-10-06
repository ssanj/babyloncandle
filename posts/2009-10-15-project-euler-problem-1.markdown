---
title: Project Euler Problem 1
author: sanjiv sahayam
description: Some attempts at solving Euler problem 1 in Scala.
tags: scala
---

[Tony Morris](http://tmorris.net) used to mention [Project Euler](http://projecteuler.net) frequently back in the day when we used to both work at [Workingmouse](http://www.workingmouse.com). I completely forgot about [Project Euler](http://projecteuler.net) until recently when I started learning Scala again. I thought it would be a good way to further my learning of Scala by solving (or trying to solve) some of the Euler problems. Below is my attempt at solving [problem 1](https://projecteuler.net/problem=1):

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

This was my first solution:

```{.scala .scrollx}
def getTotal(upper:Int) : Int = multiplesBelow(upper - 1)

 def multiplesBelow(start:Int) : Int = {
   if (start <= 0) return 0
   if (isMultiple(start)) start + multiplesBelow(start-1) else multiplesBelow(start - 1)
 }

def isMultiple(number:Int) : Boolean = (number != 0) && ((number % 3 == 0) || (number % 5 == 0))
```

Although this solution will give you the expected answer it is quite clumsy. After a quick scout around the net I found a Ruby solution that used ranges and since Scala also has ranges I came up with the following:

```{.scala .scrollx}
def getTotal2(upper:Int) : Int = (1 until upper).foldLeft(0)(((a,b) => if (b % 3 == 0 || b % 5 == 0) a+b else a))
```

I like the above solution because it is succinct and "simple". I also came up with another solution using map and fold:

```{.scala .scrollx}
def getTotal3(upper:Int) : Int = (1 until upper).map(a => if (a % 3 == 0 || a % 5 == 0) a else 0).foldLeft(0)(_ + _)
```

The above solution has an extra step of mapping the function across the values and then folding it.

And there you have the solution to problem 1. I'm sure there are much neater solutions to problem than those above. Please feel free to comment if you have a better solution and/or comments.