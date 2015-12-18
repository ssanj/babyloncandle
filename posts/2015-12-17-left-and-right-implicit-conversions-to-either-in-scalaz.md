---
title: left and right implicit conversion to Either in Scalaz
author: sanjiv sahayam
description: Simplifying imports for left and right implicit conversions to scalaz Either.
tags: scala, scalaz
comments: true
---

One way to convert any value to a [scalaz](https://github.com/scalaz/scalaz) [Either or \\/](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Either.scala) is via the __left__ and __right__ implicit conversions:

```{.scala .scrollx}
import scalaz._
import Scalaz._

5.right[String]
String \/ Int = \/-(5)

"This is an error".left[Int]
String \/ Int = -\/(This is an error)
```

The basic format is:

```{.scala .scrollx}
right_instance.right[left_type]
left_instance.left[right_type]
```

Importing [Scalaz._](https://github.com/scalaz/scalaz/blob/series/7.1.x/core/src/main/scala/scalaz/Scalaz.scala) brings a lot of unnecessary implicit conversions and types into scope. What if you wanted something a little more light weight?

## scalaz 7.0.x ##

After hunting around I found that the __left__ and __right__ methods are defined in the [scalaz.syntax.IdOps](https://github.com/scalaz/scalaz/blob/series/7.1.x/core/src/main/scala/scalaz/syntax/IdOps.scala) trait and are implemented in the [scalaz.syntax.id](https://github.com/scalaz/scalaz/blob/series/7.1.x/core/src/main/scala/scalaz/syntax/Syntax.scala) object. With that information we can now use cheaper imports for __left__ and __right__:

```{.scala .scrollx}
import scalaz._
import syntax.id._

5.right[String]
String \/ Int = \/-(5)

"This is an error".left[Int]
String \/ Int = -\/(This is an error)
```

## scalaz 7.1.x onwards ##

In scalaz 7.1.x and onwards, the __left__ and __right__ methods are defined in the [scalaz.syntax.EitherOps](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/syntax/EitherOps.scala) trait and implemented in the [scalaz.syntax.either](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/syntax/Syntax.scala) object. Your imports would have to change to:

```{.scala .scrollx}
import scalaz._
import syntax.either._

5.right[String]
String \/ Int = \/-(5)

"This is an error".left[Int]
String \/ Int = -\/(This is an error)
```