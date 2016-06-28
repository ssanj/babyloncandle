---
title: Scalaz Try Operations
author: sanjiv sahayam
description: Choosing scalaz imports for additional Try functionality can be confusing. This post describes a couple of ways to import this functionality.
tags: scala, scalaz
comments: true
---

If you are looking to use scalaz to get some additional functionality for your vanilla ```scala.util.Try ``` class, then you've got a couple of options. This can be confusing at first because you might not know which import to use.

## 1. Functions that accept a Try instance

To import only functions that must be supplied a Try instance use:

```{.scala .scrollx}
import scalaz.std.`try`._
```

This will give you functions of the form:

```{.scala .scrollx}
def cata[A, B](t: Try[A])(success: A => B, failure: Throwable => B): B

def toDisjunction[A](t: Try[A]): Throwable \/ A

def fromDisjunction[T <: Throwable, A](d: T \/ A): Try[A]

def toValidation[A](t: Try[A]): Validation[Throwable, A]

def toValidationNel[A](t: Try[A]) : ValidationNel[Throwable, A]

def fromValidation[T <: Throwable, A](v: Validation[T, A]) : Try[A]
```

Example:

```{.scala .scrollx}
cata(Try(..))(..)
```

## 2. Functions that are added to your Try instance

To get a pimped up version of Try use:

```{.scala .scrollx}
import scalaz.syntax.std.`try`._
```

This will give you functions directly on your Try instance:

```{.scala .scrollx}
final def cata[B](success: A => B, failure: Throwable => B): B

final def toDisjunction: Throwable \/ A

final def toValidation: Validation[Throwable, A]

final def toValidationNel: ValidationNel[Throwable, A]
```

Example:

```{.scala .scrollx}
Try(..).cata(..)
```