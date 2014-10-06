---
title: Boosting liftA2
author: sanjiv sahayam
description: A simple explanation of liftA2 with some basic examples and the odd diagram.
tags: haskell
---

While reading [_The Gentle Introduction to Monad Transformers_](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md) I came across an interesting use of [__liftA2__](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Applicative.html#v:liftA2). To refresh our memories, __liftA2__ is defined as:

```{.haskell}
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

It basically lifts a binary function, let's call it __g__, across two Applicative Functors (__f a__ and __f b__) that each have one of the parameters __g__ requires (__a__ and __b__ respectively). It then produces the result of applying __g__(_a_  _b_) in a third Applicative Functor (__f c__).

So a simple example would be something like:

```{.haskell .scrollx}
liftA2 (+) (Just 5) (Just 6) = Just 11
```

All very easy so far.

What does liftA2 (<*>) do?

Before we answer that question, let's explore the [<*>](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Applicative.html#v:-60--42--62-) operator.

The starship operator (as I like to call it) from Applicative is defined like this:

```{.haskell .scrollx}
<*> :: Applicative f => f (a -> b) -> f a -> f b
```
The starship operator takes a unary function, let's call it __h__, that's within a Applicative Functor (__f (__ _a -> b_ __)__) and applies it to a value (__a__) also in an Applicative Functor (__f a__). It then returns the result of function application (__h__ _a_) in another Applicative Functor (__f b__).

A simple example of its use would be something like:

```{.haskell .scrollx}
(Just (+5)) <*> (Just 6) = Just 11
```

Again very simple.

So __liftA2__ _lifts_ a binary function into two Applicative Functors to get its result. The __starship__ operator applies a function that requires one value within an Applicative Functor into another Applicative context that has the value it needs.

So back to our question: What does liftA2 (<*>) do?

We can see from the above that __liftA2__ works on Applicative Functors and the __starship__ operator also works on Applicative Functors. It might be useful when we have nested Applicative Functors.

Wait ... what?

Continuing with our example, say we had this:

```{.haskell .scrollx}
let v1 = IO (Just (+5))
let v2 = IO (Just 6)
```

How could we apply the nested +5 function to the nested 6 value to retrieve our result of 11?

With the power of __listA2__ boosted with __starship__ power we could do:

```{.haskell .scrollx}
liftA2 (<*>) v1 v2 = IO (Just 11)
```
That seemed really easy. :)

And now for a __boxes and circles__ diagram:

![diagram of how liftA2 works with the starship operator](/images/diagram_of_how_liftA2_works_with_the_startship_operator.png)

Using liftA2 (<*>) we can simply apply functions within nested Applicatives to values also within nested Applicatives.
