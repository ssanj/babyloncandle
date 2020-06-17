---
title: Contravariant Functors
author: sanjiv sahayam
description: ???
tags: Haskell
comments: true
---

# Contravariant Functors

- Overview of Functors as containers
  - Some common Functors with their implementations [x]
  - Introduce a type that can't have a Functor instance (Predicate) [x]
  - Functor Laws

- Variance/Polarity

- Contravariant Instances
  - Predicate
  - Logger (a -> String)
  - Equality/Comparison

Before we get into what a Contravariant Functor is, it's useful to look at a Functor we already know love.

# Functor

Functor is probably one of the simplest [type classes](https://wiki.haskell.org/Typeclassopedia) in the Haskell ecosystem. It's defined as:

```{.haskell .scrollx}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

We often understand a Functor to be a "container" or a "producer" of some type, where the function supplied to `fmap` is applied to the elements that are "contained" or "produced" in some type constructor `f`.

A simple example would be the `[]` type, that can represent zero or more values. Given a `[a]` we can turn it into a `[b]` when given a function `a -> b`.

```{.haskell .scrollx}
data [a] = [] | a : [a]

instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : map f xs
```

In the example below we convert a `[Int]` into a `[String]` given a function  `Int -> String`:

```{.haskell .scrollx}
import Data.Semigroup ((<>))

myInts :: [Int]
myInts = [1 .. 5]

emptyInts :: [Int]
emptyInts = []

stringMyInt :: Int -> String
stringMyInt n = (show n) <> "!"

myStrings :: [String]
myStrings = fmap stringMyInt myInts -- ["1!","2!","3!","4!","5!"]

myEmptyString :: []
myEmptyString = fmap stringMyInt emptyInts  -- []
```

Another example would the `Maybe` data type, that represents a value that may or may not exist.

```{.haskell .scrollx}
data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
```

In the example below we convert a `Maybe Int` into a `Maybe String`  given a function  `Int -> String`:

```{.haskell .scrollx}
import Data.Semigroup ((<>))

maybeInt :: Maybe Int
maybeInt = Just 10

notInt :: Maybe Int
notInt = Nothing

stringMyInt :: Int -> String
stringMyInt n = (show n) <> "!"

maybeString :: Maybe String
maybeString = fmap stringMyInt maybeInt -- Just "10!"

notString :: Maybe String
notString = fmap stringMyInt notInt -- Nothing
```

## Laws

Functor has laws, that ensure Functor instances behave as expected.

### Identity

```{.haskell .scrollx}
fmap id == id
```

Essentially if you do nothing to the value of a Functor, you get the same Functor you started with.

### Composition

```{.haskell .scrollx}
fmap (f . g) == fmap f . fmap g
```

If you convert the result of a Functor by `fmap`ing with function `g` and then `fmap`ing that result with subsequent function `f`, it's the same as composing functions `g` and `f` (`f . g`) and then `fmap`ing once.

Let's take `Maybe` as an example and try out the laws. The `Maybe` Functor is defined as:

```{.haskell .scrollx}
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
```

Given that we have:

```{.haskell .scrollx}
notInt :: Maybe Int
notInt = Nothing

maybeTenInt :: Maybe Int
maybeTenInt = Just 10

maybeFiveInt :: Maybe Int
maybeFiveInt = Just 5
```

Using `fmap id` on the above:

```{.haskell .scrollx}
fmap id notInt == notInt     -- Nothing
fmap id maybeInt == maybeTenInt -- Just 10
fmap id maybeInt == maybeFiveInt -- Just 5
```

Given that we have:

```{.haskell .scrollx}
stringMyInt :: Int -> String
stringMyInt n = (show n) <> "!"

boolMyString :: String -> Bool
boolMyString "5!" = True
boolMyString _ = False
```

Using **stringMyInt** and then **boolMyString**:

```{.haskell .scrollx}
fmap (boolMyString . stringMyInt) notInt == fmap boolMyString . fmap stringMyInt $ notInt -- Nothing
fmap (boolMyString . stringMyInt) maybeTenInt == fmap boolMyString . fmap stringMyInt $ maybeTenInt -- Just False
fmap (boolMyString . stringMyInt) maybeFiveInt == fmap boolMyString . fmap stringMyInt $ maybeFiveInt -- Just True
```

TODO: George W quote on laws

Now let's look at something a little different. Let's create a data type to wrap a predicate of some sort. A predicate is something that will evaluate to a `Bool`:

```{.haskell .scrollx}
newtype Predicate a = Predicate { getPredicate :: a -> Bool }
```

An example of a Predicate could be **greaterThanTen**:

```{.haskell .scrollx}
greaterThanTen :: Predicate Int
greaterThanTen = Predicate (\n -> n > 10)
```

that tests whether a number is greater than ten.

We can run with it **getPredicate** and an `Int`:

```{.haskell .scrollx}
getPredicate greateThanTen 5  -- False
getPredicate greateThanTen 11 -- True
```

It could be useful to define a Functor for Predicate - say if we have a `Predicate Int` and we want to convert it into a `Predicate String` when we have a `Int -> String` function. Let's try and implement that:

```{.haskell .scrollx}
instance Functor Predicate where
  -- fmap (a -> b) -> Predicate a -> Predicate b
  fmap f (Predicate p) = Predicate (\b -> undefined)
  fmap f (Predicate (a -> Bool)) = Predicate (\b -> undefined)  -- expanding p
  fmap (a -> b) (Predicate (a -> Bool)) = Predicate (\b -> undefined) -- expanding f
```

Now we've run into a small problem:

> How do we compose (a -> Bool) with (a -> b) to give us a (b -> Bool) ?

The problem is that we can't. It's because of something called "polarity" of the type variable `a`. No Functor instance for you Predicate. :sad-panda:

# Polarity

Polarity is a way of representing [variance]() using the position of type variables. Let's take a simple function `a -> b` as an example.

![Function Polarity](/images/contravariant/function-polarity.png)

If a type variable is in **input** position like `a` it is given a **negative** polarity. If it is in an **output** position like `b` then it is given a **positive** polarity. These polarities map directly to variant types.

| Polarity | Variance |
| -------- | -------- |
| Positive | Covariant |
| Negative | Contravariant |
| Both | Invariant |

What this means is that Functors (which are actually Covariant Functors) require a data type that has a type variable in the covariant position in order for you to define a Functor instance for that type.

Let's look at a type that we know has a Functor instance like `Maybe`:

![Polarity of the Maybe data type](/images/contravariant/maybe-polarity.png)

We can see that the type variable `a` occurs in a covariant position within the definition of the `Maybe` data type.

Now let's look at the definition of `Predicate` data type:

![Polarity of the Predicate data type](/images/contravariant/predicate-polarity.png)

We can see that the type variable `a` within the definition of the `Predicate` data type occurs in a contravariant position. This would then indicate to us that we can create an instance of a (Covariant) Functor instance for this data type.

But we want to map things! What do we do?

# Contravariant

Welcome the Contravariant Functor to the stage! It is defined as:

```{.haskell .scrollx}
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
```

Snazzy! Contravariant also takes some kind of type constructor `f` just like Functor but it has this weird `contramap` function instead of `fmap`.

```{.haskell .scrollx}
     fmap :: (a -> b) -> f a -> f b -- Functor
contramap :: (a -> b) -> f b -> f a -- Contravariant
                         ^^^
```

If we read `fmap` as:

> If you have an `a` in some context and a function that takes that `a` and converts it to a `b`, I can give you a context with a `b` in it.

we can then read `contramap` as:

> If you have a context that needs an `a` and a function that can convert `b`s to
`a`s, I can give you a context that needs `b`s.

But that probably doesn't make much sense. So let's try and look at this in terms of our non-Functor `Predicate`. `Predicate` has a **need** for `a`, which it then uses to tell if something about that `a` is True or False.

Let's try and write a Contravariant instance for `Predicate`. We know that the type `a` in `Predicate` occurs in a contravariant position, so that should be possible.

```{.haskell .scrollx}
instance Contravariant Predicate where
  -- contramp (a -> b) -> f b -> f a
  contramap (a -> b) -> Predicate b -> Predicate a
  contramap aToB (Predicate bToBool) = Predicate (\a -> undefined)
```

Given that we have a function `a -> b` and essentially a function of type `b -> Bool` (wrapped inside a `Predicate b`), we can if given an `a`, convert it to a `b` using `aToB` and then give that `b` to `bToBool` to give us a `Bool`.

We have gone from:

```{.haskell .scrollx}
a -> b -> Bool
```

Here's a slightly long-form implementation of Contravariant for `Predicate`:

```{.haskell .scrollx}
  contramap (a -> b) -> Predicate b -> Predicate a
  contramap aToB (Predicate bToBool) =
    Predicate $ \a ->
      let b    = aToB a
          bool = bToBool b
      in bool
```

or more succinctly:

```{.haskell .scrollx}
contramap (a -> b) -> Predicate b -> Predicate a
contramap f (Predicate b) = Predicate $ b . f
```

We can see from the definition of `Predicate b` that all we are doing is running the supplied function `f` to `contramap` **before** the function within `Predicate b`. We run the function **before** some existing functionality to adapt new inputs types to match the existing inputs.

If we revisit the (Covariant) Functor instance for `Maybe`:

```{.haskell .scrollx}
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap aToB (Just a) = Just (aToB a)
```

we can see that the function `aToB` is run **after** we have a value of `a`. We run the function **after** to convert a result of some type to another type.

These are the essential differences between covariant and contravariant Functors:

| Functor | Function runs | Purpose |
| ------- | ------------ | ---------- |
| Covariant | after | Convert results |
| Contravariant | before | adapt inputs |


Just like Functor has laws around its instance, Contravariant also has laws around its instances.

TODO: List Contravariant Laws
TODO: Expand with an example to make them clear

## More Polarity


TODO: Add CallBack example from FP Complete
TODO: Add Endo example
TODO: Should we talk about Invariant at the end?

# Variance


Let's take a look at the `CallbackRunner` example from [FP Complete](https://tech.fpcomplete.com/blog/2016/11/covariance-contravariance/)

```{.haskell .scrollx}
newtype CallbackRunner a = CallbackRunner
    {
        runCallback :: (a -> IO ()) -> IO ()
    } -- what is the polarity of a?

-- in the callback `(a -> IO ())` a is in negative position
-- in the overall function `(a -> IO ()) -> IO ()`, `(a -> IO ())` is in
-- negative position also, therefore multiplying a negative with a negative is
-- going to give us a positive. This means that we should be able to write a
-- Functor instance for  `CallbackRunner`:

instance Functor CallbackRunner where
    fmap f (CallbackRunner aCallbackRunner) =
        CallbackRunner $ \bCallback ->
            aCallbackRunner (bCallback . f)
```

## Polarity

[George Wilson](https://twitter.com/georgetalkscode) states that types in a type signature can  be in positive or negative position.

Types on their own are positive position. Some examples are:

- `number :: Int`
- someStrings :: [String]
- maybeInts :: Maybe Int

Function return types are in positive position but function parameters are in negative position:

- `length :: [a] -> Int`
- `show :: Show a =>  a -> String`

- Examples for each

A typical example could be a function from `a -> b` where we focus our function on the "input" type `a` instead of the output type `b`.

For `f` to be an instance of Functor, every `a` in `f a` must be in positive position. If that's so we say `f` is covariant in `a`.

Spot the Covariant Functor

```{.haskell .scrollx}
data Maybe a = Nothing | Just a
```

Maybe is a Functor because `a` appears in a positive position

```{.haskell .scrollx}
newtype Endo a = Endo { appEndo :: a -> a}
```
Endo is not a Functor because `a` appears in both positive and negative position. It is Invariant.

```{.haskell .scrollx}
newtype Predicate a = Predicate { getPredicate :: a -> Bool }
```

`a` is in negative position and hence Predicate is not a Functor, but a Contravariant Functor.

# Variant Types
- Why do we need variance?
- Enzymes vs Antibodies
- Table from Typed-Driven Development

# What is the motivation for it?

# What is it?

# Why are they useful? Why do we need it

# Contravariant
- Signature
- Diagram (cat theory diagrams, diagrams similar to Kleisli blog)
- Laws
- Typeclassopedia
- Combinators
- Similar to Adaptor pattern?

# Examples
- Predicate
- Logger
- Json

# Links
- [George Wilson](https://twitter.com/georgetalkscode   )'s Presentations:
    - [The Extended Functor Family](https://www.youtube.com/watch?v=JZPXzJ5tp9w)
    - [Contravariant Functors - The Other Side of the Coin](https://www.youtube.com/watch?v=IJ_bVVsQhvc&t)
- [Covariance and Contravariance](https://tech.fpcomplete.com/blog/2016/11/covariance-contravariance/)
- [Contravariant Package](http://hackage.haskell.org/package/contravariant-1.5.2)
