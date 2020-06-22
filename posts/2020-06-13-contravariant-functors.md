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

Before we get into what a Contravariant Functor is, it's useful to look at the  Functor typeclass which we know and love.

# Functor

Functor is probably one of the simplest [type classes](https://wiki.haskell.org/Typeclassopedia) in the Haskell ecosystem. It's defined as:

```{.haskell .scrollx}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

We often understand a Functor to be a "container" or a "producer" of some type, where the function supplied to `fmap` is applied to the elements that are "contained" or "produced" in some type constructor `f`.

A simple example would be the `[]` type, that can represent zero or more values. Given a `[a]` we can turn it into a `[b]` when given a function `a -> b`.

```{.haskell .scrollx}
data [a] = [] | a : [a] -- an approximation of the the [] data type

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

intToString :: Int -> String
intToString n = (show n) <> "!"

myStrings :: [String]
myStrings = fmap intToString myInts -- ["1!","2!","3!","4!","5!"]

myEmptyString :: []
myEmptyString = fmap intToString emptyInts  -- []
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

intToString :: Int -> String
intToString n = (show n) <> "!"

maybeString :: Maybe String
maybeString = fmap intToString maybeInt -- Just "10!"

notString :: Maybe String
notString = fmap intToString notInt -- Nothing
```

The Functor typeclass has laws, that ensure Functor instances behave in a predictable way.

## Laws

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

![Functor Laws in Category Theory](/images/contravariant/functor-laws-ct.png)

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
fmap id notInt   == notInt       -- Nothing
fmap id maybeInt == maybeTenInt  -- Just 10
fmap id maybeInt == maybeFiveInt -- Just 5
```

Given that we have:

```{.haskell .scrollx}
intToString :: Int -> String
intToString n = (show n) <> "!"

stringToBool :: String -> Bool
stringToBool "5!" = True
stringToBool _ = False
```

Using **intToString** and then **stringToBool**:

```{.haskell .scrollx}
-- # Identity law
-- ###############################

-- ## for notInt
fmap id notInt  == notInt
fmap id Nothing == Nothing -- expanding notInt on both sides
Nothing         == Nothing -- simplifying fmap on Nothing


-- ## for maybeTenInt
fmap id maybeTenInt == maybeTenInt
fmap id (Just 10)   == Just 10  -- expanding maybeTenInt on both sides
Just 10             == Just 10  -- simplifying fmap

-- ## for maybeFiveInt
fmap id maybeFiveInt == maybeFiveInt
fmap id (Just 5)     == Just 5  -- expanding maybeFiveInt on both sides
Just 5               == Just 5  -- simplifying fmap

-- # Composition law
-- ###############################

-- ## for notInt
fmap (stringToBool . intToString) notInt  == fmap stringToBool . fmap intToString $ notInt

-- lhs
fmap (stringToBool . intToString) notInt
fmap (stringToBool . intToString) Nothing -- expanding notInt
=> Nothing                                -- simplifying fmap on Nothing

-- rhs
fmap stringToBool . fmap intToString $ notInt
fmap stringToBool . (fmap intToString Nothing) -- expanding notInt
fmap stringToBool   (Nothing) -- simplifying fmap on Nothing for intToString
fmap stringToBool    Nothing  -- simplifying fmap on Nothing for stringToBool
=> Nothing

lhs     == rhs
Nothing == Nothing


-- ## for maybeTenInt
fmap (stringToBool . intToString) maybeTenInt == fmap stringToBool . fmap intToString $ maybeTenInt

-- lhs
fmap (stringToBool . intToString) maybeTenInt
fmap (stringToBool . intToString) (Just 10)             -- expanding maybeTenInt
fmap (stringToBool . (\n -> (show n) <> "!")) (Just 10) -- expanding intToString
Just (stringToBool . ((show 10) <> "!"))                -- simplifying for n == 10
(Just (stringToBool "10!"))                             -- applying stringToBool with "10!"
(Just (\s ->
        case s of
          "5!" -> True
          _    -> False -- branch chosen because s == "10!"
      ))                -- expanding stringToBool
= Just False

-- rhs
fmap stringToBool . fmap intToString (Just 10)             -- expanding maybeFiveInt
fmap stringToBool . fmap (\n -> (show n) <> "!") (Just 10) -- expanding intToString
Just (stringToBool . ((show 10) <> "!"))                   -- simplifying for n == 10
(Just $ stringToBool "10!")                                -- applying stringToBool with "10!"
Just ((\s ->
        case s of
          "5!" -> True
          _    -> False -- branch chosen because s == "10!"
      ))                -- expanding stringToBool
= Just False

lhs        == rhs
Just False == Just False

-- ## for maybeFiveInt
fmap (stringToBool . intToString) maybeFiveInt == fmap stringToBool . fmap intToString $ maybeFiveInt

-- lhs
fmap (stringToBool . intToString) maybeFiveInt
fmap (stringToBool . intToString) (Just 5)             -- expanding maybeFiveInt
fmap (stringToBool . (\n -> (show n) <> "!")) (Just 5) -- expanding intToString
Just (stringToBool . ((show 5) <> "!"))                -- simplifying for n == 5
(Just (stringToBool "5!"))                             -- applying stringToBool with "5!"
(Just (\s ->
        case s of
          "5!" -> True  -- branch chosen because s == "5!"
          _    -> False
      ))                -- expanding stringToBool
= Just True

-- rhs
fmap stringToBool . fmap intToString (Just 5)             -- expanding maybeFiveInt
fmap stringToBool . fmap (\n -> (show n) <> "!") (Just 5) -- expanding intToString
Just (stringToBool . ((show 5) <> "!"))                   -- simplifying for n == 5
(Just $ stringToBool "5!")                                -- applying stringToBool with "5!"
Just ((\s ->
        case s of
          "5!" -> True  -- branch chosen because s == "5!"
          _    -> False
      ))                -- expanding stringToBool
= Just True

lhs       == rhs
Just True == Just True
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

We can see that the type variable `a` within the definition of the `Predicate` data type occurs in a contravariant position. This indicates that we can't create a (Covariant) Functor instance for this data type.

But we want to map things! What do we do?

# Contravariant

Welcome the Contravariant Functor to the stage! It is defined as:

```{.haskell .scrollx}
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
```

Snazzy! Contravariant also takes some kind of type constructor `f` just like Functor but it has this weirdly named `contramap` function instead of `fmap`.

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

But that probably doesn't make much sense. So let's try and look at this in terms of our non-Functor: `Predicate`. `Predicate` has a **need** for an `a`, which it then uses to tell if something about that `a` is True or False.

Let's try and write a Contravariant instance for `Predicate` given that we know that the type `a` in `Predicate` occurs in a contravariant position.

```{.haskell .scrollx}
instance Contravariant Predicate where
  -- contramp (a -> b) -> f b -> f a
  contramap (a -> b) -> Predicate b -> Predicate a
  contramap aToB (Predicate bToBool) = Predicate (\a -> undefined)
```

Given that we have a function `a -> b` and essentially a function of type `b -> Bool` (wrapped inside a `Predicate b`), we can if given an `a`, convert it to a `b` using `aToB` and then give that `b` to `bToBool` to give us a `Bool`.

Here's a slightly long-form implementation of Contravariant for `Predicate`:

```{.haskell .scrollx}
instance Contravariant Predicate where
  -- contramap :: (a -> b) -> Predicate b -> Predicate a
  contramap aToB (Predicate bToBool) =
    Predicate $ \a ->
      let b    = aToB a
          bool = bToBool b
      in bool
```

![contramap on Predicate](/images/contravariant/contramap-predicate.png)

or more succinctly:

```{.haskell .scrollx}
instance Contravariant Predicate where
  -- contramap :: (a -> b) -> Predicate b -> Predicate a
  contramap f (Predicate b) = Predicate $ b . f
```

We can see from the definition of `Predicate b` that all we are doing is running the supplied function `f` **before** the function within `Predicate b`. The reason we do that is to adapt a new input type to match an existing input type for some functionality.

If we revisit the (Covariant) Functor instance for `Maybe`:

```{.haskell .scrollx}
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap aToB (Just a) = Just (aToB a)
```

we can see that the function `aToB` is run **after** we have a value of `a`. We do that to convert a result of some type to another type.

![fmap on Maybe](/images/contravariant/fmap-maybe.png)

These are the essential differences between covariant and contravariant Functors:

| Functor | Function runs | Purpose |
| ------- | ------------ | ---------- |
| Covariant | after | Convert results |
| Contravariant | before | adapt inputs |


Now that we know the essential difference between Functor and Contravariant, let's look at how we can use contramap with your `Predicate` class.

Given that we already have a `Predicate` that determines whether a number is greater than ten:

```{.haskell .scrollx}
numGreaterThanTen :: Predicate Int
numGreaterThanTen = Predicate (\n -> n > 10)
```

say we want to write another `Predicate` that verifies that the length of String is greater than ten characters.

```{.haskell .scrollx}
strLengthGreaterThanTen :: Predicate String
strLengthGreaterThanTen = Predicate (\s -> (length s) > 10)
```

Sure, that's pretty contrived but bear with me. Let's also say we have a `Person` data type and we want to know if a person's name is over ten characters long - if so we consider that to be a long name.

```{.haskell .scrollx}
data Person = Person { personName :: String, personAge :: Int }

personLongName :: Predicate Person
personLongName = Predicate (\p -> (length . personName $ p) > 10)
```

And we can run these `Predicate` as:

```{.haskell .scrollx}
getPredicate numGreaterThanTen 5 -- False
getPredicate numGreaterThanTen 20 -- True

getPredicate strLengthGreaterThanTen "hello"       -- False
getPredicate strLengthGreaterThanTen "hello world" -- True

getPredicate personLongName $ Person "John" 30        -- False
getPredicate personLongName $ Person "Bartholomew" 30 -- True
```

And this is fine, but there's some duplication across each of the `Predicate`s - namely the part where we compare a number to ten:

```{.haskell .scrollx}
(\n -> n > 10)  -- Int
(\s -> (length s) > 10) -- String
(\p -> (length . personName $ p) > 10) -- Person
```

It would be nice if we didn't have to repeat ourselves.

If we look at the differences between **numGreaterThanTen**, **strLengthGreaterThanTen** and **personLongName** we can see that the only difference is that one works on an Int and the others work on String and Person respectively. **strLengthGreaterThanTen** and **personLongName** each convert their input types to an Int and then do the same comparison:

```{.haskell .scrollx}
Predicate (\(n :: Int) ->
  let num = id n
  in num > 10 -- (1)
) -- numGreaterThanTen


Predicate (\(s :: String) ->
  let num = length s
  in num > 10 -- (1)
) -- strLengthGreaterThanTen

Predicate (\(p :: Person) ->
  let name = personName p
      num  = length name
  in num > 10 -- (1)
) -- personLongName

```

The above expansion of the functions demonstrates that even though the `Predicate`s themselves have different input types, at the end they are all converted to a number which is compared against the number ten. This is tagged with `(1)` in the above example.

We can also see that the only changes between the `Predicate`s is the conversion from one type to another **before** running our comparison function `(1)`. This is our clue that we can use **contramap** here to reuse some functionality.

```{.haskell .scrollx}
numGreaterThanTen :: Predicate Int
numGreaterThanTen = Predicate (\n -> n > 10)

strLengthGreaterThanTen2 :: Predicate String
strLengthGreaterThanTen2 = contramap length numGreaterThanTen -- convert the String to an Int, then pass it to numGreaterThanTen

personLongName2 :: Predicate Person
personLongName2 = contramap (length . personName) numGreaterThanTen -- convert the Person to an Int, then pass it to numGreaterThanTen
```

We get the same results as before:

```{.haskell .scrollx}
getPredicate strLengthGreaterThanTen2 "hello" -- False
getPredicate strLengthGreaterThanTen2 "hello world" -- True

getPredicate personLongName2 $ Person "John" 30 -- False
getPredicate personLongName2 $ Person "Bartholomew" 30 -- True
```

Now we have rewritten **strLengthGreaterThanTen** and **personLongName** in terms of **numGreaterThanTen** by just running a function before it to convert the types. This is a simple example of a Contravariant Functor where we can reuse some existing functionality for a given type if we can convert from our other types to that type through some mapping function.

We can also go a little further and reuse even more:

```{.haskell .scrollx}
personLongName3 :: Predicate Person
personLongName3 = contramap personName strLengthGreaterThanTen -- convert the Person to a String, then pass it to strLengthGreaterThanTen

```

## Laws

Just like Functor has laws, Contravariant also has laws around its instances.

### Identity

```{.haskell .scrollx}
contramap id == id
```

Essentially if you do not change the value of a Contravariant Functor, you get the same Contravariant Functor you started with.

Let's take `Predicate` as an example and try out the identity law. The `Predicate` Contravariant Functor is defined as:

```{.haskell .scrollx}
 instance Contravariant Predicate where
   -- contramap :: (a -> b) -> f b -> f a
   contramap f (Predicate p) = Predicate (p . f)
```

Given that we have a `Predicate Int`:

```{.haskell .scrollx}
numGreaterThanTen :: Predicate Int
numGreaterThanTen = Predicate (\n -> n > 10)
```

Using `contramap id` on the above:

```{.haskell .scrollx}
-- identity law
contramap id numGreaterThanTen == numGreaterThanTen

-- lhs
Predicate (p . f) -- applying contramap
Predicate (p . id) -- expanding f
Predicate (p) -- applying f
Predicate (\n -> n > 10) -- expanding p

-- rhs
numGreaterThanTen
Predicate (\n -> n > 10) -- expanding numGreaterThanTen

-- equality
lhs                      == rhs
Predicate (\n -> n > 10) == Predicate (\n -> n > 10)
```

### Composition

```{.haskell .scrollx}
contramap f . contramap g = contramap (g . f)
```

If you convert the input to some Contravariant Functor by `contramap`ing with function `g` and then convert its input to some other type by `contramap`ing again with a function `f`, it's the same as composing the functions `f` and `g` (`g . f`) and then `contramap`ing once. Notice the order of composition is switched as opposed to when we looked at the Functor laws.

Once again using `Predicate` as an example, let's explore the compositional law of Contravariance.

Given that we have the following `Predicate`s:

```{.haskell .scrollx}
numGreaterThanTen :: Predicate Int
numGreaterThanTen = Predicate (\n -> n > 10)

length :: [a] -> Int
personName :: Person -> String
```

Using **numGreaterThanTen**, with **length** and **personName**:

```{.haskell .scrollx}
-- composition law
contramap personName . contramap length $ numGreaterThanTen = contramap (length . personName) numGreaterThanTen


-- lhs
contramap personName . contramap length $ numGreaterThanTen
contramap personName . contramap length $ Predicate (\n -> n > 10) -- expanding numGreaterThanTen
contramap personName (Predicate $ \str ->
  let num  = length str
     bool  = num > 10
  in bool
) -- applying length
Predicate $ \person ->
  let str = personName person
      num = length str
     bool = num > 10
  in bool
) -- applying personName
=> Predicate Person

-- rhs
contramap (length . personName) numGreaterThanTen
contramap (\person ->
    let str = personName person
        num = length str
    in num
) numGreaterThanTen -- expanding length . personName
Predicate (\person ->
   let str  = personName person
       num  = length str
       bool = num > 10 -- expanding numGreaterThanTen
   in bool
)
=> Predicate Person

-- equality
lhs == rhs

Predicate (\person ->
  let str  = personName person
      num  = length str
      bool = num > 10
  in bool

) ==
Predicate (\person ->
   let str  = personName person
       num  = length str
       bool = num > 10
   in bool
)
```

![Contravariant Functor Laws in Category Theory](/images/contravariant/contravariant-laws-ct.png)

## Combinators
```{.haskell .scrollx}
-- infixl 4
(>$<) :: Contravariant f => (a -> b) -> f b -> f a

-- infixl 4
(>$) :: b -> f b -> f a

-- infixl 4
($<) :: Contravariant f => f b -> b -> f a

-- infixl 4
(>$$<) :: Contravariant f => f b -> (a -> b) -> f a
code
```

## More Examples

### Aeson

### Logger

# More Polarity


TODO: Add CallBack example from FP Complete
TODO: Add Endo example
TODO: Should we talk about Invariant at the end?


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

## Polarity Multiplication Table

# What is the motivation for it?

# What is it?

# Why are they useful? Why do we need it

# Contravariant
- Typeclassopedia (from George W)
- Similar to Adaptor pattern?

# Links
- [George Wilson](https://twitter.com/georgetalkscode   )'s Presentations:
    - [The Extended Functor Family](https://www.youtube.com/watch?v=JZPXzJ5tp9w)
    - [Contravariant Functors - The Other Side of the Coin](https://www.youtube.com/watch?v=IJ_bVVsQhvc&t)
- [Covariance and Contravariance](https://tech.fpcomplete.com/blog/2016/11/covariance-contravariance/)
- [Contravariant Package](http://hackage.haskell.org/package/contravariant-1.5.2)
