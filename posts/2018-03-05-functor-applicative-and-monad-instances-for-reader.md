---
title: Functor, Applicative and Monad instances for Reader
author: sanjiv sahayam
description: Using Functor, Applicative and Monad instances for Reader can be confusing. This article tries to make that application simpler and offers some intuition on how to use them.
tags: haskell, typeclasses
comments: true
---

How do you define a Reader (-> r) instance of a __Functor__, __Applicative__ or even a __Monad__? A Reader is a function that takes some resource __r__ and returns another value. This has been something that has always confused me. After an initial peruse it all makes sense for a while but when next faced with the same problem I can't remember how these instances are implemented.

I'd like to analyse how the Reader instances are derived for each of __Functor__, __Applicative__ and __Monad__ and test it against some examples to gain some intuition. Also note that ((->) r) and (r ->) can be used interchangeably. Thanks to [Brian McKenna](https://www.youtube.com/watch?v=qH0EjlM9Cm4) for that useful titbit.

## Functor

A functor typeclass is defined as:

```{.haskell .scrollx}
class Functor f where
  fmap, (<$>) :: (a -> b) -> f a -> f b
```

__fmap__ or __<$>__ basically runs a function (__a -> b__), on  a value within some context __f a__ and returns the context with the function applied to its value as an __f b__.

```{.haskell .scrollx}
(a -> b)  -- f', a function that requires an 'a' to create a 'b'
f a       -- Functor with an 'a'
f (f'(a)) -- apply f' to the 'a'
f b       -- the final result of a 'b'
```

Let's take a look at the __Functor__ instance for [Maybe](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Maybe):

```{.haskell .scrollx}
instance Functor Maybe where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Just a) =  Just (f a)
  fmap _ Nothing  =  Nothing
```

With __Maybe__, the function __f__, is applied to a value within a __Just__ or not applied if the value is a __Nothing__.

When we hear that (-> r) is also a Functor it can boggle our minds a little. How do we define an instance for that?

```{.haskell .scrollx}
instance Functor (-> r) where
  fmap f = -- what goes here?
```

We need a function that takes some resource __r__ and returns some other value. Let's have a crack at deriving the implementation for Functor:

```{.haskell .scrollx}
instance Functor (r -> ) where
-- fmap :: (a -> b) -> f a -> f b
  fmap fab      f a       = f b                 -- refer to (a -> b) as fab
  fmap fab      (\r -> a) = (\r -> b)           -- given that the Functor is (r ->), replace 'f' with (r ->)
  fmap fab      fra       = (\r -> b)           -- refer to (r -> a) as fra so we can use it
  fmap fab      fra       = (\r -> ??? (fra r)) -- we have an 'r' and we have something that needs an 'r' and returns an 'a'.
  fmap fab      fra       = (\r -> fab (fra r)) -- We have an 'a' and something that needs an 'a' to return a 'b'
  fmap fab      fra       = fab . fra           -- we can simplify this to composing fab and fra
```

We are applying the function __fab__ to the result of __fra__. It looks like __fmap__ takes two functions are composes them.

Compose (.) is defined as:

```{.haskell .scrollx}
(.) :: (b -> c) ->  (a -> b) -> a -> c
```

or in our case:

```{.haskell .scrollx}
(.) :: (a -> b) ->  (r -> a) -> r -> b
```

And we can implement the Functor for (r ->) with compose alone:

```{.haskell .scrollx}
instance Functor (r -> ) where
  fmap = (.)
```

This gives us the intuition that fmap over functions is just composition.

Let's use it on an example:

```{.haskell .scrollx}
fmap (*3) (+100) 1
```

What is the result of the above?

Let's use function composition to get the answer:

```{.haskell .scrollx}
fmap (*3) (+100) 1
= (\r -> (r + 100) * 3) -- expanding Functor
= ((1 + 100) * 3)       -- substituting 1 for 'r'
= 303
```

## Applicative

The Applicative typeclass is defined as:

```{.haskell .scrollx}
class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The __pure__ function lifts some value __a__ into the __Applicative__, __f__. Also note that __f__ is also a __Functor__. The __<$>__ function sequences a function from (__a -> b__) within an __Applicative__ context, with the value of __a__ supplied in another __Applicative__ context to produce the result __b__ in a third __Applicative__ context.

Note the similarities between __<$>__ and <*>:

```{.haskell .scrollx}
fmap, (<$>) ::   (a -> b) -> f a -> f b
(<*>)       :: f (a -> b) -> f a -> f b
```

The only difference is that with <*> the function is within a context __f__.

```{.haskell .scrollx}
f (a -> b)  -- f', a function within a context 'f', requires an 'a' to create a 'b'
f a         -- Applicative Functor with an 'a'
f (f'(a))   -- apply f' to the 'a' within 'f'
f b         -- the final result of a 'b'
```

Let's take a look at the __Applicative__ instance for __Maybe__:

```{.haskell .scrollx}
instance Applicative Maybe where
-- pure  :: a -> f a
  pure = Just
-- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Just f) other = fmap f other
  (<*>) Nothing  _     = Nothing
```

For __Maybe__, __pure__ simply creates an instance of __Just__ with the supplied value. With <*> the function __f__ is within a __Maybe__ context. If the context is a __Just__, the function is applied to the other __Maybe__ context using __fmap__ from the __Functor__ typeclass. If the context is a __Nothing__, no function application takes place and a __Nothing__ is returned.

How do we define an __Applicative__ instance for (r ->) ?

```{.haskell .scrollx}
instance Applicative (r -> ) where
-- pure  :: a -> f a
  pure a = \r -> a
-- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) f g = \r -> f r (g r) -- f is (\r -> (a -> b)), g is (\r -> a)
```

Apply the input __r__ to __g__ to return an __a__ and also apply __r__ to __f__, to return the function from (__a -> b__). Then apply the function (__a -> b__) to __a__ to return a __b__.

Let's use it on an example:

```{.haskell .scrollx}
(+) <$> (+3) <*> (*100) 5
= (+) <$> (\r -> r + 3) <*> (\r -> r * 100) 5 -- expanding Applicative
= (+) <$> (5 + 3) (5 * 100)                   -- substituting 5 for 'r'
= 8 + 500                                     -- combining with (+)
= 508
```

You may also notice that this gives you the same answer as __liftA2__:

```{.haskell .scrollx}
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 (+) (+3) (*100) 5
= 508
```

The intuition here is that, we can supply the input to each __Applicative__ context, and then combine them with a function either through __<$>__ or __liftA2__.

And here's one more example which may seem a little hairy:

```{.haskell .scrollx}
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
= (\x y z -> [x,y,z]) <$> (\r -> r +3) <*> (\r -> *2) <*> (\r -> /2) $ 5 -- expand Applicative
= (\x y z -> [x,y,z]) <$> (5 + 3) <*> (5 * 2) <*> (5 / 2)                -- replace 'r' with 5
= (\x y z -> [x,y,z]) <$> (8.0) <*> (10.0) <*> (2.5)
= [8.0, 10.0, 2.5]                                                       -- combine with (\x y z -> [x,y,z])
```

The same result can be achieved with __liftA3__:

```{.haskell .scrollx}
liftA3 (\x y z -> [x,y,z]) (+3) (*2) (/2) $ 5
= [8.0,10.0,2.5]
```

## Monad

The __Monad__ typeclass is defined as:

```{.haskell .scrollx}
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```

The __return__ function lifts a value __a__ into the __Monad__ __m__. Bind or (__>>=__) sequentially composes two actions, passing any value produced by the first as an argument to the second.

Let's take a look at the __Monad__ instance for __Maybe__ :

```{.haskell .scrollx}
instance  Monad Maybe  where
-- (>>=)  :: m a -> (a -> m b) -> m b
  (Just a) >>= k  = k a
  Nothing  >>= _  = Nothing
```

If the first __Maybe__ context is a __Just__, then apply the function __k__ to produce a new __Maybe__ context. If the first __Maybe__ context is a __Nothing__, then return __Nothing__.

How do we define an __Monad__ instance for (r ->) ?

```{.haskell .scrollx}
instance  Monad (r ->)  where
-- return :: a -> m a
    return = pure
-- (>>=)  :: m a -> (a -> m b) -> m b
  f >>= g = \r ->  g (f r) r -- f is (\r -> a), g is (\a -> \r -> b)
```

The __return__ function is derived from __pure__, since all __Monads__ are also __Applicatives__. The bind function (__>>=__) first applies the input __r__ to __f__ to give an __a__. It then applies the __a__ to __g__ to return a function from (__r -> b__). The input __r__ is then applied to this function to return the final __b__.

The intuition here is that we supply the input resource __r__ to __f__ and use that result as the first input to __g__ followed by __r__ as the second input.

Let's use it in an example:

```{.haskell .scrollx}
(+3) >>= (*) $ 5
= (\r -> r + 3) >>= (\a -> \r -> a * r) 5 -- expanding the Monad for 'r'
= (5 + 3) >>= (\a -> a * 5)               -- replace 'r' with 5
= (8) >>= (\a -> a * 5)
= (8 * 5)                                 -- replace 'a' with 8
= 40
```

or simply:

```{.haskell .scrollx}
(+3) >>= (*) $ 5
= ((5+3) * 5)
= (8 * 5)
= 40
```

We can also use the do syntax to solve the above:

```{.haskell .scrollx}
let z1 = do
           x <- (+3)
           (x *)
z1 5
= 40
```

### Join

The __join__ function flattens nested __Monads__ and is defined as:

```{.haskell .scrollx}
join :: (Monad m) => m (m a) -> m a
join x = x >>= id
```

Given the following:

```{.haskell .scrollx}
join (+) 10
```

armed with the what we know about __Monads__, what is its result?

```{.haskell .scrollx}
join (+) 10
-- m (m a) -> m a
= (\r -> (\r -> r + r)) 10 -- expanding the Monad for 'r'
= (10 + 10)                -- replacing 'r' with 10
= 20
```

We can also use the do syntax to solve the above:

```{.haskell .scrollx}
let z2 = do
           x <- (+)
           x
z2 10
= 20
```
