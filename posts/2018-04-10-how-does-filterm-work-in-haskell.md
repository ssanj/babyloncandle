---
title: How does filterM work in Haskell?
author: sanjiv sahayam
description: How to use filterM in Haskell to filter things in by a condition and by an effect.
tags: haskell
comments: true
---

__filterM__ is an interesting function. In one sense it's very similar to __filter__ which we all know and love. It's much more powerful though as we shall soon see. Let's start by having a look at the definition of __filter__:

```{.haskell .scrollx}
filter :: (a -> Bool) -> [a] -> [a]
```

and then at filterM:

```{.haskell .scrollx}
filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
```

A side-by-side comparison:

```{.haskell .scrollx}
filter ::                   (a ->   Bool) -> [a] ->   [a]
filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
```

By comparing the type signatures of __filter__ and __filterM__ we can see that __filterM__ is just __filter__ where the conditional expression yields a __Bool__ within a context __m__ and where the matching results are aggregated in the __m__ context.

The implementation of __filterM__ in [GCH base](https://hackage.haskell.org/package/base-4.11.0.0/docs/src/Control.Monad.html#filterM) is as follows:

```{.haskell .scrollx}
filterM   :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x -> liftA2 (\flg -> if flg then (x:) else id) (p x)) (pure [])
```

From the above definition it looks like whenever the monadic filter function `(a -> m Bool)` returns a `m True`, the value in the supplied list is prepended to an accumulator, and if it doesn't match the existing accumulator is left unchanged.

Although this sound very simple, I found the usage of __filterM__ to be somewhat difficult to understand - at least at first. Let's start investigating its usage by looking at some example instances for __m__.

## Maybe

Given a list of numbers:

```{.haskell .scrollx}
numbers :: [Int]
numbers = [1,2,3,4,5]
```

and an __isEven__ function:

```{.haskell .scrollx}
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0
```

we can use __filterM__ to filter the list of numbers that are even and return the results in a __Maybe__:

```{.haskell .scrollx}
filterM (Just . isEven) numbers
```

which results in:

```{.haskell .scrollx}
Just [2,4]
```

That seems pretty easy. Using __filter__ on __numbers__:

```{.haskell .scrollx}
filter isEven numbers
```

we get:

```{.haskell .scrollx}
[2,4]
```

The only difference between the results being that the __filterM__ variant has the results in the __Maybe__ Monad.

What happens when __filterM__ takes a function that can return __Nothing__ in some instances?

Given the following function:

```{.haskell .scrollx}
isDivisibleByThree :: Int -> Bool
isDivisibleByThree n = n `mod` 3 == 0
```

Let's filter our list of numbers so that they contain even numbers, but if we encounter a number that is divisible by three, we want to bail on the result:

```{.haskell .scrollx}
filterM (\n -> if isDivisibleByThree n then Nothing else Just (isEven n)) numbers
```

this results in:

```{.haskell .scrollx}
Nothing
```

Now, this might be a little surprising. What happened to all the matches until we encountered a three, such as two? Recall that the __filterM__ implementation:

```{.haskell .scrollx}
filterM p = foldr (\x -> liftA2 (\flg -> if flg then (x:) else id) (p x)) (pure [])
```

uses __liftA2__:

```{.haskell .scrollx}
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

 to run a binary function over the Applicative instances. With two __Maybe__ instances, the result is always __Nothing__, if one of them is __Nothing__ as you can't run the function without both inputs:

```{.haskell .scrollx}
liftA2 (+) (Just 1) (Just 2) = Just 3
liftA2 (+) (Just 1) Nothing  = Nothing
liftA2 (+) Nothing (Just 2)  = Nothing
liftA2 (+) Nothing Nothing   = Nothing
```

What this demonstrates is that if we ever receive a __Nothing__ value while using __filterM__ all results up until that point are discarded. This highlights one key difference between __filter__ and __filterM__; in addition to filtering on the __Bool__ result, __filterM__ also combines the results using its Applicative properties.

Let's run the __filterM__ code once again, but this time, we'll leave out any multiples of three:

```{.haskell .scrollx}
filterM (\n -> if isDivisibleByThree n then Nothing else Just (isEven n)) [1,2,4,5,7,8]
```

and this time the answer is:

```{.haskell .scrollx}
Just [2,4,8]
```

## IO

Let's try filtering only even numbers using the __IO__ Monad:

```{.haskell .scrollx}
ioFilterM (pure . isEven) numbers
= [2, 4] -- IO [Int]
```

That works as expected. Now let's introduce a failure in __IO__ Monad when a number is divisible by three:

```{.haskell .scrollx}
filterM (\n -> if isDivisibleByThree n then ioError (userError "boom!") else pure (isEven n)) numbers
= *** Exception: user error (boom!) -- IO [Int]
```

The above discards any results collected once it reaches an __IO__ error. This functionality is very similar to how the __Maybe__ Monad filtered when it received a __Nothing__. This is quite useful when filtering only valid results and failing on the first failure.

And if we remove any numbers divisible by three:

```{.haskell .scrollx}
filterM (\n -> if isDivisibleByThree n then ioError (userError "boom!") else pure (isEven n)) [1,2,4,5,7,8]
= [2,4,8] -- IO [Int]
```

we get back the expected results.

## List

With List, things get more interesting. Consider the following:

```{.haskell .scrollx}
filterM (\n -> [True, False]) numbers
```

What do you reckon the answer would be? Probably not a [powerset](https://en.wikipedia.org/wiki/Power_set):

```{.haskell .scrollx}
[[1,2,3,4,5],[1,2,3,4],[1,2,3,5],[1,2,3],[1,2,4,5],[1,2,4],[1,2,5],[1,2],
[1,3,4,5],[1,3,4],[1,3,5],[1,3],[1,4,5],[1,4],[1,5],[1],[2,3,4,5],[2,3,4]
,[2,3,5],[2,3],[2,4,5],[2,4],[2,5],[2],[3,4,5],[3,4],[3,5],[3],[4,5],[4],
[5],[]]
```

Remember that __filterM__ is defined as:

```{.haskell .scrollx}
filterM p = foldr (\x -> liftA2 (\flg -> if flg then (x:) else id) (p x)) (pure [])
```

How does this work with List? If we use __liftA2__ with List:

```{.haskell .scrollx}
liftA2 (+) [1,2,3] [4,5,6]
= [5,6,7,6,7,8,7,8,9]
```
we see that we get a [Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of values (all combinations). List is a non-deterministic Monad and as such it produces results of every possible combination.

Let's start by expanding out the point-free implementation of __filterM__:

```{.haskell .scrollx}
filterM p =
  foldr (\x acc -> liftA2 (\flg1 accx -> if flg1 then (x:accx) else accx) (p x) acc) (pure [])
```

__accx__ is the accumulator value passed to __liftA2__. The values passed will be the Cartesian product of __[True, False]__ and the accumulator of list __acc__, which is initially __[[]]__.

There are two main expansions happening in the implementation of __filterM__:

1. __liftA2__ is creating a Cartesian product of the flags __[True, False]__ and the accumulator __acc__ and combining them with supplied function, which prepends the current value of the list __x__ to the accumulator __accx__ if the flag is True or returns the existing accumulator __accx__ if it is False.
1. All the combinations returned from __listA2__ are then returned into __foldr__ as the new value of the accumulator __acc__.

Because __filterM__ is implemented using __foldr__ the accumulated values are used from last to first.

 Given the following legend:

```{.terminal .scrollx}
x      -- element in the list
acc    -- value of accumulator
accx   -- value of accumulator at current combination
flg1   -- value of flag at current combination
result -- value of accx after applying flg1
newacc -- value of acc returned to foldr
```

Let's start from the end of the list at 5 and follow it up to 1.

For the value of 5:

```{.terminal .scrollx}
x = 5
acc = [[]]
flags = [True, False]
--------------------
accx []
flg1 True
result = 5:[] => [5]
--------------------
accx []
flg1 False
result => []
--------------------
newacc = [[5], []]
```

For the value of 4:

```{.terminal .scrollx}
x = 4
acc = [[5], []]
flags = [True, False]
--------------------
accx [5]
flg1 True
result = 4:[5] => [4,5]
--------------------
accx []
flg1 True
result = 4:[] => [4]
--------------------
accx [5]
flg1 False
result => [5]
--------------------
accx []
flg1 False
result => []
--------------------
newacc = [[4,5],[4],[5], []]
```

For the value of 3:

```{.terminal .scrollx}
x = 3
acc = [[4,5],[4],[5], []]
flags = [True, False]
--------------------
accx [4,5]
flg1 True
result = 3:[4,5] => [3,4,5]
--------------------
accx [4]
flg1 True
result = 3:[4] => [3,4]
--------------------
accx [5]
flg1 True
result = 3:[5] => [3,5]
--------------------
accx []
flg1 True
result = 3:[] => [3]
--------------------
accx [4,5]
flg1 False
result => [4,5]
--------------------
accx [4]
flg1 False
result => [4]
--------------------
accx [5]
flg1 False
result => [5]
--------------------
accx []
flg1 False
result => []
--------------------
newacc = [[3,4,5],[3,4],[3,5],[3],[4,5],[4],[5],[]]
```

For the value of 2:

```{.terminal .scrollx}
x = 2
acc = [[3,4,5],[3,4],[3,5],[3],[4,5],[4],[5],[]]
flags = [True, False]
--------------------
accx [3,4,5]
flg1 True
result = 2:[3,4,5] => [2,3,4,5]
--------------------
accx [3,4]
flg1 True
result = 2:[3,4] => [2,3,4]
--------------------
accx [3,5]
flg1 True
result = 2:[3,5] => [2,3,5]
--------------------
accx [3]
flg1 True
result = 2:[3] => [2,3]
--------------------
accx [4,5]
flg1 True
result = 2:[4,5] => [2,4,5]
--------------------
accx [4]
flg1 True
result = 2:[4] => [2,4]
--------------------
accx [5]
flg1 True
result = 2:[5] => [2,5]
--------------------
accx []
flg1 True
result = 2:[] => [2]
--------------------
accx [3,4,5]
flg1 False
result => [3,4,5]
--------------------
accx [3,4]
flg1 False
result => [3,4]
--------------------
accx [3,5]
flg1 False
result => [3,5]
--------------------
accx [3]
flg1 False
result => [3]
--------------------
accx [4,5]
flg1 False
result => [4,5]
--------------------
accx [4]
flg1 False
result => [4]
--------------------
accx [5]
flg1 False
result => [5]
--------------------
accx []
flg1 False
result => []
--------------------
newacc = [[2,3,4,5],[2,3,4],[2,3,5],[2,3],[2,4,5],[2,4],[2,5],[2],[3,4,5],[3,4],[3,5],[3],[4,5],[4],[5],[]]
```

For the value of 1:

```{.terminal .scrollx}
x = 1
acc = [[2,3,4,5],[2,3,4],[2,3,5],[2,3],[2,4,5],[2,4],[2,5],[2],[3,4,5],[3,4],[3,5],[3],[4,5],[4],[5],[]]
flags = [True, False]
--------------------
accx [2,3,4,5]
flg1 True
result = 1:[2,3,4,5] => [1,2,3,4,5]
--------------------
accx [2,3,4]
flg1 True
result = 1:[2,3,4] => [1,2,3,4]
--------------------
accx [2,3,5]
flg1 True
result = 1:[2,3,5] => [1,2,3,5]
--------------------
accx [2,3]
flg1 True
result = 1:[2,3] => [1,2,3]
--------------------
accx [2,4,5]
flg1 True
result = 1:[2,4,5] => [1,2,4,5]
--------------------
accx [2,4]
flg1 True
result = 1:[2,4] => [1,2,4]
--------------------
accx [2,5]
flg1 True
result = 1:[2,5] => [1,2,5]
--------------------
accx [2]
flg1 True
result = 1:[2] => [1,2]
--------------------
accx [3,4,5]
flg1 True
result = 1:[3,4,5] => [1,3,4,5]
--------------------
accx [3,4]
flg1 True
result = 1:[3,4] => [1,3,4]
--------------------
accx [3,5]
flg1 True
result = 1:[3,5] => [1,3,5]
--------------------
accx [3]
flg1 True
result = 1:[3] => [1,3]
--------------------
accx [4,5]
flg1 True
result = 1:[4,5] => [1,4,5]
--------------------
accx [4]
flg1 True
result = 1:[4] => [1,4]
--------------------
accx [5]
flg1 True
result = 1:[5] => [1,5]
--------------------
accx []
flg1 True
result = 1:[] => [1]
-------------------- *
accx [2,3,4,5]
flg1 False
result => [2,3,4,5]
--------------------
accx [2,3,4]
flg1 False
result => [2,3,4]
--------------------
accx [2,3,5]
flg1 False
result => [2,3,5]
--------------------
accx [2,3]
flg1 False
result => [2,3]
--------------------
accx [2,4,5]
flg1 False
result => [2,4,5]
--------------------
accx [2,4]
flg1 False
result => [2,4]
--------------------
accx [2,5]
flg1 False
result => [2,5]
--------------------
accx [2]
flg1 False
result => [2]
--------------------
accx [3,4,5]
flg1 False
result => [3,4,5]
--------------------
accx [3,4]
flg1 False
result => [3,4]
--------------------
accx [3,5]
flg1 False
result => [3,5]
--------------------
accx [3]
flg1 False
result => [3]
--------------------
accx [4,5]
flg1 False
result => [4,5]
--------------------
accx [4]
flg1 False
result => [4]
--------------------
accx [5]
flg1 False
result => [5]
--------------------
accx []
flg1 False
result => []
--------------------

newacc = [[1,2,3,4,5],[1,2,3,4],[1,2,3,5],[1,2,3],[1,2,4,5],[1,2,4],[1,2,5],[1,2],[1,3,4,5],[1,3,4],[1,3,5],[1,3],[1,4,5],[1,4],[1,5],[1],[2,3,4,5],[2,3,4],[2,3,5],[2,3],[2,4,5],[2,4],[2,5],[2],[3,4,5],[3,4],[3,5],[3],[4,5],[4],[5],[]]
```

That was a bit harder than necessary!

## Either

Using __filterM__ with __Either__ is pretty much the same as a __Maybe__:

```{.haskell .scrollx}
let e1 = filterM (\x -> if x == 11 then Left "You gave me eleven" else Right (isEven x))
-- e1 :: :: Integral a => [a] -> Either [Char] [a]
e1 [1 .. 10]
= Right [2,4,6,8,10] -- only even numbers
e1 [1 .. 11]
= Left "You gave me eleven" -- drops all results on a Left
```

## State

Now let's use a Monad that has two type holes which are both used together. The State Monad allows us to return a value and thread through some state we are interested in at the same time. Let's use our __isEven__ method to filter in all the even inputs and use a list to record all the values inspected along the way:

```{.haskell .scrollx}
let x1 = filterM (\x -> state (\s -> (isEven(x), s ++ [x]))) [1 .. 10]
-- x1 :: (Integral a, Monad m) => StateT [a] m [a]
evalState x1 []          -- get value
= [2,4,6,8,10]           -- only even numbers
execState x1 []          -- get state
= [1,2,3,4,5,6,7,8,9,10] -- the state - all inspected values
```

The interesting thing to note is that given __x1__'s type:

```{.haskell .scrollx}
x1 :: (Integral a, Monad m) => StateT [a] m [a]
```

The __m__ in __filterM__:

```{.haskell .scrollx}
filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
```

is:

```{.haskell .scrollx}
StateT [a] m
```
which is why we can return a Bool in the value position and have it filter the inputs for us.

Hopefully that was somewhat easier to understand. You can find alternate explanations to this problem [here](https://stackoverflow.com/questions/25476248/powerset-function-1-liner#45105085) and [here](https://byorgey.wordpress.com/2007/06/26/deducing-code-from-types-filterm).
