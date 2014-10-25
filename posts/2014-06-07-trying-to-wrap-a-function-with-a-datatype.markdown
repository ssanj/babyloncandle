---
title: Trying to Wrap a Function with a Datatype
author: sanjiv sahayam
description: An attempt and using partial function application to simplify a data constructor in the Hakyll library.
tags: hakyll, haskell
comments: true
---

While using the [Hakyll](http://jaspervdj.be/hakyll) framework, I came across a function with the following definition:

```{.haskell}
renderTagCloudWith :: (Double -> Double ->
                       String -> String -> Int -> Int -> Int -> String)
                       -- ^ Render a single tag link
                       -> ([String] -> String)
                       -- ^ Concatenate links
                       -> Double
                       -- ^ Smallest font size, in percent
                       -> Double
                       -- ^ Biggest font size, in percent
                       -> Tags
                       -- ^ Input tags
                       -> Compiler String
                       -- ^ Rendered cloud
```

The first function, which I will refer to as __renderSingleLink__, had 7 types:

```{.haskell}
(Double -> Double -> String -> String -> Int -> Int -> Int -> String)
```

 I wasn't sure what each input value represented. After some trial and error I figured out what each of the values where. The input values and output are explained below:

``` {.haskell}
1. Double  -> -- Minimum font size as a %
2. Double  -> -- Maximum font size as a %
3. String  -> -- The tag label
4. String  -> -- The tag url
5. Int     -> -- The maximum use of the current tag
6. Int     -> -- The minimum use of the current tag
7. Int     -> -- The maximum use of any tag in the system
8. String  -> -- The html representation of the tag (output)
```
I would have preferred a datatype to encapsulate these values instead of a function with 7 parameters. I was fairly sure I wouldn't remember what each value meant were I to revisit this code a month from now. Also the first two parameters (Double, Double), were in min-max order. The fifth and sixth parameters (Int, Int), were in max-min order. I felt this lead to unnecessary confusion. As I was using Haskell I assumed this would be quite easy to encapsulate in a datatype.


My first attempt was to create a simple datatype called __TagInfo__ :

```{.haskell}
data TagInfo = TagInfo {
                    fontMin :: Double,
                    fontMax :: Double,
                    tagName :: String,
                    tagUrl  :: String,
                    tagMax  :: Int,
                    tagMin  :: Int,
                    maxUseCount :: Int
               }
```

I ordered the parameters to match the order of the __renderSingleLink__. I thought that I could easily compose the data constructor of __TagInfo__ with a function that provided a String-representation of __TagInfo__ to derive a function that could be supplied to __renderTagCloudWith__ :

```{.haskell}
showTag :: TagInfo -> String
```

So basically I wanted to do something like this:

```{.haskell}
showTag . TagInfo
```

and pass that composed function to __renderTagCloudWith__. Unfortunately that does not work. Composing a function that requires one parameter with a function that returns 6 paramaters makes the compiler unhappy!

To clarify, compose (.) is defined as:

```{.haskell}
(.) :: (b -> c) -> (a -> b) -> a -> c
```

The constructor of __TagInfo__ is defined as:


```{.haskell .scrollx}
TagInfo :: Double -> Double -> String -> String -> Int -> Int -> Int -> TagInfo
```

the type of __showTag__ is:

```{.haskell}
showTag :: TagInfo -> String
```

So composing __showTag__ with __TagInfo__ gives us:

```{.haskell}
 showTag   .  TagInfo

Couldn't match type `Double
                     -> String -> String -> Int -> Int -> Int -> TagInfo'
              with `TagInfo'
Expected type: Double -> TagInfo
  Actual type: Double
               -> Double -> String -> String -> Int -> Int -> Int -> TagInfo
In the second argument of `(.)', namely `TagInfo'
In the expression: showTag . TagInfo
```
Unfortunately that didn't work. It seemed so neat to be able to use the __TagInfo__ constructor with __showTag__ to give back the __renderSingleLink__ definition to __renderTagCloudWith__.

This got me thinking about Scala's [andThen](https://github.com/scala/scala/blob/v2.11.1/src/library/scala/Function1.scala) function which is the opposite of compose:

```{.scala}
trait Function1[-T1, +R] extends AnyRef { self =>
  ...
  def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }
}
```

In Haskell that would be something like:

```{.haskell}
andThen :: (a -> b) -> (b -> c) -> a -> c
andThen f g x = g $ f x
```
What I needed was something that could recreate the 7 input parameters needed for the __TagInfo__ constructor. As a first attempt I created __andThen7__:

```{.haskell .scrollx}
andThen7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8) -> (a8 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
andThen7 f g = \x1 x2 x3 x4 x5 x6 x7 -> g $ f x1 x2 x3 x4 x5 x6 x7
```

Now I could do this:

```{.haskell}
:t (TagInfo `andThen7` showTag)
(TagInfo `andThen7` showTag)
  :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
```

Now the type signature produced in the above matches that required by the __renderSingleLink__ function
to __renderTagCloudWith__.

This is obviously a pretty bad solution. I asked around for a better solution from guys in the [BFPG](http://www.meetup.com/Brisbane-Functional-Programming-Group) and [Mark Hibberd](https://twitter.com/markhibberd) came up with _a nested compose_ as a possible solution (1):

```{.haskell}
((((((showTag .) .) . ) . ) . ) . ) . TagInfo
```

Another solution proposed by [Nick Patridge](https://twitter.com/nkpart) was to use fmap (2):

```{.haskell}
(fmap . fmap . fmap . fmap . fmap . fmap . fmap) showTag TagInfo
```

Solution (2) seems like a very nice solution. The type signature of composing __fmap__ is pretty cool and seems to be built for mapping a function into a nested structure:

```{.haskell}
:t (fmap . fmap . fmap . fmap . fmap . fmap . fmap)
(fmap . fmap . fmap . fmap . fmap . fmap . fmap)
  :: (Functor f, Functor f1, Functor f2, Functor f3, Functor f4,
      Functor f5, Functor f6) =>
     (a -> b)
     -> f (f1 (f2 (f3 (f4 (f5 (f6 a))))))
     -> f (f1 (f2 (f3 (f4 (f5 (f6 b))))))
```

I wonder if there is still a better solution? Any thoughts or comments are welcome.





























