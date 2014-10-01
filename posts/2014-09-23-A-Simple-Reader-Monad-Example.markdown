---
title: A Simple Reader Monad Example
description: If you are looking for a simple Reader Monad example, then look no further.
author: sanjiv sahayam
tags: haskell
---

What is a Reader Monad?

The [Reader Monad](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html) works within the context of a shared environment. But what does that mean? Say you needed some shared object to execute a bunch of functions. An example could be that you need a database connection in every query function you execute. Or it could be some configuration options read from a file that are needed across a number of functions.

When trying to learn about the Reader Monad I've found most examples are convoluted or unnecessarily long. I hope to change that by providing a simple example that you can try out without too much head-spinning.

The Reader Monad is defined as:

```{.haskell}
type Reader r = ReaderT r Identity
```

One of the time-consuming things about learning the Reader Monad is that it is defined in terms of the ReaderT transformer (which is also a Monad). So now you have to learn multiple monads just to understand the Reader Monad. Annoying.

Let's ignore the ReaderT transformer for now and assume that Reader is defined as:

```{.haskell}
Reader r a
```

where r is some "environment" and a is some value you create from that environment. And thanks to the type alias
above you can just about do that.

Because Reader is a Monad we can do stuff like this:

```{.haskell .scrollx}
import Control.Monad.Reader

let r1 = return 5 :: Reader String Int
```

We have created a simple Reader using the Monad's __return__ function.

If we check the type of r1:

```{.haskell}
:t r1
r1 :: Reader String Int
```

We see that we have created a Reader that takes in a String and returns an Int. The String is the "environment" of the Reader. So how can we get the Int value out of the reader? By running it of course! We can use the __runReader__ function to do that:

```{.haskell}
(runReader r1) "this is your environment"
5
```

__runReader__ is defined as:

```{.haskell}
runReader :: Reader r a -> r -> a
```

So __runReader__ takes in a Reader and an environment (__r__) and returns a value (__a__).

_Now notice that we didn't really do anything with the environment supplied to us._

What if we had a bunch of Readers and we wanted to __bind__ across them?

```{.haskell .scrollx}
import Control.Monad.Reader

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"
```

The ask function is defined on [MonadReader](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#t:MonadReader).

```{.haskell .scrollx}
class Monad m => MonadReader r m | m -> r where
```

Let's ignore MonadReader for now and focus on the definition of the ask function:

```{.haskell}
ask :: m r
```

Basically the above gives you a Reader Monad with the environment in it. So if you need access to the environment you ask for it. :)

In the __tom__, __jerry__ and __tomAndJerry__ functions, we are working within the context of the Reader Monad. That allows us to __bind__ to the environment within the Reader. It also means that we need to __return__ all values within a new Reader as well.

The __tomAndJerry__ function binds to values from each Reader and then returns them combined in another Reader. We then run the whole lot in the __runJerryRun__ function with the help of __runReader__ and get the following output:

```{.haskell}
Who is this? This is Tom.
Who is this? This is Jerry.
```

I hope this simple example is useful in getting you started in using and thinking about the Reader Monad.