---
title: Following a Paginated API with Haskell and UnfoldrM
author: sanjiv sahayam
description: How to use a paginated API through Haskell with unfoldrM.
tags: haskell
comments: true
---

I've been using the [Twitter API](https://developer.twitter.com/en.html) for [Milo](https://github.com/ssanj/milo) - 
a little command line application that limits my interaction with Twitter, to a select few specific accounts and searches. 
As with most APIs that have a large number of results, the Twitter API is paginated and you need to keep supplying it 
with some manner of cursor when going from one page of results to the next. 

For the purposes of Milo, I wanted to access the latest direct messages for my user. Unfortunately the call to
get your direct messages may return multiple pages of "empty" results and a cursor to the next page of results. I wanted
to navigate these empty results and grab the first page with any results that were not empty - essentially just the
latest direct message(s).

An example of an empty result:

```{.json .scrollx}
{
  "next_cursor": "some_hash",
  "events": []
}
```

An example of a non-empty result:

```{.json .scrollx}
{
  "next_cursor": "some_hash",
  "events": [
    { "id": "110", "created_timestamp": "5300", ... },
    { "id": "109", "created_timestamp": "5200", ... },
    { "id": "108", "created_timestamp": "5200", ... },
    { "id": "107", "created_timestamp": "5200", ... },
    { "id": "106", "created_timestamp": "5100", ... },
    { "id": "105", "created_timestamp": "5100", ... },
    ...
  ]
}
```

As we can see, the resulting Json payload has both the result (contents of the `events` field) and the cursor 
(`next_cursor`).

_Please note that I use [Aeson](https://www.stackage.org/lts-14.20/package/aeson-1.4.6.0) to convert the Json payload into the `DirectMessages` data type used in the example, but
I have omitted the bindings for clarity. The full Milo source can be found on Github._

I initially came up with this solution:

```{.haskell .scrollx}
import qualified Data.Text as T

-- Wrapper around the hash return by the Twitter API
newtype Cursor = Cursor { unCursor :: T.Text } deriving Show

-- Data type to hold the list of direct messages and the cursor (if any)
-- Each DirectMessage maps to a single element in the `events` array
data DirectMessages = DirectMessages { messages :: [DirectMessage], cursorPosition :: Maybe T.Text } deriving Show

-- Function that returns the direct messages or an error
getDirectMessages :: IO (Either String DirectMessages)
getDirectMessages = getMoreDirectMessages Nothing

-- Function that loops through the result pages using a cursor
getMoreDirectMessages :: Maybe Cursor -> IO (Either String DirectMessages)
getMoreDirectMessages nextCursor = do
  dmsE <- callTwitterApi nextCursor
  case dmsE of
    Right dms@(DirectMessages messageList (Just nextCursor')) -> 
      if (null messageList) then -- if the messages are empty try to get more
        do
          (fmap (combineDms dms)) <$> (getMoreDirectMessages (Just . Cursor $ nextCursor'))
      else pure . Right $ dms

    Right dms@(DirectMessages _ Nothing) -> pure (Right dms) -- No more cursors so just stop
    Left dmError -> pure . Left $ dmError


-- Function that collates direct messages
combineDms :: DirectMessages -> DirectMessages -> DirectMessages

-- Function that calls the Twitter API with the cursor (if any)
callTwitterApi :: Maybe Cursor -> IO (Either String DirectMessages)
```

Now while this works it has a few problems:

1. It does not look very reusable, which it should be because pagination is a common problem
1. There are dangling error cases where we just lift the error into some outer context

At this point I recalled seeing a function called `unfold` somewhere that produced values until some exit condition was reached. I decided to track it down. I found [unfoldr](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#g:9)
which sort of fitted what I needed.


## unfoldr

```{.haskell .scrollx}
unfoldr :: (a -> Maybe (b, a)) -> a -> [b]
```

From the docs:

> The unfoldr function is a `dual' to foldr: while foldr reduces a list to a summary value, unfoldr builds a list from a seed value. The function takes the element and returns Nothing if it is done producing the list or returns Just (b,a), in which case, b is a prepended to the list and a is used as the next element in a recursive call.


This sounded promising. I needed to keep "producing" direct message results and stop as soon as I got some results that 
were not empty. Unfortunately I needed to work within an effect (`IO`) which this function did not support.

In any event let's try and understand how this function works. This is the implementation of the the `unfoldr` function:

```{.haskell .scrollx}
unfoldr      :: (a -> Maybe (b, a)) -> a -> [b]
unfoldr f a  =
  case f a of
   Just (b,new_a) -> b : unfoldr f new_a
   Nothing        -> []
```

Given some generator function `f` that takes a value of type `a`, call `f` with `a` which returns a `Maybe` with a pair of
values consisting of a result `b` and the next value of `a` to feed into the same function. The `Maybe` is either a `Just` value
with a new result `b` and the next value of `a`: `new_a`. In this case the result `b` is prepended to a list of results which
will be generated by recursively calling the `unfoldr` function with `f` and `new_a`. In the `Nothing` case
return an empty List.

Here's a simple example that produces numbers from 1 to 10 and then stops:


```{.haskell .scrollx}
import Data.List (unfoldr)

unfoldr (\a -> if a < 11 then Just (a, a + 1) else Nothing) 1
> [1,2,3,4,5,6,7,8,9,10]
```

Pretty neat but not what I exactly needed.

After some more digging around I stumbled across a library called [monad-loops](http://hackage.haskell.org/package/monad-loops)
which had what I was after.


## unfoldrM


```{.haskell .scrollx}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
```

We can see from its function definition that it's exactly the same as `unfoldr` except the intermediate and final results
are within some `Monad m`:

```{.haskell .scrollx}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldr  ::            (a ->    Maybe (b, a))  -> a ->   [b]
```

This is the implementation of the `unfoldrM` function (which is an alias to `unfoldrM'`):

```{.haskell .scrollx}
unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f = go
    where go a = do
            x <- f a
            case x of
                Nothing         -> return mzero
                Just (b, new_a)    -> do
                        rest <- go new_a
                        return (return b `mplus` rest)
```

The implementation is very similar to `unfoldr` with differences due to the selected effect type `m` and result type `f`.

Given some generator function `f` that takes a value of type `a`, it calls `f` with `a` within a `do` block. It returns a `Maybe` with a
pair of values; the result `b` and the next value of type `a`: `new_a`, within a context `m`. It extracts and pattern matches on the contextual result. 
If it's a `Nothing` it returns the default value for the [MonadPlus](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html#t:MonadPlus) type `f` (`mzero`). 
If the result is a `Just`, it creates a nested `do` block and recurses with the `new_a` value to extract the final result `rest`. It then
combines the `rest` with the previous result `b` according to the `mplus` implementation for the `MonadPlus` type `f` and 
returns the results in the context `m`. `unfoldrM` is a specialized version of `unfoldrM'` where `f` is a `[]`.


## The solution

Now while this seemed to be what I needed it took a little while for me to understand how to use it in my use case. One
thing that stumped me was why the initial value `a` was not a `Maybe`. Surely the first time I called the Twitter API,
I would not have a cursor, so how could I represent it as an `a`?  Even if I did make `a` a `Maybe a`, how would I 
distinguish between the initial call where I had no cursor and the final call where I would also have no cursor?!

My friend [Adam](http://twitter.com/ajfitzpatrick) stated the obvious so I could understand it:

> Maybe does not satisfy your requirements because you need more than two states

Oh dear! Was I supposed to create some ADT with three states? I thought this was supposed to be some plug-and-play solution
and it was turning out not to be.

I started off by creating the ADT for the states:

```{.haskell .scrollx}
-- | An ADT to capture the three states of a cursor:
data CursorState a 
    = NewCursor           -- NewCursor - Initial cursor state
    | GoCursor (Cursor a) -- GoCursor - A state of having a cursor, typically used for iteration
    | StopCursor          -- StopCursor - The final cursor state
```

Now if I plug in my types into the `unfoldrM` function I get the following:

```{.haskell .scrollx}
unfoldrM (a -> m (Maybe (b, a))) -> a -> m [b]
-- 'a' is CursorState c
-- 'c' is the type of cursor data
unfoldrM :: (CursorState c -> m (Maybe (b, CursorState c))) -> CursorState c -> m [b]
-- 'm' is IO
unfoldrM :: (CursorState -> IO (Maybe (b, CursorState c ))) -> CursorState c -> IO [b]
-- 'b' is DirectMessages
unfoldrM :: (CursorState c -> IO (Maybe (DirectMessages, CursorState c))) -> CursorState c -> IO [DirectMessages]
```

Now this seems to make sense.

Given that I already had a function of type:

```{.haskell .scrollx}
callTwitterApi :: Maybe Cursor -> IO (Either String DirectMessages)
```

How could I convert it to work with the above function definition?

I could define a function `unfoldWith` as:


```{.haskell .scrollx}
unfoldWith :: forall m b c. CursorState c ->  m (Maybe (b, CursorState c))
unfoldWith NewCursor = undefined
unfoldWith (GoCursor (Cursor nextCursor)) = undefined
unfoldWith StopCursor = undefined
```

The simplest one to define is the `StopCursor` variant:

```{.haskell .scrollx}
unfoldWith :: forall m b c. Applicative m => CursorState c ->  m (Maybe (b, CursorState c))
unfoldWith NewCursor = undefined
unfoldWith (GoCursor (Cursor nextCursor)) = undefined
unfoldWith StopCursor = pure Nothing
```

and that compiles!

Next I can tried to implement the `NewCursor` variant:

```{.haskell .scrollx}
unfoldWith :: forall m b c. Applicative m => CursorState c ->  m (Maybe (b, CursorState c))
unfoldWith NewCursor = undefined -- I need to be able to use callTwitterApi here
unfoldWith (GoCursor (Cursor nextCursor)) = undefined
unfoldWith StopCursor = pure Nothing
```

generalising the `callTwitterApi` function:


```{.haskell .scrollx}
callTwitterApi :: Maybe (Cursor c) -> IO (Either String DirectMessages)
-- Since we need to reduce our Monad to an m, wrap the IO (Either String) in ExceptT
-- ExceptT String IO is 'm' (Essentially a wrapped (IO Either String))
-- DirectMessages is 'a'
callTwitterApi :: Maybe (Cursor c) -> ExceptT String IO DirectMessages
-- which simplifies to:
callTwitterApi :: Maybe (Cursor c) -> m DirectMessages

-- Now we should be able to define any API that gets some `a` as:
someApi :: Maybe (Cursor c) -> m a

-- passing in someApi to unfoldWith
unfoldWith :: forall m a b c. Applicative m => (Maybe (Cursor c) -> m a) -> CursorState c ->  m (Maybe (b, CursorState c))
unfoldWith f NewCursor = f Nothing -- call it with Nothing because we don't have a Cursor
unfoldWith f (GoCursor (Cursor nextCursor)) = undefined
unfoldWith _ StopCursor = pure Nothing
```

So far so good. But now we need to extract the result and the next cursor from response of the api call. When we call 
`someApi` we get a `m a` in return:

```{.haskell .scrollx}
someApi :: Maybe (Cursor c) -> m a
```

_Note: To add type annotation to let expressions you need to enable the `ScopedTypeVariables` language extension_:

```{.haskell .scrollx}
{-# LANGUAGE ScopedTypeVariables #-}
```

We need a function that transforms that `a` into a pair of `(b, CursorState c)`:

```{.haskell .scrollx}
extractPayload :: a -> (b, CusorState c)
```

passing that into our `unfoldWith` function:

```{.haskell .scrollx}
unfoldWith :: forall m a b c. Applicative m => (a -> (b, CursorState c)) -> (Maybe (Cursor c) -> m a) -> CursorState c ->  m (Maybe (b, CursorState c))
unfoldWith extractPayload callApiWith NewCursor = 
  let resultM :: m a = callApiWith Nothing
  in  Just . extractPayload <$> resultM
unfoldWith extract callApiWith (GoCursor (Cursor nextCursor)) = undefined
unfoldWith _ _ StopCursor = pure Nothing
```

Seems to compile. Now we do the same of the `GoCursor` case:

```{.haskell .scrollx}
unfoldWith :: forall m a b c. Applicative m => (a -> (b, CursorState c)) -> (Maybe (Cursor c) -> m a) -> CursorState ->  m (Maybe (b, CursorState))
unfoldWith extractPayload callApiWith NewCursor = 
  let resultM :: m a = callApiWith Nothing
  in  Just . extractPayload <$> resultM
unfoldWith extract callApiWith (GoCursor (Cursor nextCursor)) = 
  let resultM :: m a = callApiWith (Just nextCursor)
  in  Just . extractPayload <$> resultM
unfoldWith _ _ StopCursor = pure Nothing
```

A `DirectMessages` is defined as:

```{.haskell .scrollx}
data DirectMessages = DirectMessages { messages :: [DirectMessage], cursorPosition :: Maybe T.Text } deriving Show
```

And now I just define a function that takes in a `DirectMessages` type and returns a pair of `([DirectMessage], CursorState T.Text)`:

```{.haskell .scrollx}
extractState :: DirectMessages -> ([DirectMessage], CursorState T.Text)
extractState (DirectMessages [] (Just c)) = ([], GoCursor (Cursor c)) -- No messages, but we have a cursor, then try to get more
extractState (DirectMessages [] Nothing)  = ([], StopCursor)          -- No messages and no cursor, then stop
extractState (DirectMessages msgs _)  = (msgs, StopCursor)            -- Messages so we can stop irrespective of the cursor
```

Now I can call `unfoldrM` with:


```{.haskell .scrollx}
import qualified Control.Monad.Except as Ex

callTwitterApi :: Ex.ExceptT String IO DirectMessages

getDirectMessages :: IO (Either String DirectMessages)
getDirectMessages = Ex.runExceptT $ unfoldrM (unfoldWith extractState callTwitterApi) NewCursor
```

and we have pagination!

_Note how we had to unwrap the [ExceptT](https://www.stackage.org/haddock/lts-14.20/mtl-2.2.2/Control-Monad-Except.html#t:ExceptT) 
with `Ex.runExceptT` to retrieve the wrapped `IO (Either String DirectMessages)`_.

The interesting point is that we now have a reusable function `unfoldWith` which we can use with
any paginated API that returns us a payload with a result and a cursor.

## A Simpler Example


If you got a little lost in the details of the above example, don't worry. Here's a simpler example to give you some
intuition.

```{.haskell .scrollx}
-- Payload type
data Packet = Packet { value :: String, cursor :: Maybe (Cursor Int) }

-- Function that mimicks a server response
serviceCall :: forall m . Applicative m => Maybe (Cursor Int) -> m Packet
serviceCall Nothing = pure $ Packet "packet one" (Just $ Cursor 1)
serviceCall (Just (Cursor cur))
  | cur == 1  = pure $ Packet "packet two"   (Just $ Cursor 2)
  | cur == 2  = pure $ Packet "packet three" (Just $ Cursor 3)
  | cur == 3  = pure $ Packet "packet four"  (Just $ Cursor 4)
  | otherwise = pure $ Packet "packet five"  Nothing

-- Function that splits the payload into a result and the next CursorState
extractState :: Packet -> (String, CursorState Int)
extractState (Packet v (Just c)) = (v, GoCursor c)
extractState (Packet v Nothing)  = (v, StopCursor)
```

As before we can use it with:

```{.haskell .scrollx}
unfoldrM (unfoldWith extractState serviceCall) NewCursor
```

Using the above to log out the first three page responses:

```{.haskell .scrollx}
import Control.Monad.Loops (unfoldrM)
import Data.List (intercalate)

run :: IO ()
run = 
  let resultsIO :: IO [String] = unfoldrM (unfoldWith extractState serviceCall) NewCursor
      stringyfied :: IO String = (intercalate "," . take 3) <$> resultsIO
  in stringyfied >>= putStrLn
```

which prints out:

```{.haskell .scrollx}
packet one,packet two,packet three
```

I'm not sure if this is a "pattern" that people generally use but I can see myself using this for other paginated APIs.

The code for the [Simpler Example](https://github.com/ssanj/unfoldExample).