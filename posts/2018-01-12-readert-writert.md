---
title: Using ReaderTWriterT in Haskell
author: sanjiv sahayam
description: ???
tags: haskell, monad-transformer
comments: true
---

Stacking Monads can be somewhat confusing to get your head around. While looking around for a decent example, I came across this [Gist](https://gist.github.com/Decoherence/39a4c34685d86a334b63) by
[Decoherence](https://github.com/Decoherence) on how to combine a [ReaderT](http://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control-Monad-Trans-Reader.html#ReaderT) with a [WriterT](http://hackage.haskell.org/package/transformers-0.5.5.0/docs/src/Control-Monad-Trans-Writer-Lazy.html#WriterT) over some Monad.

I needed to use this stack as I was working with the IO Monad and needed some way to capture the outcomes of a computation (via a Writer) and also needed to supply the initial inputs (via a Reader).

While Reader and Writer on their own seem easy to use, it can be somewhat daunting to try and figure out how to combine the transformer variations of these Monads over some other Monad.

> Say Monad one more time...


## Type signatures

Let's start by looking at the type signature for a ReaderT Monad Transformer:

```{.haskell .scrollx}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

The type variables are as follows:

* r = resource
* m = Monad
* a = result

We can see from the [newtype](https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell) which defines ReaderT that, it encapsulates the type:

```{.haskell .scrollx}
r -> m a
```

So everytime we see a __ReaderT r m a__ we can mentally substitute it with a simple function of the type:

```{.haskell .scrollx}
r -> m a
```

A ReaderT Monad Transformer is very similar to a Reader Monad:

```{.haskell .scrollx}
r -> a
```

in that it requires some input from the environment _r_ before it returns the result _a_. The only difference being that ReaderT returns that _a_ in a Monad _m_.


Given a __ReaderT r m a__ we can unwrap its value via the _runReaderT_ method:

```{.haskell .scrollx}
runReaderT :: ReaderT r m a -> r -> m a
```

Also given a simple Reader Monad `r -> a` we can lift it into a __ReaderT__ Monad Transformer with the _reader_ or the _asks_ function:

```{.haskell .scrollx}
reader,asks :: Monad m => (r -> a) -> ReaderT r m a
```

Also note that __ReaderT r m__ is a Monad:

```{.haskell .scrollx}
Monad m => Monad (ReaderT r m)
```

 along with __m__ which is also a Monad on its own. This is important to note when using _do_ notation, with __ReaderT__; each value will be wrapped in __ReaderT r m__ and not __ReaderT__:

```{.haskell .scrollx}
someFunc :: ReaderT r m a
someFunc = do
    r <- ask
    return a -- this will be returned into ReaderT r m
```

If you need to wrap a value within a  __ReaderT__ use:

```{.haskell .scrollx}
ReaderT \r -> ma -- your value of (m a)
```

This might all seem very confusing at the moment. These are different ways of lifting values into the transformer stack at different points.

Some other useful methods on __ReaderT__ are:

* _ask_ - to retrieve the supplied resource

```{.haskell .scrollx}
ask :: Monad m => ReaderT r m r
```

* _local_ - to map a function across the resource _before_ using it:

```{.haskell .scrollx}
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
```


## A ReaderT example

Given the above types and functions, let's have a look at an example of using a ReaderT transformer stack.

Say we had some configuration about an external service, like the _host_ and _port_ the service is running on. How could we use a Reader to supply that config to a simple program?

Let's start by defining a _Config_ type as a Map of String keys and values:

```{.haskell .scrollx}
import qualified Data.Map.Lazy as M

type Config = M.Map String String
```

Let's also define a _serverConfig_ function to return our populated config from a list of key-value pairs:

```{.haskell .scrollx}
serverConfig :: Config
serverConfig = M.fromList [("host", "localhost"), ("port", "7654")]
```

Let's use a Reader to read the host:

```{.haskell .scrollx}
getHost :: Reader Config (Maybe String)
getHost = do
  config <- ask
  return (Map.lookup "host" config)
```

First, the _getHost_ function requests the Config instance from the environment using the _ask_ function. It then looks up the "host" key from that config. Finally it lifts the Maybe value returned from the _lookup_ function into the Reader Monad using the _return_ function.

We also use a __Reader__ to read the port:

```{.haskell .scrollx}
getPort :: Reader Config (Maybe Int)
getPort = do
  config <- ask
  return (Map.lookup "port" config >>= readMaybe)
```

This function is similar to _getHost_ with the additional bind (>>=) operation to join together the value read from _lookup_ with _readMaybe_. readMaybe is defined in `Text.Read` as:

```{.haskell .scrollx}
readMaybe :: Read a => String -> Maybe a
```

and tries to read in a value supplied as String into a target type _a_, where if it successfully reads in the value it returns a (_Just a_) or if it fails it returns a _Nothing_.

If we follow the types for _getHost_ we see that it is defined as:

```{.haskell .scrollx}
Reader Config (Maybe String)
```

Given a Config type we may return a String with the host value.

_getPort_ is defined as:

```{.haskell .scrollx}
Reader Config (Maybe Int)
```

Given a Config type, we may return a Int with the port value.

Also notice that we used a __Reader__ as opposed to a __ReaderT__ and that might be an a little confusing. So why didn't we use a __ReaderT__ directly to read this config? We could have but a __ReaderT__ requires an inner __Monad m__:

```{.haskell .scrollx}
ReaderT r m a
```

and we haven't decided on what __m__ is at the moment. To keep things simple I've used a __Reader__. I'll demonstrate how we could have directly used a __ReaderT__ Monad Transformer to implement _getHost_ and _getPort_ later on.


Now that we've written functions to read the host and port, lets go ahead and use those values in a __ReaderT__ along with a __WriterT__ to log out the values we received from the config:

```{.haskell .scrollx}
getConfig :: ReaderT Config (WriterT String IO) ()
getConfig = do
  hostM <- fromReader getHost
  portM <- fromReader getPort
  let host = maybe "-" id hostM
      port = maybe "-" show portM
  _ <- log "\nConfig"
  _ <- log "\n======"
  _ <- log (printf "\nhost: %s" host)
  _ <- log (printf "\nport: %s" port)
  return ()
```

Lets start with the type definition of _getConfig_:

```{.haskell .scrollx}
getConfig :: ReaderT Config (WriterT String IO) ()
```

This says that when given a Config type as an input, the result returned will be in a __WriterT__ with a log type of String with an inner Monad of __IO__ and a value of unit ().

We've not looked at the definitions of __Writer__ and __Writer__ so let's do that now. Writer is defined as:

```{.haskell .scrollx}
(a, w)
```

The type variables are:

* a = return value
* w = log value (which has to be an Monoid)

WriterT is defined as:

```{.haskell .scrollx}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
```

where the type variables are:

* a = return value
* m = Monad
* w = log value (which has to be an Monoid)

and it encapsulates:

```{.haskell .scrollx}
m (a, w)
```

So it's basically a Writer within a Monad m.

Let's delve into the implementation of _getConfig_:

```{.haskell .scrollx}
  hostM <- fromReader getHost
  portM <- fromReader getPort
```

The above lines read the host and port into Maybe values from the configuration.

The following lines covert the Maybe values for host and port into their String equivalents:

```{.haskell .scrollx}
  let host = maybe "-" id hostM
      port = maybe "-" show portM
```

The following lines simply write String values to the log in order:

```{.haskell .scrollx}
  _ <- log "\nConfig"
  _ <- log "\n======"
  _ <- log (printf "\nhost: %s" host)
  _ <- log (printf "\nport: %s" port)
```

and the final return:

```{.haskell .scrollx}
return ()
```

lifts a unit value (), into the ReaderT (WriterT String IO) Monad. That seems quite easy to understand and closely follows an imperative program that would have the same functionality (without side effects though!)

Let's look at the helper function _fromReader_:

```{.haskell .scrollx}
fromReader :: Monad m => ReaderT r Identity a -> ReaderT r m a
fromReader = mapReaderT (return . runIdentity)
```

The type definition:

```{.haskell .scrollx}
fromReader :: Monad m => ReaderT r Identity a -> ReaderT r m a
```

tells us that we are mapping from one type of ReaderT to another:

```{.haskell .scrollx}
ReaderT r Identity a ->
ReaderT r m        a
```

Basically we want to map the contents of the Identity Monad to another Monad m. So why do we need to do this at all? Where did Identity come from?

This comes from the fact that the __Reader__ Monad is defined in terms of a __ReaderT__ Monad Transformer:

```{.haskell .scrollx}
type Reader r = ReaderT r Identity
```

So even when we used the __Reader__ Monad in _getHost_ and _getPort_ we were actually using the __ReaderT__ Monad Transformer!

So what is Identity?

```{.haskell .scrollx}
newtype Identity a
```

We can deduce from the above definition that Identity is just a wrapper around any type _a_.

From the docs:

> The identity functor and monad.

> This trivial type constructor serves two purposes:

> * It can be used with functions parameterized by functor or monad classes.
> * It can be used as a base monad to which a series of monad transformers may be applied to construct a composite monad. Most monad transformer modules include the special case of applying the transformer to Identity. For example, State s is an abbreviation for StateT s Identity.

What's interesting is that there are Monad, Applicative, Functor (and multiple other) type class instances for Identity and that gives us the power to use it within any Monad Transformer stack that requires an inner Monad. The Identity Monad does not perform any effects and simply wraps a values. This can be seen as a "empty" Monad that does nothing.

If we revisit the type of the _getHost_ function:

```{.haskell .scrollx}
Reader Config (Maybe String) -- r -> a
       r       a
```

we can expand it to use the full __ReaderT__ definition as:

```{.haskell .scrollx}
ReaderT Config Identity (Maybe String) -- r -> m a
        r      m         a
```

In order to retrieve a value from the Identity type we need use the _runIdentity_ function:

```{.haskell .scrollx}
runIdentity :: Identity a -> a
```

It should now be clear as to why we need to transform inner Monads on our _Reader_ instances:

```{.haskell .scrollx}
fromReader ::
  ReaderT Config Identity a ->  -- this is the Reader
  ReaderT Config m        a     -- this is the ReaderT
```

In order to perform this mapping we use the _mapReaderT_ function which is defined as:

```{.haskell .scrollx}
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
```

We can see from the type definition for _mapReaderT_ that the inner Monad of a __ReaderT__ Monad Transformer (m a) is converted to another type (n b).

In our case we want to convert the inner Monad from:

```{.haskell .scrollx}
Identity (Maybe a)
```

to

```{.haskell .scrollx}
m (Maybe a)
```

The implementation of _fromReader_ allows us to do just this:

```{.haskell .scrollx}
fromReader :: Monad m => ReaderT r Identity a -> ReaderT r m a
fromReader = mapReaderT (return . runIdentity)
```

When given a _m a_, which in this case is _Identity a_, we run it with _runIdentity_ to get the value of _a_ and we then lift that _a_ into the required Monad m, using the _return_ function.

This seems like unnecessary work and ideally there should be an inbuilt function that does this for us.

//TODO: Can we avoid having this at all?

Next let's have a look at the _log_ function:

```{.haskell .scrollx}
log :: (Monad m, MonadTrans t, Monoid w) => w -> t (WriterT w m) ()
log = lift . tell
```

From the type definition:

```{.haskell .scrollx}
w -> t (WriterT w m) ()
```

we can see that we are lifing some log __w__ into a transformer stack __t (WriterT w m)__.

_tell_ is defined as:

```{.haskell .scrollx}
tell :: Monad m => w -> WriterT w m ()
```

and _lift_ is defined as:

```{.haskell .scrollx}
lift :: Monad m => m a -> t m a
```

We've come a long way and we've got everything setup as needed. The only thing left to do is run the transformer stack and reap our rewards. We can do that with the _readWriteConfig_ function:

```{.haskell .scrollx}
readWriteConfig :: IO ()
readWriteConfig = execWriterT (runReaderT getConfig serverConfig) >>= putStrLn
```

Let's start with the definition of the _runReaderT_ function:

```{.haskell .scrollx}
runReaderT :: ReaderT r m a -> r -> m a
```

When given a __ReaderT__ Monad Transformer and the resource, the above function returns the inner Monad.

and _execWriterT_:

```{.haskell .scrollx}
execWriterT :: Monad m => WriterT w m a -> m w
```

which basically returns the log __w__ in the Monad __m__.

When running the stack, it is run from outside-in. So given a _ReaderT (WriterT String m) a_,
we:

1. Run the ReaderT with runReaderT. This returns the inner Monad m: WriterT String m a
1. Run the WriterT with execWriterT. This returns the log in the inner Monad m: m w

Substituting the actual Monads in the above:

1. Run the ReaderT with runReaderT. This returns WriterT String IO ()
1. Run the WriterT with execWriterT. This returns IO String

//TODO: Explain this better


https://stackoverflow.com/questions/43840588/use-readert-maybe-or-maybet-reader#43840589

## A tale of at least two Monads

Each Monad Transformer is composed of at least two Monads. If we take ReaderT as an example, we have its definition as:

```{.haskell .scrollx}
ReaderT r m a
```

where __ReaderT r m__ is a Monad and __m__ is a Monad. If you stack Monads, in the __m__ type variable as with a WriterT for example:

```{.haskell .scrollx}
ReaderT r (WriterT w m) a
```

then __ReaderT r (WriterT w m)__ is a Monad, __WriterT w m__ is a Monad and __m__ is a Monad.

---------
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
WriterT w m a
w - log
m - monad
a - value
encapsulates: m (w, a)


runWriterT :: WriterT w m a -> m (a, w)
-- other methods
execWriterT :: Monad m => WriterT w m a -> m w
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
tell :: Monad m => w -> WriterT w m ()
listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
pass :: Monad m => WriterT w m (a, w -> w) -> WriterT w m a

mapM      :: Monad m      => (a -> m b) -> t a -> m (t b)
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

sequenceA :: Applicative f => t (f a) -> f (t a)
sequence   :: Monad m      => t (m a) -> m (t a)

