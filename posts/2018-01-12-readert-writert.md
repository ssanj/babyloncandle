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

While Reader and Writer Monads on their own seem easy to use, it can be somewhat daunting to try and figure out how to combine the transformer variations of these Monads over some other Monad.

![Say Monad one more time](https://scalerablog.files.wordpress.com/2015/10/bdu68sacyaafkkr.jpg)

# Reader and ReaderT

Let's start by looking at the type signature for the Reader Monad:

```{.haskell .scrollx}
r -> a
```

The Reader Monad when given some resource, _r_ from the environment will return a result of _a_.

The type variables defined are as follows:

* r = resource from the environment
* a = value returned

Next lets have a look at the type signature for a ReaderT Monad Transformer (MT):

```{.haskell .scrollx}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

That seems a lot less clear than the definition of the Reader Monad. As we'll see below they are essentially very similar.

The type variables defined are as follows:

* r = resource from the environment
* m = the resulting Monad
* a = value returned in the Monad

The ReaderT MT has one extra type variable __m__ which is a Monad. If we examine the ReaderT constructor we can see that it encapsulates a type very similar to that of the Reader Monad:

```{.haskell .scrollx}
r -> m a -- ReaderT MT
r ->   a -- Reader Monad
```

The ReaderT MT is simply a Reader Monad whose result is returned within another Monad. More on that later. Hopefully the connection between the Reader Monad and the Reader MT is getting clearer.

When we see a ReaderT r m a we can mentally substitute it with a function of the type:

```{.haskell .scrollx}
r -> m a
```

Given a ReaderT r m a we can unwrap its value via the _runReaderT_ method:

```{.haskell .scrollx}
runReaderT :: ReaderT r m a -> r -> m a
```

Also given a simple Reader Monad (`r -> a`) we can lift it into a ReaderT MT with the _reader_ or the _asks_ function:

```{.haskell .scrollx}
reader,asks :: Monad m => (r -> a) -> ReaderT r m a
```

Also note that ReaderT r m is a Monad if __m__ is a Monad:

```{.haskell .scrollx}
Monad m => Monad (ReaderT r m)
```

 This is important to know when using _do_ notation with the ReaderT MT as each bind operation will result in a ReaderT r m and not a ReaderT:

```{.haskell .scrollx}
someFunc :: ReaderT r m a
someFunc = do
    r <- ask
    return a -- this will be returned into ReaderT r m
```

If you need to wrap a value within a ReaderT MT use:

```{.haskell .scrollx}
ReaderT \r -> -- your value of (m a)
```

This might all seem very confusing at the moment. These are different ways of lifting values into the transformer stack at different points. Once you start using the ReaderT MT this will become clearer.

Some other useful functions that work with the ReaderT MT are:

* _ask_ - to retrieve the supplied resource

```{.haskell .scrollx}
ask :: Monad m => ReaderT r m r
```

* _local_ - to map a function on the resource _before_ using it:

```{.haskell .scrollx}
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
```

* _mapReaderT_ - to change all components of the ReaderT MT except the input type (the inner Monad and result type):

```{.haskell .scrollx}
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
```

# Writer and WriterT

As we've not looked at the definitions of the Writer Monad and the WriterT MT let's do that now. The Writer Monad is defined as:

```{.haskell .scrollx}
(a, w)
```

The type variables defined are:

* a = return value
* w = log value (which has to be an Monoid)

The Writer Monad will return a pair of values; a result __a__ along with an accumulated log __w__.

The WriterT MT is defined as:

```{.haskell .scrollx}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
```

where the type variables defined are:

* a = return value
* m = the resulting Monad
* w = log value (which has to be an Monoid)

Both __a__ and __w__ are returned as a pair within the Monad __m__.

The WriterT constructor encapsulates:

```{.haskell .scrollx}
m (a, w)
```

It's basically a Writer Monad within another Monad m:

```{.haskell .scrollx}
  (a, w) -- Writer Monad
m (a, w) -- WriterT MT
```

Some other useful functions that work with the WriterT MT are:

* _runWriterT_ - to unwrap the value of a WriterT MT and return the result and the log:

```{.haskell .scrollx}
runWriterT :: WriterT w m a -> m (a, w)
```

* _execWriterT_ - to unwrap the value of a WriterT MT and return only the log:

```{.haskell .scrollx}
execWriterT :: Monad m => WriterT w m a -> m w
```

* _tell_ - to write a log entry into the WriterT MT:

```{.haskell .scrollx}
tell :: Monad m => w -> WriterT w m ()
```

* _listen_ - to change the result to include the log:

```{.haskell .scrollx}
listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
```

* _pass_ - to run a function on the log to update it:

```{.haskell .scrollx}
pass :: Monad m => WriterT w m (a, w -> w) -> WriterT w m a
```

* _mapWriterT_ - to change all components of the WriterT MT (the inner Monad, result and log type):

```{.haskell .scrollx}
mapWriterT :: (m (a, w) -> n (b, w’)) -> WriterT w m a -> WriterT w’ n b
```

# MonadTrans

The MonadTrans typeclass defines one function called _lift_:

```{.haskell .scrollx}
lift :: Monad m => m a -> t m a
```

that lifts a Monad into a MT. We can use this function to return a Monad into a given transformer.

# A ReaderT WriterT example

Given the above types and functions, let's have a look at an example of using a ReaderT transformer stack.

Say we had some configuration about an external service, like its _host_ and _port_. How could we use a Reader Monad to supply that configuration to a program?

Let's start by defining a type alias to a Map of String keys and values:

```{.haskell .scrollx}
import qualified Data.Map.Lazy as M

type Config = M.Map String String
```

Let's also define a _serverConfig_ function to return our populated configuration from a list of key-value pairs:

```{.haskell .scrollx}
serverConfig :: Config
serverConfig = M.fromList [("host", "localhost"), ("port", "7654")]
```

Let's definite a function to read the host:

```{.haskell .scrollx}
getHost :: Reader Config (Maybe String)
```

Given a _Config_ this function will return the host name in a _Just_ if present, or a _Nothing_ if not.

It would could be implemented as:

```{.haskell .scrollx}
getHost :: Reader Config (Maybe String)
getHost = do
  config <- ask
  return (Map.lookup "host" config)
```

First, the _getHost_ function requests the Config instance from the environment using the _ask_ function. It then looks up the "host" key from that config. Finally it lifts the Maybe value returned from the _lookup_ function into the Reader Monad using the _return_ function.

Let's define a function to read the port:

```{.haskell .scrollx}
getPort :: Reader Config (Maybe Int)
```

It would could be implemented as:

```{.haskell .scrollx}
getPort :: Reader Config (Maybe Int)
getPort = do
  config <- ask
  return (Map.lookup "port" config >>= readMaybe)
```

This function is similar to _getHost_ with the additional bind (>>=) operation to join together the value read from _lookup_ with _readMaybe_. readMaybe tries to parse a String into a value of type Int in this case. If it successfully parses the value it returns a (_Just a_) or if it fails it returns a _Nothing_. readMaybe is defined in `Text.Read` as:

```{.haskell .scrollx}
readMaybe :: Read a => String -> Maybe a
```

Also notice that we used a Reader Monad as opposed to a ReaderT MT to read both the host and port. Since the Reader Monad and the ReaderT MT are very similar we can easily convert between them. Why didn't we use a ReaderT MT directly to read the configuration? We could have, but the ReaderT MT requires an inner Monad m:

```{.haskell .scrollx}
ReaderT r m a
```

and we haven't decided on what __m__ is at the moment. I'll demonstrate how we could have directly used a ReaderT MT to implement _getHost_ and _getPort_ [later](/posts/2018-01-12-readert-writert.html#using-readert-instead-of-reader) on.

Now that we've written functions to read the host and port, lets go ahead and use those values in a ReaderT MT along with a WriterT MT to log out the values we received from the configuration:

```{.haskell .scrollx}
getConfig :: ReaderT Config (WriterT String IO) ()
```

Given a Config type as an input, the result returned will be in a WriterT MT with a log type of String with an inner Monad of IO and a value of unit () return within IO. That sounds more complicated that it really is.

It's implemented as:

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

Let's delve into the implementation of _getConfig_. The first two lines read the host and port into Maybe values from the configuration:

```{.haskell .scrollx}
  hostM <- fromReader getHost
  portM <- fromReader getPort
```


The next two lines covert the Maybe values for host and port into their String equivalents:

```{.haskell .scrollx}
  let host = maybe "-" id hostM
      port = maybe "-" show portM
```

The next four lines write String values to the log in order:

```{.haskell .scrollx}
  _ <- log "\nConfig"
  _ <- log "\n======"
  _ <- log (printf "\nhost: %s" host)
  _ <- log (printf "\nport: %s" port)
```

and the final line returns a unit result into ReaderT r m Monad:

```{.haskell .scrollx}
return ()
```

which is ReaderT (WriterT String IO) Monad in this instance.

Let's look at the type definition of the _fromReader_ function:

```{.haskell .scrollx}
fromReader :: Monad m => ReaderT r a -> ReaderT r m a
```

_fromReader_ converts a Reader Monad to a ReaderT MT. It is implemented as:

```{.haskell .scrollx}
fromReader :: Monad m => Reader r a -> ReaderT r m a
fromReader = reader . runReader
```

The _runReader_ function is defined as:

```{.haskell .scrollx}
runReader :: Reader r a -> r -> a
```

and unwraps the Reader Monad to a function (`r -> a`). The _reader_ function (as defined previously) lifts a function from (`r -> a`) into the ReaderT MT. This seems like unnecessary work and ideally there should be an in-built function that does this for us.

Next let's have a look at the _log_ function:

```{.haskell .scrollx}
log :: (Monad m, MonadTrans t, Monoid w) => w -> t (WriterT w m) ()
log = lift . tell
```

From the type definition:

```{.haskell .scrollx}
w -> t (WriterT w m) ()
```

we can see that we are lifing some log __w__ into a transformer stack __t__ containing a WriterT w m.

We've come a long way and we've got everything setup as needed. The only thing left to do is run the transformer stack and reap our rewards. We can do that with the _readWriteConfig_ function:

```{.haskell .scrollx}
readWriteConfig :: IO ()
readWriteConfig = execWriterT (runReaderT getConfig serverConfig) >>= putStrLn
```

When running the stack, it is run from outside-in. So given a _ReaderT (WriterT String m) a_,
we:

1. Run the ReaderT MT with runReaderT. This returns result __a__ in the inner Monad __m__ which is a WriterT String m a. Substituting the IO Monad for __m__, returns a WriterT String IO (). Notice that the result __a__ is of type Unit but we don't care about the result, only the log.
1. Run the WriterT MT with execWriterT. This returns the log __w__ in the inner Monad __m__ which is an __m w__. Substituting the IO Monad for __m__ and String for __w__, returns an IO String.
1. Binding through from IO String to _putStrLn_ gives us an IO ().

The final output of running the above is:

```{.terminal .scrollx}
Config
======
host: localhost
port: 7654
```

## Using ReaderT instead of Reader

Here are the functions that need to be rewritten if we directly use ReaderT MT instead of using the Reader Monad to read the configuration:

_getHost2_

```{.haskell .scrollx}
getHost2 :: Monad m => ReaderT Config m (Maybe String)
getHost2 = -- same as getHost
```

_getPort2_

```{.haskell .scrollx}
getPort2 :: Monad m => ReaderT Config m (Maybe Int)
getPort2 = -- same as getPort
```

_getConfig2_

```{.haskell .scrollx}
getConfig2 :: ReaderT Config (WriterT String IO) ()
getConfig2 = do
  hostM <- getHost2 -- no need to call fromReader
  portM <- getPort2 -- no need to call fromReader
  ... -- same as getConfig
```

We can see that this solution is a lot easier with less work to do. We just needed to add a Monad type constraint to the _getHost_ and _getPort_ functions. We also have no need for the _fromReader_ function which is a bonus! We can also call the _readWriteConfig_ function with _getConfig2_ instead of _getConfig_ and it all works:

_readWriteConfig2_

```{.haskell .scrollx}
readWriteConfig2 :: IO ()
readWriteConfig2 = execWriterT (runReaderT getConfig2 serverConfig) >>= putStrLn
```

The complete Solution

```{.haskell .scrollx}
module Config (readWriteConfig) where

import Text.Printf (printf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)
import Data.Functor.Identity (Identity, runIdentity)
import Text.Read (readMaybe)
import Prelude hiding (log)

type Config = Map.Map String String

serverConfig :: Config
serverConfig = Map.fromList [("host", "localhost"), ("port", "7654")]

-- variation with Reader

getHost :: Reader Config (Maybe String)
getHost = do
  config <- ask
  return (Map.lookup "host" config)

getPort :: Reader Config (Maybe Int)
getPort = do
  config <- ask
  return (Map.lookup "port" config >>= readMaybe)

fromReader :: Monad m => Reader r a -> ReaderT r m a
fromReader = reader . runReader

log :: (Monad m, MonadTrans t, Monoid w) => w -> t (WriterT w m) ()
log = lift . tell

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

readWriteConfig :: IO ()
readWriteConfig = execWriterT (runReaderT getConfig serverConfig) >>= putStrLn

-- variation with ReaderT

getHost2 :: Monad m => ReaderT Config m (Maybe String)
getHost2 = do
  config <- ask
  return (Map.lookup "host" config)

getPort2 :: Monad m => ReaderT Config m (Maybe Int)
getPort2 = do
  config <- ask
  return (Map.lookup "port" config >>= readMaybe)

getConfig2 :: ReaderT Config (WriterT String IO) ()
getConfig2 = do
  hostM <- getHost2
  portM <- getPort2
  let host = maybe "-" id hostM
      port = maybe "-" show portM
  _ <- log "\nConfig"
  _ <- log "\n======"
  _ <- log (printf "\nhost: %s" host)
  _ <- log (printf "\nport: %s" port)
  return ()

readWriteConfig2 :: IO ()
readWriteConfig2 = execWriterT (runReaderT getConfig2 serverConfig) >>= putStrLn

```

# A tale of at least two Monads

![Monads](https://pbs.twimg.com/media/CgKMfpQWwAAEsJQ.jpg)

Each Monad Transformer is composed of at least two Monads. If we take ReaderT as an example, we have its definition as:

```{.haskell .scrollx}
ReaderT r m a
```

where ReaderT r m is a Monad and __m__ is a Monad. If you stack Monads, in the __m__ type variable as with a WriterT for example:

```{.haskell .scrollx}
ReaderT r (WriterT w m) a
```

then ReaderT r (WriterT w m) is a Monad, WriterT w m is a Monad and __m__ is a Monad. Talk about Monad overload!

---------

mapM      :: Monad m      => (a -> m b) -> t a -> m (t b)
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

sequenceA :: Applicative f => t (f a) -> f (t a)
sequence   :: Monad m      => t (m a) -> m (t a)


# References

1. [use-readert-maybe-or-maybet-reader](https://stackoverflow.com/questions/43840588/use-readert-maybe-or-maybet-reader#43840589)