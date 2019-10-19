---
title: Pretty Printing Json in Haskell
author: sanjiv sahayam
description: How to pretty print Json in Haskell
tags: haskell
comments: true
---

While working on [Milo](https://github.com/ssanj/milo) - a small twitter client, I came across an issue where I wanted to dump the full response Json I received from the Twitter API to the console. While I could write this response out as a `String`, it was very hard to read as it was not nicely indented. I had to resort to copying this text into Sublime Text and pretty printing the Json there using a plugin. Certainly not ideal.

What would be nice is to be able to pretty print the Json content of the response directly to the console without the need for external tools. I got some hints on how to do it from [Adventures in pretty printing JSON in haskell](https://onoffswitch.net/2015/08/15/adventures-pretty-printing-json-haskell/).

To do this I first had to convert the response text into a Json value. I was using [Aeson](https://www.stackage.org/lts-14.3/package/aeson-1.4.4.0) Json library in Haskell and all Json is presented by the [Value](https://www.stackage.org/haddock/lts-14.3/aeson-1.4.4.0/Data-Aeson.html#t:Value) data type:

```{.scrollx .haskell}
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
```

The primary way to convert some type `a` to a `Value` is by the use of the `ToJSON` typeclass:

```{.scrollx .haskell}
class ToJSON a where
    -- | Convert a Haskell value to a Json-friendly intermediate type.
    toJSON     :: a -> Value
    -- The rest of the class has been omitted for brevity
```

While I had written conversions from some concrete types to a Json value, I did not have a way to convert from a `ByteString` (which was what was returned to me from the [http-client](https://www.stackage.org/lts-14.3/package/http-client-0.6.4) library) to a Json `Value`. It seemed obvious that there should be an instance of `ToJSON` for `ByteString` - but there wasn't. There were instances for `String` via `[Char]` and `Text` though and it seemed like the obvious way to make this conversion happen was by converting the `ByteString` to one of these two types.

I also came across the [aeson-pretty](https://www.stackage.org/lts-14.3/package/aeson-pretty-0.8.7) library which pretty prints Json values. The `encodePretty` and `encodePretty'` functions looked of particular interest:

```{.scrollx .haskell}
encodePretty :: ToJSON a => a -> ByteString

encodePretty' :: ToJSON a => Config -> a -> ByteString
```

In order to use one of the `encodePretty` functions I would have to find some way of converting the lazy `ByteString` (`LBS.ByteString`) into a `ToJSON` instance and then have it converted back into a `LBS.ByteString` by one of the `encodePretty` functions. A little weird to say the least.

So how do we convert a `LBS.ByteString` into a `ToJSON` value? Not surprisingly there is an instance of `TOJSON` for  `Value` which just returns the supplied Json `Value`. This seems obvious in hindsight but had me stumped for a while. Given a Json `Value` we should be able to get a pretty-printed `LBS.ByteString` representation of that Json `Value` using one of the `encodePretty` functions:

```{.scrollx .haskell}
jsonToText :: Value -> LBS.ByteString
jsonToText = encodePretty
```

Now that we have a `LBS.ByteString` with our pretty Json content, we need to convert it to a format that can be printed out to the console.

But first a quick primer on a few different ways of representing Strings in Haskell:

- ByteStrings

 > A time and space-efficient implementation of byte vectors using packed Word8 arrays, suitable for high performance use, both in terms of large data quantities, or high speed requirements. This has both lazy and strict variants.

- Text
  
 > A time and space-efficient implementation of Unicode text. Suitable for performance critical use, both in terms of large data quantities and high speed. This also has lazy and strict variants.

Given that Tweets use a lot of unicode characters for emojis, internationalisation and the like, using `Text` (Data.Text) seemed to be the best option for this conversion.

The `decodeUtf8` function in `Data.Text.Encoding` which is defined as:


```{.scrollx .haskell}
decodeUtf8 :: ByteString -> Text
```

seems useful and is in the general direction we want to go. Unfortunately it requires a strict `ByteString` and we need a way to convert from our `LBS.ByteString` into a strict one.

The `toStrict` function in `Data.ByteString.Lazy` which is defined as:


```{.scrollx .haskell}
toStrict :: LSB.ByteString -> ByteString
```

can handle the coversion for us. 

Now that we have all the pieces we can convert from our lazy `ByteString` to `Text`:

```{.scrollx .haskell}
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.Encode.Pretty (encodePretty)

lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> T.Text
jsonToText = lsbToText . encodePretty
```

`Data.Text.IO` has the `Text` equivalent of `putStrLn` and `putStr` for `String`:


```{.scrollx .haskell}
putStr   :: Text -> IO ()
putStrLn :: Text -> IO ()
```

We can use these functions so that we don't need to convert between `String` and `Text` to print out our Json content.

Here's the final code to write out pretty Json:

```{.scrollx .haskell}
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.IO as TIO

lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> T.Text
jsonToText = lsbToText . encodePretty

prettyPrint :: Value -> IO ()
prettyPrint = TIO.putStrLn . jsonToText
```

With the sample Json of:


```{.scrollx .haskell}
tweetJson :: Value
tweetJson = 
  object [
           "created_at" .= A.String "Wed Sep 18 01:28:16 +0000 2019", 
           "user" .= object [
                              "screen_name" .= A.String "tweetbot", 
                              "name" .= A.String "The Twittebot" 
                           ],
           "full_text" .= A.String "this is a strange tweet",
           "lang" .= A.String "en"
        ]

```

The following output is printed:

```{.scrollx .json}
{
    "user": {
        "screen_name": "tweetbot",
        "name": "The Twittebot"
    },
    "lang": "en",
    "created_at": "Wed Sep 18 01:28:16 +0000 2019",
    "full_text": "this is a strange tweet"
}
```