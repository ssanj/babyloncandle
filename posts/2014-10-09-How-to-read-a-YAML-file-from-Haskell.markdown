---
title: How to read a YAML file from Haskell
description: A short example on how to read a YAML file from Haskell using Aeson and Data.Yaml.
author: sanjiv sahayam
tags: haskell
---

While working on BrainCandy (My first Haskell web application), I needed a way to load in some database configuration. As I use YAML quite frequently in other languages such as  Java, I thought it would be nice to use it through Haskell as well. After a bit of [SO](http://stackoverflow.com) and Google I came across a few good examples that helped me get started. (See the references for more details.)

Given how easy it is to read YAML through Haskell, I thought I'd write a very short example to get people started.

The config.yml file:

```{.yaml}
username: "John Smith"
password: "Sm1thy@Jon0"
```

The Haskell code to read the config.yml file:

```{.haskell .scrollx}
{-# LANGUAGE DeriveGeneric #-} -- (2)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson


data Cred = Cred { username :: String, password :: String } deriving (Show, Generic) -- (1,2)
instance FromJSON Cred -- (3)

main :: IO ()
main = do
    content <- BS.readFile "config.yml" -- (4)
    let parsedContent = Y.decode content :: Maybe Cred -- (5)
    case parsedContent of
        Nothing -> error "Could not parse config file."
        (Just (Cred u p)) -> putStrLn ("username: " ++ u ++ ", password: " ++ p)
```

Here are the basic steps:

1. Create a datatype to match the YAML structure of your config file. __Creds__ in this case.
2. Make sure your datastructure derives __Show__ and __Generic__. You need to use {-# LANGUAGE DeriveGeneric #-} to derive Generic instances.
3. Create an __Aeson.FromJSON__ instance of your datatype.
4. Read the content of your config file as __ByteString__ .
5. Pass file contents to the __Data.Yaml.decode__ function. The __Data.YAML.decode__ function returns a __Maybe Cred__ as it might not be able to parse the config file into the supplied data structure.

The [cabal project](https://github.com/ssanj/HaskellYamlReaderExample) for this example can be found on github.

Now go externalize those configurations!

References:

* [Parsing config files in Haskell with Yaml](http://lenguyenthedat.blogspot.com.au/2014/01/parsing-config-file-in-haskell-with-yaml.html)
* [Reading YAML lists of objects in Haskell](http://stackoverflow.com/questions/21292428/reading-yaml-lists-of-objects-in-haskell)
