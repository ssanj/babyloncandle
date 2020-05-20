---
title: An even Easier Way to Define a Multiline Function in Haskell using GHCi
author: sanjiv sahayam
description: The easy way to define a function over multiple lines in Ghci
tags: haskell, ghci
comments: true
---

I previously blogged about [Defining a multiline function in Haskell using Ghci](https://sanj.ink/posts/2018-08-09-defining-a-multiline-function-in-haskell-using-ghci.html). The solution was somewhat cumbersome and while hunting around for other options, [I came across what I consider to be the simplest way to write a multiline function in GHCi](https://stackoverflow.com/questions/45362445/defining-function-signature-in-ghci).

From within GHCi, enclose your function between `:{` and `:}` strings.

For example:

```{.haskell .scrollx}
:{
  printAfter :: Int -> IO ()
  printAfter delay = do
    putStrLn $ (\d -> "waiting for " ++ d ++ " microseconds") $ show delay
    Control.Concurrent.threadDelay delay
    putStrLn "done"
:}
```

If you can't be bothered to add the opening and closing braces or you don't need nice formatting, you can smash the whole function on one line by replacing every newline with a `;`:


```{.haskell .scrollx}
printAfter :: Int -> IO (); printAfter delay = do; putStrLn $ (\d -> "waiting for " ++ d ++ " microseconds") $ show delay; Control.Concurrent.threadDelay delay;putStrLn "done"
```

And that's it!