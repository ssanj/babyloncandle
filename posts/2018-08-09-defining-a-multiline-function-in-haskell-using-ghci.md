---
title: Defining a multiline function in Haskell using Ghci
author: sanjiv sahayam
description: How to define a function over multiple lines in Ghci
tags: haskell
comments: true
---

I've always found it difficult to remember the exact syntax for setting Ghci into multiline mode and defining a function therein. Below are the steps for easy access.

Start by setting Ghci into multiline mode with:

```{.command .scrollx}
:set +m
```

You can start a multiline block with `:{` and end it with `:}`.

Function definitions must be preceded with `let`. This has tripped me up many times.

For example, to define a function that pauses for a given delay before printing out "done":

```{.haskell .scrollx}
:{
  let printAfter :: Int -> IO ()
      printAfter delay =
        do putStrLn $ (\d -> "waiting for " ++ d ++ " microseconds") $ show delay
           threadDelay delay
           putStrLn "done"
:}
```

To unset multiline mode use:

```{.command .scrollx}
:unset +m
```

References: [Multi-line commands in GHCi](https://stackoverflow.com/questions/8443035/multi-line-commands-in-ghci), [How to define a function in ghci across multiple lines?](https://stackoverflow.com/questions/2846050/how-to-define-a-function-in-ghci-across-multiple-lines)
