---
title: Loading your Stack Project into GHCi
author: sanjiv sahayam
description: Loading your Stack Project into GHCi
tags: haskell, stack
comments: true
---


Sometimes it's quite useful to play around with functions and data types in your Stack project from within the GHCi. You can do this by simply running:

```{.terminal .scrollx}
stack ghci
```

from the root of your Stack project.

This however does not include any test sources or dependencies you may have. To include those as well run:


```{.terminal .scrollx}
stack ghci --test --no-load
```