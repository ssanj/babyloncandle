---
title: Loading a Package into GHCi through Stack
author: sanjiv sahayam
description: Loading a package into GHCi without creating a Stack project
tags: haskell, stack
comments: true
---

Ever wanted to play around with a particular set of packages in GHCi but didn't want to setup a project? You're in luck. With Stack you can now selectively load named packages and launch directly into GHCi. The incantation you need is:

```{.terminal .scrollx}
stack ghci --package [package-name1] --package [package-name2]
```

For example to load the transformers package:

```{.terminal .scrollx}
stack ghci --package transformers
```

And now we have transformers loaded in GHCi:


```{.command .scrollx}
*Main Lib> import Control.Monad.
Control.Monad.Fail                 Control.Monad.Trans.Except
Control.Monad.Fix                  Control.Monad.Trans.Identity
Control.Monad.IO.Class             Control.Monad.Trans.List
Control.Monad.Instances            Control.Monad.Trans.Maybe
Control.Monad.ST                   Control.Monad.Trans.RWS
Control.Monad.ST.Lazy              Control.Monad.Trans.RWS.Lazy
Control.Monad.ST.Lazy.Safe         Control.Monad.Trans.RWS.Strict
Control.Monad.ST.Lazy.Unsafe       Control.Monad.Trans.Reader
Control.Monad.ST.Safe              Control.Monad.Trans.State
Control.Monad.ST.Strict            Control.Monad.Trans.State.Lazy
Control.Monad.ST.Unsafe            Control.Monad.Trans.State.Strict
Control.Monad.Signatures           Control.Monad.Trans.Writer
Control.Monad.Trans.Class          Control.Monad.Trans.Writer.Lazy
Control.Monad.Trans.Cont           Control.Monad.Trans.Writer.Strict
Control.Monad.Trans.Error          Control.Monad.Zip
*Main Lib> import Control.Monad.Trans.Writer.Lazy
```

[Reference](https://stackoverflow.com/questions/39848576/load-a-new-package-in-ghci-using-stack#39848577)
