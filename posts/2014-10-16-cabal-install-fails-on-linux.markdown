---
title: Cabal Install cabal-install Fails on Linux
author: sanjiv sahayam
description: My cabal upgrade of cabal via cabal-install failed on Ubuntu 12.04 with the rather cryptic error of&#58; "cannot find -lz". Here are the steps I took to resolve the problem.
tags: cabal, haskell, linux
---

While upgrading cabal with cabal-install on Ubuntu 12.04 I received the following error:

    Linking dist/build/cabal/cabal ...
    /usr/bin/ld: cannot find -lz


After that extremely helpful error message, I decided to trawl the Internet. I found information on a similar error on [SO](http://stackoverflow.com/questions/16952741/unable-to-install-yesod-bin) that pointed to some possible libraries I needed to install on my OS.

The __-l__ in __-lz__ seemed to indicate __libghc__ and the following characters the name of the library, in this case: __z__. So I searched for a list of libraries that were of the form: libghc-z* and came up with this list:

* libghc-zeromq-haskell-dev
* libghc-zeromq-haskell-doc
* libghc-zeromq-haskell-prof
* libghc-zip-archive-dev
* libghc-zip-archive-doc
* libghc-zip-archive-prof
* libghc-zlib-bindings-dev
* libghc-zlib-bindings-doc
* libghc-zlib-bindings-prof
* libghc-zlib-conduit-dev
* libghc-zlib-conduit-doc
* libghc-zlib-conduit-prof
* libghc-zlib-dev
* libghc-zlib-doc
* libghc-zlib-enum-dev
* libghc-zlib-enum-doc
* libghc-zlib-enum-prof
* libghc-zlib-prof


So I decided to filter the list to only __libghc-zlib-__ variations. I also removed __-doc__ and __-prof__  variants. I further removed __-conduit-__ variations because I was not using __conduit__.

The final list was:

1. libghc-zlib-dev
1. libghc-zlib-bindings-dev
1. libghc-zlib-enum-dev

I decided to install each library in turn. After which I would install cabal-install to see if it worked.

After installing __libghc-zlib-dev__ via: ```sudo apt-get install libghc-zlib-dev``` I found that
cabal-install installed successfully. That turned out to be a very lucky guess.

Happy Days :)
