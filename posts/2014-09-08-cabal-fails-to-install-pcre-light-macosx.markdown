---
title: Cabal Fails to Install PCRE-Light on Macosx
author: sanjiv sahayam
description: If your cabal installation of mysql-simple is failing on PCRE-Light, you may want to install the PCRE library on macosx.
tags: cabal, haskell, macosx
---

When trying to install [mysql-simple](http://hackage.haskell.org/package/mysql-simple-0.2.1.1) on GHC 7.8.3 with cabal 1.18.1.4 on Mac OSX 10.9.4, the [pcre-light](https://hackage.haskell.org/package/pcre-light) dependency failed with the following error:

    Base.hsc:103:10: fatal error: 'pcre.h' file not found

After googling around for a bit I came across this thread on [Stackoverflow](https://stackoverflow.com/questions/22555561/error-building-fatal-error-pcre-h-no-such-file-or-directory/22559967) which had the same missing header file error, but which had nothing to do with Haskell. So from the sounds of things I just needed to install the PCRE library on my Mac.

I installed PCRE on MacOSX with:

    brew install pcre

After which ```cabal install --only-dependencies``` completed successfully. Yay! :)
