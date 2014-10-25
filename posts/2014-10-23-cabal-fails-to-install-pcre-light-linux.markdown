---
title: Cabal Fails to Install PCRE-Light on Linux
author: sanjiv sahayam
description: If your cabal installation of mysql-simple is failing on PCRE-Light, you may want to install the PCRE library on linux.
tags: cabal, haskell, linux
---
When trying to install [mysql-simple](http://hackage.haskell.org/package/mysql-simple-0.2.2.4) on GHC 7.8.3 with cabal 1.18.1.3 on Ubuntu Linux 12.04, the [pcre-light](https://hackage.haskell.org/package/pcre-light) dependency failed with the following error:

    mysql-simple-0.2.2.4 depends on pcre-light-0.4.0.3 which failed to install.
    pcre-light-0.4.0.3 failed during the configure step. The exception was:
    ExitFailure 1

Thankfully since I had come across [the same problem before on my Mac](http://blog.ssanj.net/posts/2014-09-08-cabal-fails-to-install-pcre-light-macosx.html), the solution was an easy one.

Install the pcre-light package on linux with:

    sudo apt-get install libpcre3 libpcre3-dev

Enjoy! :)