---
title: Cabal Fails to Install MySQL Linux Driver
author: sanjiv sahayam
description: If your cabal installation of mysql-simple is failing on mysql, you may want to install the mysql-client library on linux.
tags: cabal, haskell, linux
---
When trying to install [mysql-simple](http://hackage.haskell.org/package/mysql-simple-0.2.2.4) on GHC 7.8.3 with cabal 1.18.1.3 on Ubuntu Linux 12.04, the [mysql](https://hackage.haskell.org/package/mysql) dependency failed with the following error:

    mysql-0.1.1.6 failed during the configure step. The exception was:
    ExitFailure 1
    mysql-simple-0.2.2.4 depends on mysql-0.1.1.6 which failed to install.


A quick [SO](http://stackoverflow.com/questions/7475223/mysql-config-not-found-when-installing-mysqldb-python-interface) lead me to the solution. I needed to install the mysql-client library on linux:

    sudo apt-get install libmysqlclient-dev

Enjoy! :)