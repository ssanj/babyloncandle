---
title: Escaping Cabal Hell
author: sanjiv sahayam
description: Some simple steps to escaping Cabal Hell and reach Cabal Heaven.
tags: cabal, haskell
---

Cabal. What a nightmare. Right? That was the view I had on it until recently. I had just upgraded to GCH 7.8.3 and almost none of my projects built with Cabal anymore. Hell. So I decided it was time to learn to use Cabal properly. In my research I came across the extremely useful [Cabal Survival Guide](http://www.haskell.org/haskellwiki/Cabal/Survival) which had many great pointers. So have a read of it if your Cabal project fails to build.

[Cabal Sandboxes](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) are also a great way to isolate your project's dependencies from the rest of your system and drastically reduce the number of dependency issues you will have with Cabal.

And if you haven't read it, have a read of the [Cabal User Guide](http://www.haskell.org/cabal/users-guide/) to understand how things work. Read the User Manual? Are you crazy?

Some other unusual errors have missing header files (.h). I came across one of these [errors](http://sanj.ink/posts/2014-10-23-cabal-fails-to-install-pcre-light-linux.html) and the solution was to install the necessary library on the OS. So basically any Haskell wrappers around OS libraries would (obviously) need the correct versions of the library installed.

Hopefully a basic understanding of Cabal usage plus some solutions to common problems will lead you to Cabal heaven. Too soon? ;)


