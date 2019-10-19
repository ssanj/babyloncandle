---
title: Running Hoogle Locally for Haskell Dev
author: sanjiv sahayam
description: How to run a local Hoogle server for looking up your Haskell api documentation
tags: haskell
comments: true
---

[Stackage](https://www.stackage.org/) is a neat tool to use when developing Software in Haskell. It's integrated with the [Hoogle](http://www.haskell.org/hoogle/) search engine that can search on type signatures (and function names) which lets you find almost any function easily.

One of the downsides of Hoogle is that it can be slow (at least for me from Australia) and so looking up any kind of documentation becomes a really frustrating experience.

The other issue with using Hoogle is that when you look up documentation for a function of a library that you use in a project, you need to make sure you are looking at the documentation for the exact version of that library that you are using in your `package.yml` or `.cabal` file; otherwise you maybe looking at an entirely different api and end up wasting a lot of time. 

Stackage solves this by letting you search against a particular snapshot where all the versions of your libraries will be pinned against that snapshot (see the *resolver* field in `stackage.yaml`). But I find Stackage searches slow as well. :(

Wouldn't it be nice if we could run Hoogle locally for our project with all the documentation we need available at our fingertips? What would be even better is that Hoogle was seeded with only the versions of dependencies in our project that we care about.

Well thanks to the fantastic [An opinionated guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/) by [Alexis King](https://lexi-lambda.github.io/resume.html) we now know how!

The basic steps are:

1. Generate Haddock documentation when you compile your project

```{.terminal .scrollx}
stack test --fast --haddock-deps
```

The first time you run this it can take a while.

2. Generate a local Hoogle database

```{.terminal .scrollx}
stack hoogle -- generate --local
```

3. Run your local Hoogle server

```{.terminal .scrollx}
stack hoogle -- server --local --port=8080
```

4. Profit

You can get full access to your local Hoogle with:

```{.terminal .scrollx}
open localhost:8080
```

or open the documentation of a specific library only:

```{.terminal .scrollx}
stack haddock --open lens
```

Something to be aware of is that when you add new dependencies or change existing ones in your project you need to generate your local Hoogle database again: 

> Unfortunately, you will have to manually regenerate the Hoogle database when you install new packages and their documentation, which you can do by re-running stack hoogle -- generate --local. Fortunately, regenerating the database doesn’t take very long, as long as you’ve been properly rebuilding the documentation with --haddock-deps.

This shouldn't take too long though if you've been generating your Haddock as per step 1.

Thanks Alexis! You rock! :)