---
title: If it's memorable it's reusable
author: sanjiv sahayam
description: Using memorable phrases to encode software development techniques
tags: software-development
comments: true
---

Memorable phrases are like a catchy tune; you can't get them out of your head. Take this phrase for instance:

> Make the change easy, then make the easy change

I heard it first on the [Elm Radio](https://elm-radio.com/episode/incremental-steps) podcast. This was some very useful advice on how to introduce changes to a code base. First make it easy to introduce the change by refactoring your code, adding in tests etc. Then swoop in an add your change with minimal fuss. This is a nice reusable technique.

[Kent Beck](https://twitter.com/KentBeck/status/250733358307500032?s=20) originally said this back in 2012. I remember mentioning this pattern to a few colleagues at work and sure enough even months later they still keep referring to it.

If something is memorable, you are more likely to use it solve a given problem.

This got me thinking:

> Is memorable advice more useful than unmemorable advice?


It seems obvious in hindsight. What good is advice if no one remembers it?

Another phrase I heard of recently was

> Teach me how to message

a technique used by [Richard Feldman](https://youtu.be/RN2_NchjrJQ) to identify functions of the following type in Elm:

```haskell
(CustomType -> msg)
```

Based on how you implement this function, you "teach" it how to create the appropriate message. Here's an example from Richard's presentation:

```haskell
Article.view :
  (Article a -> msg) //teach this how to message
  -> Article a
  -> Html msg
```

Now everytime I see a function of the above shape I always think that it needs me to teach it how to message. Somehow that makes it easier to implement this function. I know what it "needs".

There is something calming about seeing a problem in the code and knowing how to go about solving it. These catchy phrases become part of your swissarmy knife used to carve out solutions.

The next time you has stumbled on a great technique on how to solve a problem try and package it in memorable way.


