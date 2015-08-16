---
title: Programming in Python
author: sanjiv sahayam
description: Thoughts on programming in Python.
tags: python
comments: true
---

[I needed to create a Sublime Text plugin](http://sanj.ink/posts/2015-08-16-Scoggle.html). Sublime Text plugins are written in Python. I had never written any Python code before. I thought I'd give it a crack anyway. [The results were pretty nice](https://packagecontrol.io/packages/Scoggle).

I usually work in Scala which is a strongly-type language. I had my reservations about working in Python which is a dynamically typed language. I had experience coding in other dynamically typed languages such as Javascript. It was not something I enjoyed thoroughly.

Python turned out to be a different beast. For one it was fast. This was a good thing. The repl was easy to use and returned a rich set of information. help(..) and dir(..) are actually useful. The api documentation is also reasonably good. Since it was dynamic it was very easy to get started with something. 

Something that crash when you ran it. This is the balance between static typing and dynamic typing. You pay at compilation time in static typing but you are more sure about what your program will do. With dynamic typing you pay later but you pay in production.

One problem with paying in production is that any syntax errors, method name errors etc, will only occur when you run some code that actually uses that erroneous pathway through the code. There could very well be pathways through the code that would blow up one day if it were used. This is pretty scary and doesn't inspire much confidence. If you are using dynamic languages your code coverage should be close to a hundred percent. A good IDE or linter would also help catch some of these errors early.

[I found it fairly easy to use Functional Programming concepts](http://sanj.ink/posts/2015-08-14-functional-programming-in-python.html) in Python which suited me very well. So that's a plus.

Python is a easy language to get into. It's also fun to use. I wouldn't recommend it for production code though. But hey, being a static-typing fan, I'm probably biased.