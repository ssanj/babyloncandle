---
title: Functional Programming in Python
author: sanjiv sahayam
description: Functional Programming concepts as used in Python.
tags: fp, python
comments: true
---

While working on [Scoggle](https://github.com/ssanj/Scoggle) I came across a number of Functional Programming concepts that I could use to implement it - much to my surprise. Here are the concepts I came across:

## Map ##

```{.python .scrollx}
return list(map(lambda x: self.get_file_without_extension(x), files))
```

## Filter ##

This example has a map and a filter and is very similar to a [Haskell list comprehension](http://learnyouahaskell.com/starting-out#im-a-list-comprehension).

```{.python .scrollx}
hits = [os.path.join(root, f) for f in filenames if f.endswith(".scala") and strategy(root, dirnames, f)]
```

## Higher-Order Functions ##

In this example the returned __m__ is  a reference to the constructor of a class.

```{.python .scrollx}
def get_class(self, kls):
    parts = kls.split('.')
    module = ".".join(parts[:-1])
    m = __import__( module )
    for comp in parts[1:]:
        m = getattr(m, comp)            
    return m
```

## Currying ##

We get to curry a function with [Pythons functools library](https://docs.python.org/3.2/library/functools.html):

```{.python .scrollx}
matches_file = fp.partial(cut.match_test_file, None, None)
```