---
title: Java Reflection-Style Class Loading in Python
author: sanjiv sahayam
description: How to dynamically load a class in Python similar to what you could do in Java through reflection.
tags: java, python
comments: true
---

While working on [Scoggle](https://github.com/ssanj/Scoggle) I wanted to be able to dynamically load matcher classes. This is very easy using Java with reflection. [I found this SO article on how to do just that in Python](http://stackoverflow.com/questions/452969/does-python-have-an-equivalent-to-java-class-forname). Given a package path to a class, this piece of code loads each module and returns the final class constructor:

```{.python}
def get_class( kls ):
    parts = kls.split('.')
    module = ".".join(parts[:-1])
    m = __import__( module )
    for comp in parts[1:]:
        m = getattr(m, comp)            
    return m
```

 The author explains how it works:

 > We're using __import__ to import the module that holds the class, which required that we first extract the module name from the fully qualified name. Then we import the module:

```{.python}
 m = __import__( module )
```
 
 > In this case, m will only refer to the top level module,

 > For example, if your class lives in foo.baz module, then m will be the module foo.
> We can easily obtain a reference to foo.baz using getattr( m, 'baz' ).

 > To get from the top level module to the class, have to recursively use gettatr on the parts of the class name

 > Say for example, if you class name is foo.baz.bar.Model then we do this:

```{.python}
 m = __import__( "foo.baz.bar" ) #m is package foo
 m = getattr( m, "baz" ) #m is package baz
 m = getattr( m, "bar" ) #m is module bar
 m = getattr( m, "Model" ) #m is class Model
``` 

> This is what's happening in this loop:

```{.python}
for comp in parts[1:]:
    m = getattr(m, comp)    
```    

> At the end of the loop, m will be a reference to the class. This means that m is actually the class itself, you can do for instance:

```{.python}
  a = m() #instantiate a new instance of the class    
  b = m( arg1, arg2 ) # pass arguments to the constructor
```
