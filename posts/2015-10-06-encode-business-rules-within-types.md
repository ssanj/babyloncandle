---
title: Encode Business Rules Within Types
author: sanjiv sahayam
description: How to use types to prevent bugs and write cleaner code.
tags: scala
comments: true
---

A technique I've been using quite frequently is to encode a set of rules for an object within a type.

Take a basic Person class as an example:

```{.scala .scrollx}
final case class Person(name: String, age: Int)
```

While this definition is very easy to understand, it does allow some inconsistencies and bugs to creep into the code.

So what are some of the problems with this definition? Because _name_ is a String, it will accept any String value: "Bob", "1234", "@JOE" and "". These might not be valid names for a Person. There might be definite rules around the names that can be used. For instance any valid name should not be an empty String. It should probably not contain numbers. There could be other rules but these are some obvious ones.

Age similarly has problems. Because we use an Int to present it, any Int will be allowed. 1000 and -10 are valid Int values, but these are not valid ages. We may need some constraints on this age such as between 0 and 120.

The main issue with using imprecise types like String and Int to represent specific types is that we allow a whole range of values that are invalid to be used along with a small subset of valid values for those types.

Given the above definition of Person we can construct the following invalid Person instances as described:

```{.scala .scrollx}
Person("", 23)
Person("Bob", -1)
```

So now do we stop this from happening?

First let's define the rules for name and age:

 * name: A non empty String, with only alphabetic characters.
 * age: a number between 0 and 120.

We need to control how a Person instance is created. There has to be only one valid way to create a Person. We do this by making the constructor on the Person case class, private. In the companion object we add a method to create a valid Person instance via this private constructor.

## Option 1: Encode name and age rules into Person ##

If the name or age is not valid then we return an Option[Person].

```{.scala .scrollx}
final case class private Person(name: String, age: Int)

object Person {
  def createPerson(name: String, age: Int): Option[Person] =
    for {
      n <- Option(name).
              map(_.trim).
              filterNot(_.isEmpty).
              filter(_.forall(_.isLetter))
      a <- if (age >= 0 && age <= 120) Option(age) else None
    } yield Person(n, a)
}
```

__note__: _Unfortunately we can't create another apply method on the Person companion object as the compiler complains that we have duplicate apply methods. Therefore we are relegated to using the createObject naming format for factory methods._

Now we can't create an invalid Person instance:

```{.scala .scrollx}
Person.create("", 23)
```

```{.terminal .scrollx}
None
```

```{.scala .scrollx}
Person.create("Bob", 1000)
```

```{.terminal .scrollx}
None
```

We can create a valid instance:

```{.scala .scrollx}
Person.create("Bob", 23)
```

```{.terminal .scrollx}
Some(Person(Bob,23))
```

When we create a Person object we know that it has valid a name and age. But the user of the Person object has no idea that is the case by looking at the object definition. It's any String for a name and any Int for an age.

Further if the name or age of a Person is used in another method we have no guarantees that the rules governing them still hold.

```{.scala .scrollx}
def ageBracket(age: Int): String = {
  if (age <= 20) "young"
  else if (age <= 40) "prime"
  else "old"
}
```

__note__:_We should encode the return type of ageBracket into its own ADT: (Young|Prime|Old). I have left this out for brevity._

Now ageBracket can easily be called with an invalid age. Of course we could pass in a Person to ageBracket, but we shouldn't have to. We only care about age. Also the Person object now has knowledge of all the rules governing the creation of a name and an age. This is a conflation of concerns. We need to separate them.

## Option 2: Encode name and age rules into their own types ##

Encode the rules for a name and an age into their own types.

```{.scala .scrollx}
final case class Person private(name: Name, age: Age)

final case class Name private(value: String)
final case class Age private(age: Int)

object Name {
 def createName(name: String): Option[Name] =
   Option(name).
     map(_.trim).
     filterNot(_.isEmpty).
     filter(_.forall(_.isLetter)).
     map(Name(_))
}

object Age {
  def createAge(age: Int): Option[Age] = if (age >= 0 && age <= 120) Option(Age(age)) else None
}

object Person {
  def createPerson(name: String, age: Int): Option[Person] =
    for {
      n <- Name.createName(name)
      a <- Age.createAge(age)
     } yield Person(n, a)
}

def ageBracket(age: Age): String = {
  if (age.age <= 20) "young"
  else if (age.age <= 40) "prime"
  else "old"
}
```

Let's try to create a Person with an invalid name:
```{.scala .scrollx}
Person.createPerson("", 23)
```

```{.terminal .scrollx}
None
```

Let's try to create a Person with an invalid age:

```{.scala .scrollx}
Person.createPerson("Bob", -1)
```

```{.terminal .scrollx}
None
```

Let's try to create a Person with a valid name and age:

```{.scala .scrollx}
Person.createPerson("Bob", 23)
```

```{.terminal .scrollx}
Some(Person(Name(Bob),Age(23)))
```

So now when we create or are given a Person object we know definitely that the Person instance is valid and that the Name and Age are valid. We can also use the Name and Age values independently because we know they remain valid outside a Person object. The Person class doesn't have knowledge about what rules are applied to create a Name or an Age. The concerns are separated.

The full [example source](https://github.com/ssanj/encode-business-rules-in-types) can be found on Github.

__note__: _If you need more information on failures during construction use an [Either](https://github.com/scala/scala/blob/v2.11.7/src/library/scala/util/Either.scala) or [\\/](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/Either.scala) instead of Option._