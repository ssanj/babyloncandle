---
title: Using Validated for Error Accumulation in Scala with Cats
author: sanjiv sahayam
description: How to accumulate errors in Validated
tags: scala
comments: true
---

The `Either` data type allows us to represent a computation that may fail. A simplified definition of an `Either` is given below:


```{.scala .scrollx}
sealed abstract class Either[+A, +B]
final case class Left[+A, +B](value: A) extends Either[A, B]
final case class Right[+A, +B](value: B) extends Either[A, B]
```

We use the two data constructors of Either to represent an error or a success. We wrap an error in the `Left` constructor or a successful value in the `Right` constructor. Let's look at a simple example to make that clearer.

Say we want to validate a person's name, age and email address. We could use the following ADT (algebraic data type) to do that:

```{.scala .scrollx}
  sealed trait PersonErrorType
  case object NameInvalid extends PersonErrorType
  case object AgeInvalid extends PersonErrorType
  case object EmailInvalid extends PersonErrorType

  final case class PersonError(value: String, errorType: PersonErrorType)

  final case class Name(value: String)
  final case class Age(value: Int)
  final case class Email(value: String)

  final case class Person(name: Name, age: Age, email: Email)
```

The `PersonErrorType` models the various errors we may encounter during the creation of a `Person`and `PersonError` captures that error with some extra information as to why it failed. We also have wrapper classes around name, age and email to differentiate them from regular `String`s.

Now we could use an `Either` type, which has a `PersonError` on the `Left` and some valid type `A` on the `Right` to represent our validations. We define the type alias `ErrorOr` to represent this:

```{.scala .scrollx}
type ErrorOr[A] = Either[PersonError, A]
```

Notice that in `ErrorOr`, the error type or the left-side of the `Either` is fixed to `PersonError` and we can only vary the success type or the right-side of the Either. This is represented by the type variable `A`. `A` could be any type.

We can imagine having three functions that represent validating our name, age and email address:

```{.scala .scrollx}
  def validateName(name: String): ErrorOr[Name] = ???
  
  def validateAge(age: String): ErrorOr[Age] = ???

  def validateEmail(email: String): ErrorOr[Email] = ???
```

We will leave their definitions for a little later.

Notice that all the functions above return an `ErrorOr` with different types for the success value.

In addition we would need some kind of validation function that combines the above functions together to return a valid `Person`:

```{.scala .scrollx}
def validatePerson(name: String, age: String, email: String): ErrorOr[Person] = ???
```

Now let's go ahead and implement the validation functions according to the following rules:

1. The supplied name must not be empty and has to start with an uppercase character.
1. The supplied age must be a number and must be between one and a hundred and twenty.
1. The supplied email address must not be empty and must have at least a single `@` character.

Following is a sample implementation of the above rules:

```{.scala .scrollx}
  def validateName(name: String): ErrorOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Right(Name(name))
    else Left(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))
  }
  
  def validateAge(age: String): ErrorOr[Age] = for {
    numericAge <- Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid))
    validAge <- { 
      if (numericAge <= 0 || numericAge > 120) Left(PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid))
      else Right(numericAge)
    }
  } yield Age(validAge)

  
  def validateEmail(email: String): ErrorOr[Email] = {
    if (email.isEmpty || !email.contains("@")) Left(PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid))
    else Right(Email(email))
  }
```

Don't worry too much about the implementation. 

Notice that we wrap the errors in a `PersonError` and put it in the `Left` constructor. Similarly we put the success values in the `Right` constructor. We also lift all success values into their wrapper types: `Name`, `Age` and `Email`.

Given the above validation implementations, how do we go about combining them to give us either a valid `Person` instance or an error of type `PersonError`?

Fortunately for us, the `Either` datatype implements both the `flatMap` and `map` methods which allows us to use a for-comprehension to sequence the validations we have:

```{.scala .scrollx}
  def validatePerson(name: String, age: String, email: String): ErrorOr[Person] = for {
    validName   <- validateName(name)
    validAge    <- validateAge(age)
    validEmail  <- validateEmail(email)
  } yield Person(validName, validAge, validEmail)
```

Now given a valid name, age and email, the `validatePerson` function returns a `Right` with a `Person` instance:

```{.scala .scrollx}
validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st")
//Right(Person(Name(Benjamin Sisko),Age(50),Email(b.sisko@dsn.st)))
```

And it returns a `Left` with the first error of type `PersonError` for an invalid person:

```{.scala .scrollx}
validatePerson("odo", "200", "odo.founder.net")
//Left(PersonError(Name is empty or does not start with an uppercase character: odo,NameInvalid))
```

Now in the above invalid example, we can see that Odo's age is not between one and a hundred and twenty and his email address does not have a single `@` character in it and would be invalid. Unfortunately `Either` bails on the first error and we don't get to see what any of the other errors are. This could get annoying if we keep getting new errors each time we run this code.

What we want is to get all the errors returned to us at once. How do we do that?

## Validated

This is where we need to lean on the [Validated](https://typelevel.org/cats/datatypes/validated.html) datatype. The `Validated` datatype also lets us represent a computation that may fail - but with one crucial difference. It accumulates any errors that may occur.

The `Validated` datatype is not defined in the Scala standard library and has to be sourced from the [Cats](https://typelevel.org/cats/) functional programming library.

A simplified definition of the `Validated` type is given below:

```{.scala .scrollx}
sealed abstract class Validated[+E, +A]
final case class Invalid[+E](e: E) extends Validated[E, Nothing]
final case class Valid[+A](a: A) extends Validated[Nothing, A]
```

We can see that the similarity to `Either` is uncanny. The `Invalid` data constructor is used to wrap some type of error while the `Valid` data constructor wraps success types. So what is it about this datatype that makes it accumulate errors in the `Invalid` case?

Before we answer that question, let's look at transforming our previous validation example to use `Validated` instead of `Either`.

Let's start off by creating a type alias called `AllErrorsOr` to accumulate our errors or return our success value:

```{.scala .scrollx}
type AllErrorsOr[A] = Validated[PersonError, A]

type ErrorOr[A]     =    Either[PersonError, A]
```

The code is almost identical to `ErrorOr`; we just swapped out `Either` for `Validated`. Let's change the return type of all our validation function to use `AllErrorsOr`:

```{.scala .scrollx}
  def validateName(name: String): AllErrorsOr[Name] = ???
  
  def validateAge(age: String): AllErrorsOr[Age] = ???

  def validateEmail(email: String): AllErrorsOr[Email] = ???

  def validatePerson(name: String, age: String, email: String): AllErrorsOr[Person] = ???
```

All we did is swap the `ErrorOr` type alias for `AllErrorsOr` and we have the definitions we need.

### Construction

Let's go ahead and implement our validators. We can change our `validateName` function quite easily:

```{.scala .scrollx}
  def validateName(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Valid(Name(name))
    else Invalid(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))
  }

```

We just simply swap `Left` for `Invalid` and `Right` for `Valid` data constructors and we are done. We could have also used the `invalid` and `valid` helper functions as well:

```{.scala .scrollx}
  def validateName(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Name(name).valid
    else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalid
  }

```

`validateAge` requires a little more work.


The `Either` implementation of `validateAge` was defined as:


```{.scala .scrollx}
  def validateAge(age: String): AllErrorsOr[Age] = for {
    numericAge <- Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid))
    validAge <- { 
      if (numericAge <= 0 || numericAge >= 120) Left(PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid))
      else Valid(numericAge)
    }
  } yield Age(validAge)
```


So we have a couple of hurdles we need to jump over:

  1. How do we convert a `Try` instance to a `Validated`?
  1. `Validated` does not implement `flatMap`; only `map`. This means we can't use a for-comprehension to sequence `Validated` instances.


We can solve the first problem by using the function `fromEither` on `Validated` that converts an `Either[A, B]` to a `Validated[A, B]`:

```{.scala .scrollx}
def fromEither[A, B](e: Either[A, B]): Validated[A, B]
```

Updating `validatedAge` we get:


```{.scala .scrollx}
  def validateAge(age: String): AllErrorsOr[Age] = {

    val ageEither: Either[PersonError, Int] = 
      Try(age.toInt).
        toEither.
        left.map(ex => PersonError(ex.getMessage, AgeInvalid))

    val validatedIntAge: Validated[PersonError, Int] = Validated.fromEither(ageEither)

    ...
  }
```

We can also use the `toValidated` to achieve the same result and IMO it's a little nicer:

```{.scala .scrollx}
  def validateAge(age: String): AllErrorsOr[Age] = {

    val validatedIntAge: Validated[PersonError, Int] Either[PersonError, Int] = 
      Try(age.toInt).
       toEither.
       left.map(ex => PersonError(ex.getMessage, AgeInvalid)).
       toValidated

    val numericAge = ??? //we need some way to get the Int age out of validatedIntAge

    val validateAge: Validated[PersonError, Age] =
      if (numericAge <= 0 || numericAge > 120) Invalid(PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid))
      else Valid(numericAge)
    }

    validatedAge
  }
```

We are almost there but we still need a way of sequencing two `Validated` instances together; where one `Validated` instance depends on the value returned from the previous `Validated`     instance.

### Combining

Let's see how we can answer our previous question:

> So what is it about this datatype that makes it accumulate errors in the `Invalid` case?

In order to combine errors in a `Validated` we need the <u>types used as errors</u> to have some behaviours such as [Functor](https://typelevel.org/cats/typeclasses/functor.html) (think something that can be mapped over) and [Semigroupal](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Semigroupal.scala) (think combining two effectful values into a pair of effectful values) or we need to <u>put the errors in a type</u> that has the above behaviours. One type that has those behaviours already defined is [NonEmptyList](https://typelevel.org/cats/datatypes/nel.html). A `NonEmptyList` is as the name suggests, a `List` that is guaranteed not to be empty (it has at least one element); which means you can safely call `head` on it among other things.

Cats already has a pre-build `Validated` type that uses `NonEmptyList` as its error type called `ValidatedNel`. The type definition of `ValidatedNel` reveals its form to us:

```{.scala .scrollx}
type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
```

We can see that `ValidatedNel` is a simple type alias for a `Validated` with a `NonEmptyList` of some error type `E` or a success type of `A`.

The `Nel` part in `ValidatedNel` refers to the `N`on`E`mpty`L`ist of the error type. There are also `NonEmptyVector` and `NonEmptyChain` variations but they encapsulate the same thing - some data structure that is not empty that is used to accumulate any errors.

Let's start off by updating our `AllErrorsOr` type alias to work with `ValidatedNel` instead of `Validated`:

```{.scala .scrollx}
//old
//type AllErrorsOr[A] = Validated[PersonError, A]

//new
type AllErrorsOr[A]   = ValidatedNel[PersonError, A]
```

To lift our invalid and valid values into a `ValidateNel` we can use the helper functions: `invalidNel` and `validNel` respectively. Here's how we'd change the `validateName` function to use `ValidatedNel` instead of `Validated`:

```{.scala .scrollx}
def validateName(name: String): AllErrorsOr[Name] = {
  if (name.headOption.exists(_.isUpper)) Name(name).validNel
  else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalidNel
  }
```

We can also use the usual `Valid` and `Invalid` data constructors but it's a little cumbersome to wrap the `Invalid` case within a `NonEmptyList`:

```{.scala .scrollx}
import cats.data.Validated._
import cats.data.NonEmptyList

def validateName(name: String): AllErrorsOr[Name] = {
  if (name.headOption.exists(_.isUpper)) Valid(Name(name))
  else Invalid(NonEmptyList.of(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid)))
}
```

So far so good. Let's try and update the `validateAge` function:

```{.scala .scrollx}
def validateAge(age: String): AllErrorsOr[Age] = {
    val numericAgeV: AllErrorsOr[Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid)).toValidatedNel
    
    ...
  }
```

We can quite easily convert an `Either` to a `ValidateNel` by calling `toValidatedNel` on it. Next let's try and update the `validateAge` function:

```{.scala .scrollx}
def validateAge(age: String): AllErrorsOr[Age] = {

    val numericAgeV: AllErrorsOr[Int] = //from before
    
    def validAgeV(numericAge: Int): AllErrorsOr[Int] = {
      if (numericAge <= 0 || numericAge > 120) PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid).invalidNel
      else numericAge.validNel
    }
  }
```

Also quite easy. The next question is how do we combine these two validations to give us an `Age`?

```{.scala .scrollx}
  numericAgeV ??? validAgeV => Age
```

#### andThen

Here's where the `andThen` method on `Validated` comes into play. It is defined as:


```{.scala .scrollx}
sealed abstract class Validated[+E, +A] {
  ...
  def andThen[EE >: E, B](f: (A) ⇒ Validated[EE, B]): Validated[EE, B]
}
```

We can see that this method definition is similar to `Either`'s `flatMap` definition, in that it sequences together two computations that may fail, returning the latter as the result:

```{.scala .scrollx}
sealed abstract class Either[+E, +A] {
  ...
  flatMap[EE >: E, B](f: (A) => Either[EE, B]): Either[EE, B]
}
```


 We can also see that the first computation has to complete, to supply the `A` before the next computation can proceed. So let's use it:

```{.scala .scrollx}
  val combineAgeV: AllErrorsOr[Int] = numericAgeV.andThen(validAgeV)
```

We can see that when we run `numericAgeV` validator we get a `AllErrorsOr[Int]`. This `Int` is then needed by the `validAgeV` function, which will then produce another `AllErrorsOr[Int]` if all goes well.

We still don't have an `Age` instance, only an `Int`. To lift the validated `Int` value into the `Age` constructor we can use the `map` function. `map` is defined on `Validated` as follows:

```{.scala .scrollx}
sealed abstract class Validated[+E, +A] {
  ...
  def map[B](f: (A) ⇒ B): Validated[E, B]
}
```

Using `map` on `combineAgeV` with the `Age` constructor gives us the final result:

```{.scala .scrollx}
  val result: AllErrorsOr[Age] = combineAgeV.map(n => Age(n))
```

The complete `validateAge` function is as follows:

```{.scala .scrollx}
def validateAge(age: String): AllErrorsOr[Age] = {
    val numericAgeV: AllErrorsOr[Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid)).toValidatedNel
    
    def validAgeV(numericAge: Int): AllErrorsOr[Int] = {
      if (numericAge <= 0 || numericAge >= 120) PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid).invalidNel
      else numericAge.validNel
    }

    numericAgeV.andThen(validAgeV).map(Age)
  }
```

Next lets convert our `validateEmail` function to use `ValidatedNel`:


```{.scala .scrollx}
def validateEmail(email: String): AllErrorsOr[Email] = {
  if (email.isEmpty || !email.contains("@")) PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid).invalidNel
  else Email(email).validNel
  }
```

That's very similar to how we modified the `validateName` function.

Now let's try and implement the `validatePerson` function. As mentioned previously, `Validated` does not implement the `flatMap` function and hence it [can't be used in a for-comprehension](https://stackoverflow.com/questions/35761043/how-to-make-your-own-for-comprehension-compliant-scala-monad).
So how can we combine these three `Validated` instances? We can use the `andThen` method again but it gets quite messy:

```{.scala .scrollx}
validateName(name).andThen(validName => 
  validateAge(age).andThen(validAge => 
    validateEmail(email).map(validEmail => 
      Person(validName, validAge, validEmail)))) //ValidatedNel[PersonError, Person]
```

If only there were a neater way of combining these validations. What we need is a function that is given each of the validated values if successful:

```{.scala .scrollx}
def validatePerson(name: String, age: String, email: String): AllErrorsOr[Person] = {
    validateName(name) ??? validateAge(age) ??? validateEmail(email) ??? (validName, validAge, validEmail) => Person(validName, validAge, validEmail)
  }
```

#### mapN

To combine two or more `ValidatedNel` instances that don't depend on each others' values we can use the `mapN` method. A simplified definition of `mapN` is given below:

```{.scala .scrollx}
//A: 1st successful value
//B: 2nd successful value
//Z: Result of applying function `f`
//E: The failure type
def mapN[Z](f: (A, B) => Z)(implicit functor: Functor[ValidatedNel[E, ?]],implicit semigroupal: Semigroupal[ValidatedNel[E,?]]): ValidatedNel[E,Z]
```

Luckily we already know that `ValidatedNel` has `Functor` and `Semigroupal` instances so we can just use `mapN`. `mapN` is specialised for combining `Validated` instances from two to twenty two parameters. Using the product capabilities which we get from `Semigroupal`, we can write a function that uses the three validated values from our validators in  the `validatePerson` function to create an `AllErrorsOr[Person]` instance.


```{.scala .scrollx}
def validatePerson(name: String, age: String, email: String): AllErrorsOr[Person] = {
    (validateName(name), validateAge(age), validateEmail(email)).mapN((validName, validAge, validEmail) => Person(validName, validAge, validateEmail))
  }
```

Now let's see what happens when all of the validations fail:

```{.scala .scrollx}
validatePerson("odo", "200", "odo.founder.net")
//Invalid(NonEmptyList(PersonError(Name is empty or does not start with an uppercase character: odo,NameInvalid), PersonError(Age must be a number between 1-120: 200,AgeInvalid), PersonError(Email address is empty or does not contain an `@` symbol: odo.founder.net,EmailInvalid)))
```

We can see that all the errors have been accumulated for us!

Let's see what happens when there are no errors:

```{.scala .scrollx}
validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st")
//Valid(Person(Name(Benjamin Sisko),Age(50),Email(b.sisko@dsn.st)))
```

We can see the output is as expected.

#### productL and productR

Now let's assume that we rewrote `validateName` such that it depended on two separate validators; one for validating a non empty name (`validateNonEmptyName`) and one for validating whether the name starts with an uppercase character (`validateStartsWithUpper`):

```{.scala .scrollx}
def validateNonEmptyName(nameString: String): AllErrorsOr[String] = 
  if (nameString.nonEmpty) nameString.validNel else PersonError(s"Name is empty", NameInvalid).invalidNel

def validateStartsWithUpper(nameString: String): AllErrorsOr[String] = 
  if (nameString.headOption.exists(_.isUpper)) nameString.validNel else PersonError(s"$nameString does not start with an uppercase character", NameInvalid).invalidNel
```

We might compose them as follows within the `validateName` function:

```{.scala .scrollx}
def validateName(name: String): AllErrorsOr[Name] = {
  validateNonEmptyName(name).andThen(_ => validateStartsWithUpper(name)).map(Name)
}
```

Notice that we are discarding the success value returned from `validateNonEmptyName` when using the `andThen` function. We can directly use the value of the argument `name` in the `validateStartsWithUpper` function. 

A more succinct way of writing this function composition is by using the `productR` function to ignore the result of the validator on the left and use the result of the validator on the right (hence the `R` in `productR`):


```{.scala .scrollx}
def validateNameWithProduct(name: String): AllErrorsOr[Name] = {
  (validateNonEmptyName(name) productR validateStartsWithUpper(name)).map(Name)
}
```

There is an inverse method to `productR` called `productL` which uses the result of the validator on the left and ignores the result of the validator on the right. The important thing to realise is that both validators still get run, but only one of the success results is returned. This is a nice shorthand when you need to ignore one of the results from a validator. The `productL` and `productR` functions are available to any [Applicative](https://typelevel.org/cats/typeclasses/applicative.html) [typeclass](https://typelevel.org/cats/typeclasses.html) (via [Apply](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Apply.scala#L40)) and since `Validated` has an `Applicative` instance we get these methods for free.

#### combine

Using `productL` and `productR` is similar to using the `combine` function available to any instance of the [Semigroup](https://typelevel.org/cats/typeclasses/semigroup.html) typeclass (think something that can combine values similar to addition or multiplication of numbers) - with one important difference: The successes are accumulated in the event of all the validators succeeding - if your success type has an instance for `Semigroup`. Some common `Semigroup`s are `String`, `List` and `NoneEmptyList`.

```{.scala .scrollx}
//captures all errors if there are any errors
validateNonEmptyName("") combine validateStartsWithUpper("joe")
res11: Validated[NonEmptyList[PersonError],String] = Invalid(NonEmptyList(PersonError(Name is empty,NameInvalid), PersonError(joe does not start with an uppercase character,NameInvalid)))

//accumulates successes when the success value is a Semigroup
validateNonEmptyName("joe1") combine validateStartsWithUpper("Joe2")
res12: Validated[NonEmptyList[PersonError],String] = Valid(joe1Joe2)
```

This is just something to be aware of so you won't get tripped up when your successes are also accumulated. 

I'm not really sure when this accumulation of successes would be useful. Send me a comment if you have a problem that this solves.

#### combineK

What if you want to choose between two or more validators, where you only want the one that passed? Think something similar to short-circuiting `Boolean` operators such as `&&` or `||`.
In this scenario you could use `combineK` which comes from the [SemigroupK](https://typelevel.org/cats/typeclasses/semigroupk.html) typeclass:

```{.scala .scrollx}
def combineK[A](x: F[A], y: F[A]): F[A] //can choose between `x` and `y` for some types of `F`
```

Here's how we could use it with our validators:

```{.scala .scrollx}
//returns the first validator that succeeds
validateNonEmptyName("joe") combineK  validateStartsWithUpper("Joe2")
res16: net.ssanj.validated.ValidatedValidations.AllErrorsOr[String] = Valid(joe)

//tries the second validator if the first fails
validateStartsWithUpper("joe1") combineK  validateStartsWithUpper("Joe2")
res17: net.ssanj.validated.ValidatedValidations.AllErrorsOr[String] = Valid(Joe2)

//accumulates errors if all validators fail
validateStartsWithUpper("joe1") combineK  validateStartsWithUpper("joe2")
res18: net.ssanj.validated.ValidatedNelValidations.AllErrorsOr[String] = Invalid(NonEmptyList(PersonError(joe1 does not start with an uppercase character,NameInvalid), PersonError(joe2 does not start with an uppercase character,NameInvalid)))
```

### Extracting

And finally you can run a `fold` on a `Validated` instance (just like you for `Option` or `Either`) to extract the value of failure or success:

```{.scala .scrollx}
sealed abstract class Validated[+E, +A] {
  ...
  def fold[B](fe: (E) ⇒ B, fa: (A) ⇒ B): B
}
```

An example usage of `fold`:

```{.scala .scrollx}
validateNonEmptyName("joe").
  fold(failure => s"you failed: $failure", success => s"you succeeded with $success")
res19: String = you succeeded with joe
```

You can also use pattern matching instead to achieve the same result:

```{.scala .scrollx}
import cats.data.Validated._

validateNonEmptyName("joe") match  {
  case Invalid(failure) => s"you failed: $failure"
  case Valid(success) => s"you succeeded with $success"
}
res22: String = you succeeded with joe
```

While it can seem like `Validated` is complex to use, in practise it is quite straight forward once you know a few rules. Hopefully this article has given you some confidence in using `Validated` the next time you need to accumulate some errors. Also be sure to read the [Cats documentation on Validated](https://typelevel.org/cats/datatypes/validated.html) to give you more insight into their usage. Sample code for this article can be found [here](https://github.com/ssanj/validated-examples)
