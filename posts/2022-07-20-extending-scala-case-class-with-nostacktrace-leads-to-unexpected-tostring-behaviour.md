---
title: Extending Scala Case Class With NoStackTrace Leads To Unexpected toString Behaviour
author: sanjiv sahayam
description: Extending a Scala case class with NoStackTrace leads to unexpected toString behaviour
tags: scala, java
comments: true
---

Say you had an ADT similar to this:

```{.scala .scrollx}
import scala.util.control.NoStackTrace

sealed trait MyError extends NoStackTrace
final case class MyError1(message: String) extends  MyError
final case class MyError2(message: String) extends  MyError
```

This might seem weird to some. Why are we extending `NoStackTrace` ? This allows us to use the `MyError` as a return value through something like an `Either`:

```{.scala .scrollx}
def sanitiseInput(value: String): Either[MyError, ValidInput]
```

We can also use it as an error that can be thrown or raised into [cats.IO](https://typelevel.org/cats-effect/), [fs2.Stream](https://fs2.io/#/), [Monix.eval.Task](https://monix.io/) or equivalent:

```{.scala .scrollx}
object IO {
  def raiseError[A](t: Throwable): IO[A] //`t` has to extend Throwable if we want to use this function.

  //other functions
}
```

You can also read about some related ideas in [Make error ADTs subtypes of Exception](https://nrinaudo.github.io/scala-best-practices/adts/errors_extend_exception.html).

Now if we use `MyError` in a test:

```{.scala .scrollx}
import scala.util.control.NoStackTrace

object MyErrorSuite extends weaver.FunSuite {

sealed trait MyError extends NoStackTrace
final case class MyError1(message: String) extends  MyError
final case class MyError2(message: String) extends  MyError

  test("error message") {
    expect.same(MyError1("error1"), MyError2("error2")) //this is an error
  }

}

```

the test output from [Weaver-Test](https://disneystreaming.github.io/weaver-test/) gets truncated somewhat:

```{.scala .scrollx}
[info] com.example.validation.extra.MyErrorSuite
[info] - error message 30ms
[info] *************FAILURES**************
[info] com.example.validation.extra.MyErrorSuite
[error] - error message 30ms
[error]   Values not equal: (src/test/scala/com/example/validation/extra/MyErrorSuite.scala:12)
[error]
[error]   com.example.validation.extra.MyErrorSuite$[MyError1]  |  com.example.validation.extra.MyErrorSuite$[MyError2]
```

All we get are the class names returned in the diff:

```{.terminal .scrollx}
com.example.validation.extra.MyErrorSuite$[MyError1]  |  com.example.validation.extra.MyErrorSuite$[MyError2]
```

The diff we expected was something like:

```{.terminal .scrollx}
 [MyError1]([error1]) | [MyError2]([error2]) //we can see that the class and error messages are different
```

When you create a case class it generates a `toString` implementation of the form: `ClassName(field1Value, field2Value, ....)`.

So why are we loosing our `toString` implementation?

## Cause

Let's try a simpler example in the REPL:

```{.scala .scrollx}
case class MyError1(message: String)

scala> MyError1("Oh noes")
val res35: MyError1 = MyError1(Oh noes) //"Oh noes" is output
```

We can see that we do get the contents of all fields of the case class written out.

Let's try extending `NoStackTrace` and see if it makes a difference:

```{.scala .scrollx}
import scala.util.control.NoStackTrace

case class MyError2(message: String) extends NoStackTrace

scala> MyError2("Oh noes")
val res36: MyError2 = MyError2 //no message output
```

We can see that although the class name is output the contents of the `message` field has not.

Interesting. This seems to be the cause of our issue in the test.

It turns out a case class doesn't generate a `toString` method (and other implementations such has hashCode etc) if you **already** have a custom implementation for that method in a super type.


[It's not a bug, it's a feature](https://github.com/scala/bug/issues/1549).

![Nope](https://media.giphy.com/media/l41YqG5h9gIWrcSBy/giphy.gif)

So where does our `MyError2` class get a custom `toString` implementation from?

Lets have a look at the `NoStackTrace` class, since `MyError2` extends that:

```{.scala .scrollx}
trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable =
    if (NoStackTrace.noSuppression) super.fillInStackTrace()
    else this

  ...
}
```

No `toString` implementation here. Let's follow the inheritance trail to `java.lang.Throwable`. Here, we see that it [does](https://github.com/EricChows/JDK-1.8-sourcecode/blob/d34a693ffa76fdbb0fea022b5bb7bfbd2c6df0bd/java/lang/Throwable.java#L390) have a custom `toString` implementation:

```{.java .scrollx}
public String toString() {
    String s = getClass().getName();
    String message = getLocalizedMessage();
    return (message != null) ? (s + ": " + message) : s;
}
```

From the above implementation we can deduce that for `MyError2` the `getLocalizedMessage` method returns `null` because we only get back the class name `s` as output: (`MyError2`) as opposed to: `MyError2: message`.

Let's follow along to `getLocalizedMessage` to see how `message` is calculated:

```{.java .scrollx}
public String getLocalizedMessage() {
    return getMessage();
}
```

and also to `getMessage`:

```{.java .scrollx}
public String getMessage() {
    return detailMessage;
}
```

The `detailMessage` field is set through the many of the constructor methods for `Throwable`:

```{.java .scrollx}
public Throwable(String message) {
    fillInStackTrace();
    detailMessage = message; //set
}

//or

public Throwable(String message, Throwable cause) {
    fillInStackTrace();
    detailMessage = message; //set
    this.cause = cause;
}

//or

public Throwable(Throwable cause) {
    fillInStackTrace();
    detailMessage = (cause==null ? null : cause.toString());  //set
    this.cause = cause;
}

//or

protected Throwable(String message, Throwable cause,
                    boolean enableSuppression,
                    boolean writableStackTrace) {
    if (writableStackTrace) {
        fillInStackTrace();
    } else {
        stackTrace = null;
    }
    detailMessage = message;  //set
    this.cause = cause;
    if (!enableSuppression)
        suppressedExceptions = null;
}
```

Since we have a field named `message` and not `detailMessage`, we don't really override the value used by `Throwable` to generate its `toString` implementation.

## Workarounds

If we renamed our `message` field in `MyError2` to `detailMessage` we should be able to get our `toString` implementation working:

```{.scala .scrollx}
import scala.util.control.NoStackTrace

case class MyError2(detailMessage: String) extends NoStackTrace

scala> MyError2("Oh noes")
val res37: MyError2 = MyError2 //Doesn't work
```

Wow! That didn't work either. Why though?


If we look at the definition of the `detailMessage` field on `java.lang.Throwable` we see that it's **private**:

```{.java .scrollx}
private String detailMessage;
```

This means we can't override it from a sub class. Boo!

From our previous investigation we can see that all we need to do is override either `getLocalizedMessage` or `getMessage` or `toString` which are all **public**:

```
public String toString() {
    String s = getClass().getName();
    String message = getLocalizedMessage(); //message calculated from here
    return (message != null) ? (s + ": " + message) : s;
}


public String getLocalizedMessage() {
    return getMessage(); //message content retrieved from here
}


public String getMessage() {
    return detailMessage; //message content
}
```

### Override getMessage or getLocalizedMessage

By overriding `getMessage` or `getLocalizedMessage` in our case class, we can get some form of `toString`-ery happening. While this is not ideal, it "works".

```{.scala .scrollx}
import scala.util.control.NoStackTrace

case class MyError2(override val getMessage: String) extends NoStackTrace

scala> MyError2("Oh noes")
val res38: MyError2 = MyError2: Oh noes //We did it!
```



### Override toString

If you want a more case classy `toString` implementation, you're going to have to do it yourself:

```{.scala .scrollx}
case class MyError2(message: String) extends NoStackTrace {
  override def toString: String = s"MyError2($message)"
}

scala> MyError2("Oh noes")
val res39: MyError2 = MyError2(Oh noes) //we have case classiness
```

Now we can get our test to fail with a better error message:

```{.scala .scrollx}
import scala.util.control.NoStackTrace

object MyErrorSuiteTake2 extends weaver.FunSuite {

sealed trait MyError extends NoStackTrace {
  val message: String

  override def toString: String = {
    val className = getClass.getName
    s"$className($message)"
   }
}

final case class MyError1(message: String) extends  MyError
final case class MyError2(message: String) extends  MyError

  test("error message") {
    expect.same(MyError1("error1"), MyError2("error2"))
  }
}
```


Which results in:

```{.terminal .scrollx}
info] com.example.validation.extra.MyErrorSuiteTake2
[error] - error message 38ms
[error]   Values not equal: (src/test/scala/com/example/validation/extra/MyErrorSuiteTake2.scala:20)
[error]
[error]   com.example.validation.extra.MyErrorSuiteTake2$[MyError1]([error1])  |  com.example.validation.extra.MyErrorSuiteTake2$[MyError2]([error2])

```

All this seems a bit tedious... as does extending the `Exception` hierarchy. If you do decide to go this route, hopefully this will help you stave off at least one of the issues with extending `java.lang.Throwable` and friends.
