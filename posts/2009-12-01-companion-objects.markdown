---
title: Companion Objects
author: sanjiv sahayam
description: Using the power of Companion objects in Scala.
tags: scala
---

Scala defines singleton objects with the "object" keyword. If a singleton object shares the same name as that of a class and is defined within the same file, the singleton object is called a "companion object" and the class is called a "companion class". Companionship offers full access to each other's private variables/members. This is similar to an inner class in Java, without the need for an enclosing instance of the outer class.

So what can we do with this new found power of companion objects? Companion object can be used to create companion class instances through factory methods. You could for instance have the companion class extend a trait and make all its constructors private. Then using a factory method on the companion object, you could create an instance of the trait but abstract the companion class out of the equation. We could also provide additional constructors for a class outside the class.

Let's take an example. Say we have a trait:

```{.scala}
sealed trait Vehicle {
  protected val vin:String
}
```

Say we also have an extension:

```{.scala}
class Car(val vin:String,private val model:String) extends Vehicle
```

The vin is a protected value in the trait Vehicle and as such can only be accessed by subclasses and the model is private.

We can define a companion object of Car like so:

```{.scala}
object Car {

  def apply(vin:String, model:String) = new Car(vin, model)
  def apply(vin:String) = new Car(vin, "Jaguar")
  def apply() = new Car("N/A", "Invisble Car")
  def print(car:Car) {println("My " + car.model + "'s vin is: " + car.vin) }
}
```

By using the companion object we can instantiate Car with:

```{.scala}
Car() or Car("1234567") or Car("9876543", "Ford")
```

The companion object has given us extra constructors for the Car object for free....or so it seems. We have also rid ourselves of the "new" key word that you would normally need to create an instance of a class. Case classes are the exception and now Car behaves a little like a case class thanks to the companion object.

In the print method we see that the companion object does access the protected value "vin" (although it does not extend Vehicle) and the private value "model". Thus we can define methods that need to access the private state of the companion class within the companion object.

One Caveat to keep in mind is that the companions are explicitly linked by name and name alone. Attempting the following will fail although it may "feel" right:

```{.scala}
def print2(vehicle:Vehicle) {println("My vehicle's vin is: " + vehicle.vin) }
```

We get the following compilation error:

```
error vin cannot be accessed from blog.this.Vehicle
```

The reason for this failure is that we are trying to access a value from the Vehicle trait and not the Car class. The companion object Car in this case only has access to the state of the companion class Car.

Here's the full example:

```{.scala}
package blog

import Car._

object Companionship extends App {
    print(Car())
    print(Car("1234567"))
    print(Car("9876543", "Ford"))
}

sealed trait Vehicle {
  protected val vin:String
}

class Car(val vin:String,private val model:String) extends Vehicle

object Car {
  def apply(vin:String, model:String) = new Car(vin, model)
  def apply(vin:String) = new Car(vin, "Jaguar")
  def apply() = new Car("N/A", "Invisble Car")
  def print(car:Car) {println("My " + car.model + "'s vin is: " + car.vin) }
  //def print2(vehicle:Vehicle) {println("My vehicle's vin is: " + vehicle.vin) }
}
```