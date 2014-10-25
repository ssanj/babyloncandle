---
title: Semigroup
author: sanjiv sahayam
tags: fp, scala
---

I recently came across something called a "Semigroup" while browsing [scalaz](http://github.com/nkpart/scalaz) code. I was actually looking for an example of a Monoid, but found that the scalaz implementation used Semigroup in the implementation of Monoids.

## What is a Semigroup?

A Semigroup is an abstraction of a binary operation where the operands and the result are of the same type.

A functional definition of a Semigroup in scala could be:


```{.scala}
def op[T](t1:T, t2:T) : T
```

The abstraction has to adhere to 2 rules to be considered a Semigroup.

### Semigroup Rules

1. The result of the binary operation should be of the same type as that of the two operands.
2. The binary operation should be associative.

__Rule 1__, defines a "closed set", or a closure - which is an operation on elements of a set, that always yield another element of the same set. An example set would be natural numbers. If the operation to abstract was addition, we would get op(1,2) = 3 -> 1+2 = 3, where both 1,2 and 3 are all members of the same set (natural numbers).

__Rule 2__, defines that if we had a sequence of the same operation, the associativity of the operations would be not change the final result. For example taking the operation as multiplication, we would get op(2, op(4, 5)) = op(op(2,4), 5) -> 2*(4*5) = (2*4)*5

The division operation is not a natural number Semigroup, because the result of dividing through 2 natural numbers may be a real number. Eg. 5/2 -> 2.5. This breaks rule 1, since 2.5 is not a member of the natural number set.

The division operation is also not a real number Semigroup because it is not an associative operation and hence breaks rule 2. Given op(op(27.0,3.0), 2.0) != op(27.0, op(3.0,2.0) -> (27.0/3.0)/2.0 != 27.0/(3.0/2.0)


## Some Examples

Here's an implementation of the plus operator:

```{.scala}
trait Semigroup[T] {
  def op(t1:T, t2:T) : T
}

def plus = new Semigroup[Int] { def op(t1:Int, t2:Int) = t1 + t2 }
}
```

Scalaz defines a Semigroup as the following trait:

```{.scala}
trait Semigroup[S] {
 def append(s1: S, s2: => S): S
}
```

and some example implementations from scalaz:

```{.scala}
trait Semigroups {
  def semigroup[S](f: (S, => S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }
}

implicit def IntSemigroup: Semigroup[Int] = semigroup(_ + _)
implicit def StringSemigroup: Semigroup[String] = semigroup(_ + _)
```
##  Why use Semigroups?

When using any Semigroup implementation, we know that the 2 rules apply: closure and associativity. This gives us knowledge on how to use the API correctly.

For example since we know that Semigroups are associative, we don't have to wonder about what will happen if the associativity of a statement changes. We also know by closure that the types returned are within the same domain as the operands.

Semigroups can also be used to implement other concepts such as Monoids and thus having a Semigroup implementation helps you create further functional concepts.

Any feedback is much appreciated.
