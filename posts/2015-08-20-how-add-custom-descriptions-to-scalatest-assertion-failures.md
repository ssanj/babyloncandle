---
title: How to Add Custom Descriptions to ScalaTest Assertion Failures
author: sanjiv sahayam
description: How to append or prepend a custom description to as ScalaTest assertion failure.
tags: scala, scalatest
comments: true
---

[This had me stumped for a while](http://stackoverflow.com/questions/6451530/how-to-show-custom-failure-messages-in-scalatest). Here's a few ways to do it:


## Using assert ##

```{.command .scrollx}
assert(your Boolean assertion, "your description")
```
_note_: assert takes in a Boolean assertion not a matcher assertion.

Example:

```{.command .scrollx}
assert(Seq("something").size == 2, "- the size should be one")
```

Example output:

```{.terminal .scrollx}
org.scalatest.exceptions.TestFailedException: List("something") had size 1 instead of expected size 2 - the size should be one
```


## Using WithClue ##

```{.command}
withClue("Your prefix") { your assertion }
```

Example:

```{.command .scrollx}
withClue("Sequence size - ") { Seq("something") should have size 2 }
```

Example output:

```{.terminal .scrollx}
org.scalatest.exceptions.TestFailedException: Sequence size - List("something") had size 1 instead of expected size 2
```

[or if you want a suffix, mix in AppendedClues](http://stackoverflow.com/questions/28307155/is-there-syntax-for-adding-a-clue-to-scalatest-matchers):

```{.command .scrollx}
class TestSuite extends FlatSpec with Matchers with AppendedClues {
  your assertion withClue("your suffix")
}
```

Example:

```{.command .scrollx}
class TestSuite extends FlatSpec with Matchers with AppendedClues {
  3 should equal(4) withClue("expecting a header row and 3 rows of data")
}
```

Example output:

```{.terminal .scrollx}
org.scalatest.exceptions.TestFailedException: 3 did not equal 4 expecting a header row and 3 rows of data
```