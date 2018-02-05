---
title: How to Run Specific Tests through Stack with Tasty in Haskell
author: sanjiv sahayam
description: How to run a specific test through Tasty when used through Stack
tags: haskell, stack
comments: true
---

Something I've become accustomed to while coding in Scala is being able to run a [subset of tests](https://sanj.ink/posts/2015-08-02-run-a-test-method-of-a-test-class-from-sbt.html) through SBT. And while the same functionality is possible through [Tasty](https://hackage.haskell.org/package/tasty), I found it a little difficult to use through Stack.

Now stack allows you to send through parameters to your testing framework with the following options:

```{.terminal .scrollx}
--ta,--test-arguments TEST_ARGS Arguments passed in to the test suite program
```

So given a full test suite of:

```{.terminal .scrollx}
  RM
    exits from home screen:       OK
    handles invalid query syntax: OK
    handle valid query:           OK
    handle invalid index:         OK
    handle invalid action:        OK
    go home from search:          OK
    quit from search:             OK
    search without results:       OK
```

Using plain Tasty, you can use the -p argument to run tests that match a pattern:

```{.terminal .scrollx}
-p,--pattern ARG         Select only tests that match pattern
```

Combining the two options, I can choose to run only the *quit from search* test with:

```{.terminal .scrollx}
stack test --ta '-p "quit from search"'
```

which results in running only that specific test:

```{.terminal .scrollx}
  RM
    quit from search: OK

All 1 tests passed (0.00s)
```

As long as the string you pass to Tasty is specific, you can target tests at different levels (TestGroup, TestCase etc).

For example to only target the *CommandParser* test group I could use:

```{.terminal .scrollx}
stack test --ta '-p "CommandParser"'
```

which results in running all the tests with the the *CommandParser* test group:

```{.terminal .scrollx}
  CommandParser
    matchValue parser should match one of *?^:                  OK
    matchValue parser should not match other chars:             OK
      +++ OK, passed 100 tests.
    matchType parser should match format: "> [*?^]":            OK
    query parser should parse a valid query with matches:       OK
    query parser should parse a valid query with only commands: OK
    query parser should parse all valid queries:                OK
      +++ OK, passed 100 tests.

All 6 tests passed (0.00s)
```

And to target a specific test case within a Test Group I could use the

> testGroupName/testName

format.

For example to run the _matchValue parser should not match other chars_ test within the *CommandParser* test group, I could use:

```{.terminal .scrollx}
stack test --ta '-p "CommandParser/matchValue parser should not match other chars"'
```

which results in:

```{.terminal .scrollx}
  CommandParser
    matchValue parser should not match other chars: OK
      +++ OK, passed 100 tests.

All 1 tests passed (0.01s)
```

While this great, the version of Tasty I'm using ([0.11.3](https://hackage.haskell.org/package/tasty-0.11.0.3)) has support for additional patterns:

> An optional prefixed bang ! negates the pattern.

> If the pattern ends with a slash, it is removed for the purpose of the following description, but it would only find a match with a test group. In other words, foo/ will match a group called foo and any tests underneath it, but will not match a regular test foo.

> If the pattern does not contain a slash /, the framework checks for a match against any single component of the path.
> Otherwise, the pattern is treated as a glob, where:

> The wildcard * matches anything within a single path component (i.e. foo but not foo/bar).

> Two wildcards ** matches anything (i.e. foo and foo/bar).

> Anything else matches exactly that text in the path (i.e. foo would only match a component of the test path called foo (or a substring of that form).

> For example, group/*1 matches group/test1 but not group/subgroup/test1, whereas both examples would be matched by group/**1. A leading slash matches the beginning of the test path; for example, /test* matches test1 but not group/test1.

Newer versions of Tasty such as [1.0](https://hackage.haskell.org/package/tasty-1.0) onward support even more advanced syntax.

Now that's pretty neat!