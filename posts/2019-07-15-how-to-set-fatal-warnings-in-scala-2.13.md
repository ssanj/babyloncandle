---
title: How to set Fatal Warnings in Scala 2.13?
author: sanjiv sahayam
description: The old -Xfatal-warnings compiler option in 2.12 no longer works in 2.13. Here's how to enable the same functionality in scala 2.13.
tags: scala
comments: true
---

With the introduction of Scala 2.13, a bunch of commonly used compiler options have been moved. 

`-Xfatal-warnings` has been replaced with `-Werror`. In fact a lot of *warning* options which were previously `-Ywarn` have been moved to `-W` such as `-Ywarn-dead-code` which is now `-Wdead-code`.

Here's a full list of `-W` options:

 - **-Wdead-code**                    Warn when dead code is identified.
 - **-Wextra-implicit**               Warn when more than one implicit parameter section is defined.
 - **-Wmacros:**\<mode>                Enable lint warnings on macro expansions. Default: `before`, `help` to list choices.
 - **-Wnumeric-widen**                Warn when numerics are widened.
 - **-Woctal-literal**                Warn on obsolete octal syntax.
 - **-Wself-implicit**                Warn when an implicit resolves to an enclosing self-definition.
 - **-Wunused:imports**               Warn if an import selector is not referenced.
 - **-Wunused:patvars**               Warn if a variable bound in a pattern is unused.
 - **-Wunused:privates**              Warn if a private member is unused.
 - **-Wunused:locals**                Warn if a local definition is unused.
 - **-Wunused:explicits**             Warn if an explicit parameter is unused.
 - **-Wunused:implicits**             Warn if an implicit parameter is unused.
 - **-Wunused:params**                Enable -Wunused:explicits,implicits.
 - **-Wunused:linted**                -Xlint:unused.
 - **-Wvalue-discard**                Warn when non-Unit expression results are unused.

A lot of `-Ywarn` options have also now become `-Xlint`options such as `-Ywarn-infer-any` which is now `-Xlint:infer-any`. Here's a full list of the `-Xlint` options:

 - **-Xlint:adapted-args**            Warn if an argument list is modified to match the receiver.
 - **-Xlint:nullary-unit**            Warn when nullary methods return Unit.
 - **-Xlint:inaccessible**            Warn about inaccessible types in method signatures.
 - **-Xlint:nullary-override**        Warn when non-nullary `def f()' overrides nullary `def f'.
 - **-Xlint:infer-any**               Warn when a type argument is inferred to be `Any`.
 - **-Xlint:missing-interpolator**    A string literal appears to be missing an interpolator id.
 - **-Xlint:doc-detached**            A Scaladoc comment appears to be detached from its element.
 - **-Xlint:private-shadow**          A private field (or class parameter) shadows a superclass field.
 - **-Xlint:type-parameter-shadow**   A local type parameter shadows a type already in scope.
 - **-Xlint:poly-implicit-overload**  Parameterized overloaded implicit methods are not visible as view bounds.
 - **-Xlint:option-implicit**         Option.apply used implicit view.
 - **-Xlint:delayedinit-select**      Selecting member of DelayedInit.
 - **-Xlint:package-object-classes**  Class or object defined in package object.
 - **-Xlint:stars-align**             Pattern sequence wildcard must align with sequence component.
 - **-Xlint:constant**                Evaluation of a constant arithmetic expression results in an error.
 - **-Xlint:unused**                  Enable -Ywarn-unused:imports,privates,locals,implicits.
 - **-Xlint:nonlocal-return**         A return statement used an exception for flow control.
 - **-Xlint:implicit-not-found**      Check @implicitNotFound and @implicitAmbiguous messages.
 - **-Xlint:serial**                  @SerialVersionUID on traits and non-serializable classes.
 - **-Xlint:valpattern**              Enable pattern checks in val definitions.
 - **-Xlint:eta-zero**                Warn on eta-expansion (rather than auto-application) of zero-ary method.
 - **-Xlint:eta-sam**                 Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.
 - **-Xlint:deprecation**             Enable linted deprecations.

For a full list of options checkout [scalac 2.13 options and flags](https://sanj.ink/posts/2019-06-14-scalac-2.13-options-and-flags.html)