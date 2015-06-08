---
title: Show Source Changes for Each Git Commit
author: sanjiv sahayam
description: How to display patches for each commit to a Git repository.
tags: git
comments: true
---

An easy way to display source changes per Git commit is:

```{.command}
git log -p -num_commits
```

Eg:

```{.command}
git log -p -2
```

Running this on the [Scalaz](https://github.com/scalaz/scalaz) project gives us the source updates across the last 2 commits:

```{.terminal .scrollx}
commit 93c6e378d705205ec9afbd64799cf9ff068cf9d5
Author: xuwei-k <6b656e6a69@gmail.com>
Date:   Fri May 22 00:39:00 2015 +0900

    reduce jar size

diff --git a/core/src/main/scala/scalaz/Coyoneda.scala b/core/src/main/scala/scalaz/Coyoneda.scala
index a2c9b57..dd86d12 100644
--- a/core/src/main/scala/scalaz/Coyoneda.scala
+++ b/core/src/main/scala/scalaz/Coyoneda.scala
@@ -233,7 +233,7 @@ private trait CoyonedaFoldable[F[_]] extends Foldable[Coyoneda[F, ?]] {
   def F: Foldable[F]

   override final def foldMap[A, B: Monoid](fa: Coyoneda[F, A])(f: A => B) =
-    F.foldMap(fa.fi)(i => f(fa.k(i)))
+    F.foldMap(fa.fi)(fa.k andThen f)
   override final def foldRight[A, B](fa: Coyoneda[F, A], z: => B)(f: (A, => B) => B) =
     F.foldRight(fa.fi, z)((i, b) => f(fa.k(i), b))
   override final def foldLeft[A, B](fa: Coyoneda[F, A], z: B)(f: (B, A) => B) =
@@ -244,7 +244,7 @@ private abstract class CoyonedaFoldable1[F[_]] extends Foldable1[Coyoneda[F, ?]]
   def F: Foldable1[F]

   override final def foldMap1[A, B: Semigroup](fa: Coyoneda[F, A])(f: A => B) =
-    F.foldMap1(fa.fi)(i => f(fa.k(i)))
+    F.foldMap1(fa.fi)(fa.k andThen f)
   override final def foldMapRight1[A, B](fa: Coyoneda[F, A])(z: A => B)(f: (A, => B) => B) =
     F.foldMapRight1(fa.fi)(i => z(fa.k(i)))((i, b) => f(fa.k(i), b))
 }

commit a8bb05f8d024c94b94b96e8a83912e66fa84269a
Author: xuwei-k <6b656e6a69@gmail.com>
Date:   Thu May 21 19:22:28 2015 +0900

    kind-projector 0.5.4

diff --git a/project/build.scala b/project/build.scala
index 7473615..74ebbd9 100644
--- a/project/build.scala
+++ b/project/build.scala
@@ -168,7 +168,7 @@ object build extends Build {
       ),
     // kind-projector plugin
     resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
-    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.5.3"  cross CrossVersion.binary)
+    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.5.4" cross CrossVersion.binary)
   ) ++ osgiSettings ++ Seq[Sett](
     OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
   )
```