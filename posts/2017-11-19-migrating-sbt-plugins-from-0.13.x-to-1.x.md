---
title: Migrating SBT Plugins from 0.13.x to 1.x
author: sanjiv sahayam
description: A simple process outline on how to migrate from SBT 0.13.x to 1.x.
tags: sbt, scala
comments: true
---

Migrating SBT 0.13.x plugins to SBT 1.x can be a little confusing. Here I try to document the steps I followed to migrate two plugins from 0.13.x to 1.x.

## Cross-Building

The first thing you need to do is bump your SBT version to 0.13.16 or later, as this lets you cross-compile your plugin between 0.13.x and 1.x. Update your __project/build.properties__ file with the following:

```{.scala .scrollx}
sbt.version=0.13.16
```

Next we need to update our __build.sbt__ file with some new settings.

Change your scala version to 2.12.3:

```{.scala .scrollx}
scalaVersion := "2.12.3"
```

Set the global version of SBT to 1.0.3:

```{.scala .scrollx}
sbtVersion in Global := "1.0.3"
```

Add a __scalaCompilerBridgeSource__ setting:

```{.scala .scrollx}
scalaCompilerBridgeSource := {
  val sv = appConfiguration.value.provider.id.version
  ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
}
```

Add cross-building versions you require:

```{.scala .scrollx}
crossSbtVersions := Vector("0.13.16", "1.0.3")
```

The full list of changes to __build.sbt__ are:

```{.scala .scrollx}
scalaVersion := "2.12.3"

sbtVersion in Global := "1.0.3"

scalaCompilerBridgeSource := {
  val sv = appConfiguration.value.provider.id.version
  ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
}

crossSbtVersions := Vector("0.13.16", "1.0.3")
```

_Feel free to change the Scala version to the latest 2.12.x version and SBT to the latest 1.x version_ when applying these settings.

Run a clean test across all versions through SBT:

```{.command .scrollx}
^ ;clean;test
```

The __^__ will run all SBT tasks across all versions defined in __crossSbtVersions__. This new syntax is documented [here](http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#sbt-cross-building)

## Fixing issues

Here are some of the issues I ran across and their solutions:

1. For any SBT plugins that your plugin depends on, you need to get the latest version of the plugin that supports 1.x. If the plugin doesn't support 1.x, you either can't update your plugin to 1.x or find another plugin or a workaround to get the same functionality without including that plugin.

2. [sbt.Process](http://www.scala-sbt.org/0.13.15/api/index.html#sbt.Process) does not exist in SBT 1.x. This has been removed and you can simply use the Scala's [sys.process.Process](http://www.scala-lang.org/api/2.12.3/scala/sys/process/Process.html) instead.

3. [sbt.BuildStructure](http://www.scala-sbt.org/0.13.15/api/index.html#sbt.BuildStructure) does not exist in SBT 1.x. This has been moved to [sbt.internal.BuildStructure](http://www.scala-sbt.org/1.0.4/api/sbt/internal/BuildStructure.html).

4. There were a lot of deprecated warnings. Please see [Migrating from SBT 0.13.x](http://www.scala-sbt.org/1.x/docs/Migrating-from-sbt-013x.html) on how to fix them all.

5. Your plugin is not an [AutoPlugin](http://www.scala-sbt.org/0.13.15/api/index.html#sbt.AutoPlugin) and extends [sbt.Plugin](http://www.scala-sbt.org/0.13.15/api/index.html#sbt.Plugin) instead. SBT 0.13.15 onwards recommends the [creation of an AutoPlugin](http://www.scala-sbt.org/0.13/docs/Plugins.html#Creating+an+auto+plugin) instead of Plugin.

If you have a simple plugin you can do the following:

a. Extend AutoPlugin instead of Plugin
a. Add an __autoImport__ module under AutoPlugin for any settings you want automatically   imported into SBT.
a. Add the following settings:

```{.scala .scrollx}
 override def requires = plugins.JvmPlugin
 override def trigger = allRequirements
```

Please see [sbt-ctags SBT 1x Compatibility PR](https://github.com/ceedubs/sbt-ctags/pull/20/files) for an example.

6. [Scripted](http://www.scala-sbt.org/0.13/docs/Testing-sbt-plugins.html#step+2%3A+scripted-plugin) failures. The scripted plugin might get different paths to dependencies than it did previously. Simply fix this with the new paths required.

## Publish

```{.command .scrollx}
^ publish
```

Should publish both the __sbt/0.13__ and __sbt/1.0__ versions of your plugin. If you use a service like [Bintray](https://bintray.com) you'll find both versions uploaded when you publish to Bintray.
