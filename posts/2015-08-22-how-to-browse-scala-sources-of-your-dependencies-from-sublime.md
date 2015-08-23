---
title: How to Browse Scala Sources of your Dependencies from Sublime
author: sanjiv sahayam
description: How to browse sources of external dependencies of your Scala project through Sublime Text.
tags: sbt, scala, sublime, sublimeide
comments: true
---

A feature I desperately needed in Sublime [since my migration from Intellij](http://sanj.ink/posts/2015-07-15-using-sublime-for-scala-development.html) was the ability to browse the sources of my project's dependencies. Without this ability you are basically relegated to using the Scaladocs and Google/SO for all your information. Not good.

There is a way for you to do this in Sublime using [Ctags](http://ctags.sourceforge.net). 

<iframe src="https://player.vimeo.com/video/137045055" width="800" height="501" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe> <p><a href="https://vimeo.com/137045055">Browsing Scala dependency sources through Sublime and Ctags</a> from <a href="https://vimeo.com/user4351020">ssanj</a> on <a href="https://vimeo.com">Vimeo</a>.</p>

So what are Ctags?

> Ctags generates an index (or tag) file of language objects found in source files that allows these items to be quickly and easily located by a text editor or other utility.

Cool. That sounds promising! Unfortunately [Scala is not one of the supported languages](http://ctags.sourceforge.net/languages.html).

[The sbt-ctags plugin](https://github.com/ceedubs/sbt-ctags) gives you this Scala support for Ctags through sbt.

> SBT ctags is an SBT plugin that will generate ctags for your Scala project.

> It unzips the source jars for your project dependencies and generates ctags for these dependency sources in addition to the Scala/Java source of your project itself.

In addition to downloading all the sources for your dependencies the sbt-ctags plugin also creates the .tags file that is used by any Ctags-aware editor.

Fortunately Sublime has Ctags support through the [Sublime Ctags plugin](https://packagecontrol.io/packages/CTags). 

Now we have all the pieces we need to get Ctags working with Scala and Sublime. Yay!

# Installation #

1. Install Exuberant tags.

On a Mac you can do it with brew:

```{.command .scrollx}
brew install ctags
```

For additional OS installation options checkout the [Sublime Ctags page](https://packagecontrol.io/packages/CTags) or the [Exuberant Tags page](http://ctags.sourceforge.net).

2. Install the SBT Ctags plugin globally.

Ctags support is something we will need on every project. To do this we need to add it to the global plugins configurations so that it will be available across all our projects. 

Add the [sbt-ctags plugin](https://github.com/ceedubs/sbt-ctags) to your global sbt configuration file. Global plugin dependencies should be added to the __plugins.sbt__ located at __~/.sbt/0.13/plugins__:

```{.scala}
addSbtPlugin("net.ceedubs" %% "sbt-ctags" % "0.1.0")
```

note: _you may need to create the above file if it doesn't exist._

The sbt-ctags plugin downloads the sources for your project dependencies into a subdirectory under your project's target directory. One problem with that is that every time you run an ```sbt clean``` your tags get deleted. Not very useful.

We want our tags in a directory that is not under the target directory. To do that globally we have to create a global plugin.

Create the global plugin under __~/.sbt/0.13/plugins__ in a file named __CustomCtagsSrcDir.scala__ with the following contents:

```{.scala}
import sbt._
import Keys._
import net.ceedubs.sbtctags.CtagsKeys._

object CustomCtagsSrcDir extends Plugin {
  override def settings = Seq(
    dependencySrcUnzipDir := baseDirectory.value / ".ctags_srcs"
  )
}
```

In the above plugin the dependency sources are written to a directory named __.ctags_srcs__ under your project's root directory.

Now in any sbt project you can run the following to generate your ctags:

```{.command .scrollx}
sbt genCtags
```

The above incantation will download all your project dependency sources to the __.ctags_srcs__ directory and create a __.tags__ file in the project root directory. 

3. Create a Scala .tags configuration file to enable Ctags for Scala

[Create a ~/.tags file](https://github.com/ceedubs/sbt-ctags#user-content-configuring-ctags) to configure Ctags to  index Scala files:

```{.command .scrollx}
--langdef=scala
--langmap=scala:.scala
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*class[ \t]+([a-zA-Z0-9_]+)/\4/c,classes/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*object[ \t]+([a-zA-Z0-9_]+)/\4/c,objects/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*case class[ \t]+([a-zA-Z0-9_]+)/\4/c,case classes/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*case object[ \t]+([a-zA-Z0-9_]+)/\4/c,case objects/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*trait[ \t]+([a-zA-Z0-9_]+)/\4/t,traits/
--regex-scala=/^[ \t]*type[ \t]+([a-zA-Z0-9_]+)/\1/T,types/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*def[ \t]+([a-zA-Z0-9_]+)/\3/m,methods/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*val[ \t]+([a-zA-Z0-9_]+)/\3/l,constants/
--regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*var[ \t]+([a-zA-Z0-9_]+)/\3/l,variables/
--regex-scala=/^[ \t]*package[ \t]+([a-zA-Z0-9_.]+)/\1/p,packages/
```

4. Install the Sublime Ctags Plugin

You can install the Sublime Ctags plugin from [Package Control](https://packagecontrol.io/packages/CTags) or manually from the [repository](https://github.com/SublimeText/CTags).

The Sublime Ctags plugin will use the __.tags__ index file generated in your project root directory with the Ctags executable to lookup the symbols you need.

# Usage #

1. On any new project or when you add a new dependency, run:

```{.command .scrollx}
sbt genCtags
```

2. Within Sublime put your cursor on a method or member and choose "Goto Definition" from the context menu.

![Goto Definition](/images/sublime_go_to_definition_ctags.jpg)

# Customisation #

To define a shortcut for the "Goto Definition" command add a binding to your user key bindings file. 

Edit your user key bindings file by clicking on __Sublime Text__ > __Preferences__ > __Key Bindings - User__:

```{.command .scrollx}
  { "keys": ["f4"], "command": "goto_definition" }
```

The above binding maps __F4__ as the key to browse your sources. You can change this mapping to whatever you like.