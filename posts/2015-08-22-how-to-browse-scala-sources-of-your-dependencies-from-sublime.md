---
title: How to Browse Scala Sources of your Dependencies from Sublime
author: sanjiv sahayam
description: How to browse sources of external dependencies of your Scala project through Sublime Text.
tags: ctags, sbt, scala, sublime, sublimeide
comments: true
---

_This article has been updated to address a couple of issues. If you followed this article previously to generate your ctags, you might want to skim it to find out the changes tagged with [2016-08-28]_.

A feature I desperately needed in Sublime [since my migration from Intellij](http://sanj.ink/posts/2015-07-15-using-sublime-for-scala-development.html) was the ability to browse the sources of my project's dependencies. Without this ability I was basically relegated to using the Scaladocs and Google/SO for all my information. Not good.

There is a way for you to do this in Sublime using [Ctags](http://ctags.sourceforge.net).

<iframe id="movie1" src="https://player.vimeo.com/video/137045055" width="800" height="501" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe> <p><a href="https://vimeo.com/137045055">Browsing Scala dependency sources through Sublime and Ctags</a> from <a href="https://vimeo.com/user4351020">ssanj</a> on <a href="https://vimeo.com">Vimeo</a>.</p>

So what are Ctags?

> Ctags generates an index (or tag) file of language objects found in source files that allows these items to be quickly and easily located by a text editor or other utility.

Cool. That sounds promising! Unfortunately [Scala is not one of the supported languages](http://ctags.sourceforge.net/languages.html).

[The sbt-ctags plugin](https://github.com/ceedubs/sbt-ctags) gives you this Scala support for Ctags through sbt.

> SBT ctags is an SBT plugin that will generate ctags for your Scala project.

> It unzips the source jars for your project dependencies and generates ctags for these dependency sources in addition to the Scala/Java source of your project itself.

> By default, the plugin assumes you have a ctags executable on your path that is syntax-compatible with Exuberant Ctags.

In addition to downloading all the sources for your dependencies the sbt-ctags plugin also creates the .tags file that can be used by any Ctags-aware editor.

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

Add the [sbt-ctags plugin](https://github.com/ceedubs/sbt-ctags) dependency to __plugins.sbt__ located at __~/.sbt/0.13/plugins__:

```{.scala}
addSbtPlugin("net.ceedubs" %% "sbt-ctags" % "0.1.0")
```

note: _you may need to create the above file if it doesn't exist._

The sbt-ctags plugin downloads the sources for your project dependencies into __target/sbt-ctags-dep-srcs__ by default. One problem with this is that every time you run an ```sbt clean``` your dependency source files are deleted. Not very useful.

Let's fix it so that the dependency source files are not downloaded to the target directory. We want to do this globally so we have to create a global plugin.

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

[Create a ~/.ctags file](https://github.com/ceedubs/sbt-ctags#user-content-configuring-ctags) to configure Ctags to  index Scala files:

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
[2016-08-28] __NB__: _If you read a previous revision of this article, you might have created a __~/.tags__ file instead of a __~/.ctags__ file. This was a typo and you should create the __~/.ctags__ file instead_.

4. Install the Sublime Ctags Plugin

You can install the Sublime Ctags plugin from [Package Control](https://packagecontrol.io/packages/CTags) or manually from the [repository](https://github.com/SublimeText/CTags).

The Sublime Ctags plugin will use the __.tags__ index file generated in your project root directory to lookup the symbols you need once you build its tags.

# Usage #

## Directly through Sublime Text ##

[2016-08-28] Running __genCtags__ will download all sources for your dependencies and put them into your __.ctags_srcs__ directory. From here Sublime Text can index these files and resources and they will be available under the "Goto Definition" command.

1. On any new project or when you add a new dependency, run:

```{.command .scrollx}
sbt genCtags
```

2. Within Sublime put your cursor on a method or member and choose "Goto Definition" from the context menu.

![Goto Definition](/images/sublime_go_to_definition_ctags.jpg)


## Through Ctags ##

[2016-08-28] Ctags offers a similar functionality but you have to rebuild the Ctags index files, after running __genCtags__.

1.  Rebuild your Ctags index with: __Find > CTags > Rebuild Tags__ or look it up in the command palette with __CMD + SHIFT + P__. Once the tags have been generated you will see a file called __.tags_sorted_files__ in your project directory.

2. To use Ctags to browse sources, use the "Navigate to Definition" command.

Both variations (plain Sublime Text and Ctags) allow you to browse sources. It might be overkill to have them both so choose which one you like better and stick with that.

# Customisation #

To define a shortcut for the "Goto Definition" command add a binding to your user key bindings file.

Edit your user key bindings file by clicking on __Sublime Text__ > __Preferences__ > __Key Bindings - User__ and add the following binding:

```{.command .scrollx}
{ "keys": ["f4"], "command": "goto_definition" }
```

The above binding maps __F4__ as the key to browse your sources. You can change this mapping to whatever you like."