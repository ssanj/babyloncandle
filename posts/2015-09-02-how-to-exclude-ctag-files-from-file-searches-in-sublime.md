---
title: How to Exclude Ctag Files from File Searches in Sublime
author: sanjiv sahayam
description: How to exclude certain files and directories from file searches in Sublime Text 3.
tags: scala, sublime, sublimeide
comments: true
---

If you've started using [ctags to browse your scala sources](http://sanj.ink/posts/2015-08-22-how-to-browse-scala-sources-of-your-dependencies-from-sublime.html), one thing that you might find annoying is that when you do a file search, it also searches through your __.ctags_srcs__ directory and your __.tags__ files. Most of the time this is not what you want. So how do you go about excluding these directories and files from your search?

![Find without exclude](/images/sublime-find-without-excludes.jpg)

When doing a __Find in Files...__ (CMD + SHIFT + F), you can exclude directories and files you don't want with a minus sign in front of them in the __Where__ box. Multiple clause are separated by commas.

To exclude a directory and all the files there in you need to also use a wildcard (*) following the directory.

For example to exclude all files in the .ctags_srcs directory use:

```{.command}
-.ctags_srcs/*
```

To exclude single files simply use its name. For example to exclude the .tags file use:

```{.command}
-.tags
```

Here's a full list of excludes to exclude all ctag directories and files along with the target directory, the sublime-workplace file and any xml files:

```{.command}
<project>, -.ctags_srcs/*, -.tags, -target/*, -*.sublime-workspace, -*.xml
```

![Find with exclude](/images/sublime-find-with-excludes.jpg)

And that's all there is to it.