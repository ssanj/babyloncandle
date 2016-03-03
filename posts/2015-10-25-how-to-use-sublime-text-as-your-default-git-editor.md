---
title: How to Use Sublime Text as Your Default Git Editor
author: sanjiv sahayam
description: How to set Sublime Text as your default Git editor.
tags: git, sublime, sublimeide
comments: true
---

If you want to use Sublime Text to edit your Git comments then you are just a couple of steps away.

First, you need to make sure your sublime executable is on your system or user path. I have my user's bin directory (~/bin) on my path. I create a symlink to the [subl](http://www.sublimetext.com/docs/3/osx_command_line.html) executable as ~/bin/sublime.

```{.terminal}
ln -s /Applications/Sublime Text.app/Contents/SharedSupport/bin/subl ~/bin/sublime
```

_Notice how I've renamed the executable from subl to sublime._

Next you need to [edit your Git config](https://help.github.com/articles/associating-text-editors-with-git) and set your default editor:

```{.terminal}
git config --global core.editor "sublime -n -w"
```

The editor options are:

```{.terminal}
n - Open a new window
w - Wait for the files to be closed before returning
```

The next time you need to edit anything for Git you'll be doing it in Sublime! :)