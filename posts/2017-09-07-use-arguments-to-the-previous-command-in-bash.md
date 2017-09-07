---
title: Use Arguments to the Previous Command in Bash
author: sanjiv sahayam
description: How to access the arguments passed to the previous command in a Bash shell.
tags: bash
comments: true
---

Sometimes when running a shell command with some arguments, you might realise that you need to run another command but with the same arguments. If the argument is a long path or such, you really don't want to have to type it in again. A simple way to run a new command with the previous arguments is:

```{.bash .scrollx}
command arg1 arg2
newCommand !*
newCommand arg1 arg2
```

For example:

```{.bash .scrollx}
echo A B C D
echo !*
A B C D
```

or if you want to use only the last argument:

```{.bash .scrollx}
command arg1
newCommand !$
newCommand arg1
```

For example:

```{.bash .scrollx}
vim ~/.bash_profile
source !$
```

or if you want to repeat the last command:

```{.bash .scrollx}
command arg1 arg2 arg3
!!
command arg1 arg2 arg3
```

It turns out there is a slew of other variations to this syntax as I found out in this [Stackoverflow answer](https://stackoverflow.com/questions/4009412/how-to-use-arguments-from-previous-command):

```{.terminal .scrollx}
!^      first argument
!$      last argument
!*      all arguments
!:2     second argument

!:2-3   second to third arguments
!:2-$   second to last arguments
!:2*    second to last arguments
!:2-    second to next to last arguments

!:0     the command
!!      repeat the previous line
```

You could also use the __history__ command in conjunction:

```{.bash .scrollx}
command !history_line_number:1-indexed-argument-number
```

For example:

```{.bash .scrollx}
$ history
   97  rm ~/bin/st
   98  ln -s /Applications/SourceTree.app/Contents/Resources/stree ~/bin/
$ ll !98:3
ll ~/bin/
```
