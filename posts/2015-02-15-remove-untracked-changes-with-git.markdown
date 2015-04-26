---
title: Remove Untracked Changes with Git
author: sanjiv sahayam
description: How to use git clean to remove untracked changes.
tags: git
---

So you've got a bunch of untracked files and folders in your git repo and you don't want to delete them all manually one-by-one. How do you use the power of git to do this for you?

__git clean__ will remove all untracked files for you.

There are several variants:

Perform a dry run to display which files will be deleted:

```{.command}
git clean -dn
```

Once you are satisfied with the output then run:

```{.command}
git clean -df
```

This will remove all files and directories that are not tracked.

If you would like to perform an interactive removal of files and directories then use:

```{.command}
git clean -di
```
