---
title: How to Rename a Git Branch
author: sanjiv sahayam
description: How to rename a exist Git branch without losing your work.
tags: git
comments: true
---

So you've been working on a Git branch on some new feature and when you go to push your changes you realise your branch name is incorrect. You forgot to include the issue number or some such and now your pull request is going to be hard to track down to a feature.

What would be useful is to be able to rename your current branch without losing all your work. The Git incantation for this is surprisingly easy:

```{.command .scrollx}
git branch -m <old name> <new name>
```

For example:

```{.command .scrollx}
git branch -m feature-x [123]-feature-x
```

The __-m__ option moves an existing branch into a new one:

 >  -m, --move Move/rename a branch and the corresponding reflog.

If you've already pushed the old branch name to remote, you can delete it with:

```{.command .scrollx}
git push origin :<old name>
```

For example:

```{.command .scrollx}
git push origin :feature-x
```

Then simply push your changes for the new branch to remote:

```{.command .scrollx}
git push origin [123]-feature-x
```

and you're done. :)
