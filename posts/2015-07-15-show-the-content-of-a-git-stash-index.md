---
title: Show the Content of a Git Stash Index
author: sanjiv sahayam
description: A short Git incantation to show file contents of a stash
tags: git
comments: true
---

If you've ever wondered how to see the contents of files within a Git stash, you are not alone.

List your Git stashes with:

```{.command}
git stash list
```

Then show the contents of the stash with:

```{.command}
git stash show -p stash@{stash_number}
```