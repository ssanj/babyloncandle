---
title: Git Clone only a Single Branch
author: sanjiv sahayam
description: How to only clone a single branch of a repository with multiple branches.
tags: git
comments: true
---

Cloning a Git repository gives you all the branches by default. Sometimes you might just want a single branch. To do that use:

```{.command .scrollx}
git clone [url] -b [branch-name] --single-branch
```

reference: [so:clone-only-one-branch](http://stackoverflow.com/questions/4811434/clone-only-one-branch#14930421)