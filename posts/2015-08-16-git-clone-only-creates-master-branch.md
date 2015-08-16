---
title: Git Clone Only Creates Master Branch
author: sanjiv sahayam
description: ???
tags: git
comments: true
---

When you clone a repository in Git, it only downloads the master branch. What if you wanted a specific remote branch as well?

There are a couple of ways you can do this:

## Use checkout ##

Git checkout will clone the remote branch and then change to it.

```{.command .scrollx}
git checkout -b remote/branchname
```

Example:

```{.command .scrollx}
git checkout -b origin/new_config
```

## Use branch ##

Git branch will only clone the remote branch.

```{.command .scrollx}
git branch local_branch_name remote/branchname
```

Example:

```{.command .scrollx}
git branch new_config origin/new_config
```