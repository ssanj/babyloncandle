---
title: How to Remove Remotely Deleted Branches from your Local Git Repository
author: sanjiv sahayam
description: How to clean up local branches that track deleted remote branches.
tags: git
comments: true
---

Once you start working on feature branches you often find that you have a bunch of local branches that are tracking remote branches that no longer exist.

One easy way of keeping your local and remote repositories in sync is to use the _prune_ option of the Git __remote__ command:

```{.command .scrollx}
git remote prune <remote name>
```

For example:

```{.command .scrollx}
git remote prune origin
```

From the docs:

 > Deletes all stale remote-tracking branches under <name>. These stale branches have already been removed from the remote repository referenced by <name>, but are still locally available in "remotes/<name>".

 > With --dry-run option, report what branches will be pruned, but do not actually prune them.

This will remove all local references that track remote branches that have been deleted and keep your local repository clean and tidy. :)