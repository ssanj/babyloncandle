---
title: How to use Git Branches
author: sanjiv sahayam
description: How to use Git branches in your workflow.
tags: git   
comments: true
---

Create a branch from the current branch and change to it:

```{.command}
git checkout -b new_branch_name
```

Create a branch from a commit:

```{.command}
git branch commitid
```

Create a remote tracking branch on the __origin__ remote for a local branch:

```{.command}
git push -u origin branch_name
```

Switch to another branch:

```{.command}
git checkout new_branch_name
```

Delete a local branch if it has synced up changes with the upstream branch or HEAD

```{.command}
git branch -d branch_name
```

Force delete a local branch even if it has not synced up changes with its upstream branches:

```{.command}
git branch -D branch_name
```

Delete a remote branch on the __origin__ remote:

```{.command}
git push origin :branch_name
```