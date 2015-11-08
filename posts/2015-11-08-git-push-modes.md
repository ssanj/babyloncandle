---
title: Git Push Modes
author: sanjiv sahayam
description: The different Git push modes.
tags: git
comments: true
---

If you've tried to push to git recently after a new installation you might get a warning of the sort:

> warning: push.default is unset; its implicit value has changed in
Git 2.0 from 'matching' to 'simple'.

So what does this mean? The previous default behaviour ("matching") was to push all local branches that had remote counterparts whenever you did a push. While this seems "useful" it can lead to potential issues

Example: You complete work on your current branch and choose to push to remote. You forget that you have other branches with unfinished work that you don't want pushed. All branches now get pushed to remote, which is not what you wanted.

The new default for Git 2.0 is the "simple" mode which only pushes the current branch you are working on.

The Git documentation describes the four Git push modes:

* nothing - do not push anything (error out) unless a refspec is explicitly given.
* current - push the current branch to update a branch with the same name on the receiving end. When pushing to a remote that is different from the remote you normally pull from, work as current.
* upstream - push the current branch back to the branch whose changes are usually integrated into the current branch (which is called @{upstream}). This mode only makes sense if you are pushing to the same repository you would normally pull from.
* simple - Works like upstream with an added safety to refuse to push if the upstream branch's name is different from the local one.
* matching - push all branches having the same name on both ends.

