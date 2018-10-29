---
title: Rebasing with Git Pull by Default
author: sanjiv sahayam
description: Rebase everytime you `git pull` to prevent ugly merge commits.
tags: git
comments: true
---

Sometimes when pulling changes from a remote repository you are left with a somewhat ugly merge. This leaves you wishing you had rebased instead. Wouldn't it be nice if you could rebase everytime you `git pull` automatically? And now you can with another simple configuration option:

```{.command .scrollx}
git config --global --bool pull.rebase true
```