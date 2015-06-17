---
title: Where&apos;s your Git HEAD at?
author: sanjiv sahayam
description: How to show which branch HEAD is on using Git.
tags: git
comments: true
---

Sometimes you want to know which branch and commit HEAD points to. With this magical Git incantation you can:

```{.command}
git log --oneline --decorate --graph --all
```

![showing git branches with HEAD](/images/git_head_branch.jpg)