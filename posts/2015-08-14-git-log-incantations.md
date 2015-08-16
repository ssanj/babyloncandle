---
title: Git Log Incantations
author: sanjiv sahayam
description: Various ways to display a Git log.
tags: git
comments: true
---

Display a Git log with the status of each file of a commit along with an abbreviated commit hash:
```{.command .scrollx}
git log --stat --abbrev-commit
```

![git log example 1](/images/git-log-1.jpg)

Display a graph of all changes along with merges and the location of HEAD and other branches:

```{.command .scrollx}
git log --color --graph --pretty=format:'%C(bold white)%H %d%Creset%n%s%n%+b%C(bold blue)%an <%ae>%Creset %C(bold green)%cr (%ci)' --abbrev-commit
```

![git log example 2](/images/git-log-2.jpg)


Display commits with relative dates:

```{.command .scrollx}
git log --color --graph --pretty=format:'%C(bold white)%h%Creset -%C(bold green)%d%Creset %s %C(bold green)(%cr)%Creset %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
```

![git log example 3](/images/git-log-3.jpg)

Display the last ten commits with a one line summary:
```{.command .scrollx}
git log --pretty=oneline -n 10 --abbrev-commit
```

![git log example 4](/images/git-log-4.jpg)