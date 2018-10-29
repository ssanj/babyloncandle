---
title: How to Push Git Tags with Commit
author: sanjiv sahayam
description: Stop having to double push commits and tags and do it all in one go.
tags: git
comments: true
---

When pushing code that has some tags to a remote repository, you need to first push the commits with:

```{.command .scrollx}
git push
```

and then follow with:

```{.command .scrollx}
git push --tags
```

Wouldn't it be nice if you could push the commits and tags in one go? Well now you can by setting one simple config option:

```{.command .scrollx}
git config --global push.followTags true
```

From the git documentation:

> push.followTags
> If set to true enable --follow-tags option by default. You may override this configuration at time of push by specifying --no-follow-tags.

> --follow-tags
> Push all the refs that would be pushed without this option, and also push annotated tags in refs/tags that are missing from the remote but are pointing at commit-ish that are reachable from the refs being pushed.

References:

- [Push git commits & tags simultaneously](https://stackoverflow.com/questions/3745135/push-git-commits-tags-simultaneously),
- [git-conig](https://git-scm.com/docs/git-config/2.4.1)