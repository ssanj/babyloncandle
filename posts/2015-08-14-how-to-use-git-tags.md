---
title: How to use Git Tags
author: sanjiv sahayam
description: How to use Git tags in your workflow.
tags: git
comments: true
---

Here's a summary on how to use Git tags.

List existing tags with:

```{.command}
git tag -l
```

Create a tag with:

```{.command}
git tag -a "your.tag.name"
```

Attach a tag to an existing commit with:

```{.command}
git tag -a "your.tag.name" commitid
```

Push your tags remotely with:

```{.command}
git push --tags
```

Delete a local tag with:

```{.command}
git tag -d "your.tag.name"
```

[Delete a remote tag with](https://nathanhoad.net/how-to-delete-a-remote-git-tag):

```{.command}
git push origin :refs/tags/your.tag.name
```