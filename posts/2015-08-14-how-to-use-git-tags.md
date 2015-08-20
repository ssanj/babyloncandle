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
git tag
```
Thanks to my friend [Ryan](http://ryandrake.com) who pointed this out. I had incorrectly assumed you needed the __-l__ flag for this - which you don't.

Example output:

```{.terminal}
v0.1
v0.10
v0.11
v0.12
v0.13
v0.2
v0.3
v0.4
v0.5
v0.6
v0.7
v0.8
v0.9
```

To filter existing tags (this is when you need the __-l__ flag):

```{.command}
git tag -l yourfilter
```

Example:

```{.command}
git tag -l v0.1*
```

Example output:

```{.terminal}
v0.1
v0.10
v0.11
v0.12
v0.13
```

Create an [annotated](https://git-scm.com/book/en/v2/Git-Basics-Tagging#Creating-Tags) tag with:

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

To see information about a tag use:

```{.command}
git show your.tag.name
```

Example:

```{.command}
git show v0.13
```

Example output:

```{.terminal}
git show v0.13
tag v0.13
Tagger: Brian McKenna <brian@brianmckenna.org>
Date:   Thu Apr 30 14:10:58 2015 -0600

Releasing 0.13

commit c1e780b9a5803ec231a8901dac6ff55982766719
Author: Brian McKenna <brian@brianmckenna.org>
Date:   Thu Apr 30 14:10:58 2015 -0600

    Setting version to 0.13

diff --git a/version.sbt b/version.sbt
index 66bd841..0cb2bbf 100644
--- a/version.sbt
+++ b/version.sbt
@@ -1,2 +1,2 @@

-version in ThisBuild := "0.13-SNAPSHOT"
+version in ThisBuild := "0.13"
```

An interesting note is [there are two types of tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging):

> Git uses two main types of tags: lightweight and annotated.

> A lightweight tag is very much like a branch that doesn’t change – it’s just a pointer to a specific commit.

> Annotated tags, however, are stored as full objects in the Git database. They’re checksummed; contain the tagger name, e-mail, and date; have a tagging message; and can be signed and verified with GNU Privacy Guard (GPG). It’s generally recommended that you create annotated tags so you can have all this information; but if you want a temporary tag or for some reason don’t want to keep the other information, lightweight tags are available too.