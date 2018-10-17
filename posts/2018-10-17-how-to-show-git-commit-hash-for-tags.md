---
title: How to Show Git Commit Hash for Tags
author: sanjiv sahayam
description: How do you display the commit hash and contents of a tag?
tags: git
comments: true
---

To list the commit hash for every tag in a repo use:

```{.command .scrollx}
git show-ref --tags
```

which yields something like:

```{.terminal .scrollx}
ee02aa7363f9988af700ab136a219c455cab4b5f refs/tags/v.0.4.0
2d5befba5bc80a69c6308d2a5da965488e6bf9d7 refs/tags/v.0.4.1
0099c11405a3ace8ee14b0881f9677bfc1e30f5e refs/tags/v0.4.1
```

To only list the commit hash for a particular tag use:

```{.command .scrollx}
git show-ref tag_name
```

for example, to list the commit hash for v0.4.1 use:

```{.command .scrollx}
git show-ref v0.4.1
```

which gives you a single hash:

```{.terminal .scrollx}
0099c11405a3ace8ee14b0881f9677bfc1e30f5e refs/tags/v0.4.1
```

To display the contents of a hash use:

```{.command .scrollx}
git show hash
```

