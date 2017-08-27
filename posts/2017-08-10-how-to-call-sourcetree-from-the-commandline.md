---
title: How to Call Sourcetree from the Commandline
author: sanjiv sahayam
description: How to install Sourcetree commandline tools
tags: git, sourcetree
comments: true
---

I use [Sourcetree](https://www.atlassian.com/software/sourcetree) to do most of my diffing when I use Git. One thing that has urked me is that I'd have to always launch the application from Spotlight or Alfred when I wanted to see the state of some Git diffs. Strangely, when I tried to install Sourcetree's commandline tools, I got the following error:

```{.terminal .scrollx}
Installation failed
Unable to install command line tools, please refer to the system logs for more details.
```

While looking for a solution to this problem, I found a [simpler alternative](https://community.atlassian.com/t5/SourceTree-questions/SourceTree-command-line-tools-installation-failed/qaq-p/85023) by
Philip Borenstein:

 > This has been annoying me for a while. Here's a workaround that works for me:
 > $ ln -s /Applications/SourceTree.app/Contents/Resources/stree /usr/local/bin/
 > I use homebrew, so /usr/local/bin is already in my path and owned by me (instead of root)

All you need to do is add the __stree__ application to your path:

```{.terminal .scrollx}
ln -s /Applications/SourceTree.app/Contents/Resources/stree ~/bin/
```

and you are good to go!