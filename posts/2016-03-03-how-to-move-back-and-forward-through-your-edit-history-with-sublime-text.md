---
title: How to Move Back and Forward through your Edit History with Sublime Text
author: sanjiv sahayam
description: How to use Sublime Text 3 to move through your edits history.
tags: sublime, sublimeide
comments: true
---

When using Sublime Text as your IDE, it's pretty useful to be able to move through the history of your edits. I went looking for a plugin to do this on [Package Control](https://packagecontrol.io) (as you do) but then [found out that this feature is inherently supported](https://packagecontrol.io/packages/Edit%20History) in [Sublime Text 3](http://www.sublimetext.com/3).

![Jumping back and forward](/images/sublime-jump-back-and-forward-through-history.jpg)

If you want to change the key bindings, click on __Preferences > Key Bindings - User__ and add your key mappings. Here's an example that uses ALT + COMMAND + LEFT and ALT + COMMAND + RIGHT to move back and forward through history respectively:

```{.command .scrollx}
    { "keys": ["alt+super+left"], "command": "jump_back" },
    { "keys": ["alt+super+right"], "command": "jump_forward" }
```