---
title: How to Reveal the Current File in the Sidebar in Sublime
author: sanjiv sahayam
description: How to add a shortcut to reveal the current file in the sidebar in Sublime Text 3.
tags: sublime, sublimeide
comments: true
---

A feature I often need when coding in Sublime is to reveal the current file in the sidebar.

![Reveal in Sidebar](/images/sublime-reveal-in-sidebar.jpg)

You can do this by right-clicking and choosing __Reveal in Sidebar__ from the context menu.

![Context Menu](/images/sublime-reveal-in-sidebar-context-menu.jpg)

What would be nicer would be to have a keyboard shortcut to do this very same thing.

Choose __Sublime Text__ > __Preferences__ > __Key Bindings - User__ and add the following to map (SHIFT + F4) to reveal the current file in the sidebar:

```{.command}
{ "keys": ["shift+f4"], "command": "reveal_in_side_bar" }
```