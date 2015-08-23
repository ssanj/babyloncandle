---
title: Shortcut to Close All Open Windows in Sublime
author: sanjiv sahayam
description: How to add a shortcut to close all open windows in Sublime.
tags: sublime, sublimeide
comments: true
---

Sublime has many options to close tabs such as closing the current tab or closing all tabs to the right or closing other tabs. What would be nice is to have an option to close all tabs.

This functionality already exists under __File__ > __Close All Files__ but there is no shortcut for it. 

Here's how you add a shortcut to close all open files:

```{.command}
{ "keys": ["shift+super+ctrl+w"], "command": "close_all" }
```

Now you can close all open files with "shift+super+ctrl+w".