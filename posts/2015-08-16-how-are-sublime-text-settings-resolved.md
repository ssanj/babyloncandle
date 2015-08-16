---
title: How are Sublime Text Settings Resolved?
author: sanjiv sahayam
description: The order in which Sublime Text settings are resolved.
tags: sublime
comments: true
---

Sublime Text settings have a definite resolution order. [I stumbled across this forum post on the exact order](http://www.sublimetext.com/forum/viewtopic.php?f=6&t=9076):

The user can override these globally (User/Base File), per-syntax, or per-project. The order that things would be searched would be (last match wins):

1. Packages/MyPlugin/Base File.sublime-settings
1. Packages/MyPlugin/Base File <platform>.sublime-settings
1. Packages/User/Base File.sublime-settings
1. Packages/User/Preferences.sublime-settings
1. Project Settings
1. Packages/MyPlugin/<syntax>.sublime-settings
1. Packages/MyPlugin/<syntax> <platform>.sublime-settings
1. Packages/User/<syntax>.sublime-settings
1. Packages/User/Distraction Free.sublime-settings