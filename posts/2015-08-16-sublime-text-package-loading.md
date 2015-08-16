---
title: Sublime Text Package Loading
author: sanjiv sahayam
description: How package loading works in Sublime Text.
tags: python, sublime
comments: true
---

I recently submitted [Scoggle](https://github.com/ssanj/Scoggle) to [package control](https://packagecontrol.io) and had some [issues around package path and how classes are loaded](https://github.com/wbond/package_control_channel/pull/4701). Here is some feedback I received from one of the repository maintainers:

> Sublime Text adds the Packages path to sys.path (and a custom loader to sys.meta_path for that matter, which handles the loading of .sublime-package files), so every module in a package is accessible with PackageName.path.to.module. For your tests (or more specifically everything that is in a sub package, i.e. subdirectory) you need to use two dots with relative imports so that you go up a level, such as from .. import scoggle_types as stypes.

> For the dynamic class loading, you can use ____package____ instead of "Scoggle.matchers" because the two are equivalent in the average situation and the first will still be correct if the package was saved under a different name. I'm not exactly sure about tests, but they should work as well. There are other methods available for doing what you intend to do (e.g. importing the classes into matcher.__init__ and then using getattr), but yours should work "just fine".