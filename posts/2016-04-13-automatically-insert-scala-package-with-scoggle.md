---
title: Automatically Insert Scala Package with Scoggle
author: sanjiv sahayam
description: Scoggle now supports automatically inserting a package for any scala file in your sublime project.
tags: scala, sublime, sublimeide
comments: true
---

When using Sublime Text for Scala development I found myself constantly hand-crafting the package path of every Scala file I created. While this was annoying, once I spent the two seconds to convert the file path to a dotted path and type it in, I completely forgot about it - until the next time.

Sometime last week I started wondering if I could use the [Scoggle](https://github.com/ssanj/Scoggle) code base to implement this new functionality. I knew I had all the necessary information to make this possible in Scoggle, but thought it belonged in its own Sublime plugin.

A couple of days ago, I decided to simply add it into Scoggle as a supplementary feature. The results have been great! I no longer have to hand-craft package paths! Have a look at it in action below.

![Inserting a package through Scoggle](/images/scoggle_sublime_text_plugin_insert_package.gif)