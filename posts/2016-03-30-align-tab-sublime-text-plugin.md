---
title: AlignTab Sublime Text Plugin
author: sanjiv sahayam
description: A Sublime Text 2/3 plugin to align code on virtually any character or set of characters.
tags: sublime, sublimeide
comments: true
---

The [AlignTab](https://packagecontrol.io/packages/AlignTab) plugin allows you to select a bunch of code and automatically align it on some arbitrary characters. Just choose __AlignTab__ from the command palette (CMD + SHIFT + P) and you're good to go.

Here's an example of aligning on definition:

![Align on Definition](/images/sublime_plugin_aligntab_on_equals.gif)

And another on aligning on case matches:

![Align on Pattern Match](/images/sublime_plugin_aligntab_on_arrow.gif)

Wouldn't it be nice if you could preview the alignment before actually committing to it? Well you're in luck with the __Live Preview Mode__ you can do just that:

![Align on Comma with Live Preview](/images/sublime_plugin_aligntab_on_comma.gif)

Another nifty alignment is __Table Mode__. If you are writing a wiki-style table, this alignment automatically formats headings, columns and rows for you at application time and from then on. It also aligns any other tables you create on the page with the first table.

_One caveate is that you need to escape the pipe symbol (|) (because it's a regular expression) with a backslash before using it as a formatting character_.

![Align with Table Mode](/images/sublime_plugin_aligntab_on_table.gif)

You can also use complex regex matches for those highly customisable alignments. Dig into the [documentation](https://github.com/randy3k/AlignTab) and [examples](https://github.com/randy3k/AlignTab/wiki/Examples) to find out more.

