---
title: How to Move a Tab Between Split Panes in Sublime Text
author: sanjiv sahayam
description: Moving Tabs between split panes in Sublime Text
tags: sublime
comments: true
---

When looking up definitions of a complex [Scala](https://scala-lang.org/) class in [Sublime Text](https://www.sublimetext.com/) it's quite common to open up a lot of related classes; each in a separate tab. 🤦🏾

![Related Classes](/images/how-to-move-between-split-panes-st/many-tabs.png)

In the past, I opened related classes in a new tab with the `goto_definition` command. This opens the class in a new tab in the current pane. I would repeat this for every definition I needed to look up. This very quickly leads to a lot of open tabs as shown in the image above.

It becomes very hard to keep track of the original class you were working on because there are so many tabs open. This can get overwhelming in more complex projects, sometimes leading to tens of related classes open in separate tabs.

One way I've found to  manage this "tab overload" is to move these related classes to another split pane. For example you can create a vertical split pane with the `CMD` + `ALT` + `SHIFT` + `2` key combo.

![Vertical Split Pane](/images/how-to-move-between-split-panes-st/horizontal-split.png)

I used to click-and-drag the required tabs between split panes.

![Dragging Tabs Between Panes](/images/how-to-move-between-split-panes-st/click-drag-between-panes.gif)

While this is fine it does become a little tedious when you have many open tabs that you want to move. It's much easier if you can do it through a keyboard shortcut.

To my surprise, Sublime Text already had this feature buried in the `View` menu. Sublime Text refers to the split panes as "groups" and lets you move files between these "groups".

![Sublime Text View Menu](/images/how-to-move-between-split-panes-st/st-menu.png)

Now I can seamlessly move tabs to pane below with `CTRL` + `SHIFT` + `2`. So much easier :) What would make this even better is if Sublime Text selected the tab to the `right` after a move instead of the one on the `left`. That way you could combo your way through all the open tabs (assuming they were stacked to the right) without moving your mouse .

![Move to Split Pane 2](/images/how-to-move-between-split-panes-st/move-views-between-panes.gif)

More generally you can move tabs to a pane with a given number with  `CTRL` + `SHIFT` + `PANE_NUMBER`
