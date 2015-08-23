---
title: Quick File Creator Sublime Text Plugin
author: sanjiv sahayam
description: A Sublime Text 3 plugin to create files or directories anywhere within your project structure.
tags: sublime, sublimeide
comments: true
---

My friend [Matt](https://twitter.com/edwardsmatt) put me on to this cool Sublime Text plugin called [Quick File Creator](https://packagecontrol.io/packages/Quick%20File%20Creator). It basically lets you create a file or directory anywhere within your project without having to navigate to it. The doco states:

 >  This plugin instead pops up a quick panel that lets you pick the directory for the new file or subdirectory using the built-in fuzzy matching. If you are currently editing a file, that file's directory will be located at the top of the list to make it even easier to create the new file or subdirectory in the same location. Select a directory, input the new name in the input panel at the bottom of the window, and you're done!

![Quick File Creator - Sublime Text Plugin](/images/quick_file_creator_popup.jpg)

 You can also exclude directories you do not care about through your project settings:

```{.javascript .scrollx}
{
    "SublimeQuickFileCreator":
      {
        "excluded_dir_patterns":
        [
          "tmp.*", "|.git", "|.svn", "|.hg"
        ]
      }
}
```
or User preferences:

```{.javascript .scrollx}
{
  "excluded_dir_patterns": [
    "tmp", "|.git", "|.svn"
  ]
}
```

Some caveats:

 > Note that each pattern is anchored to the beginning and end of the directory name, so you should not use ^ or $ in your patterns. Also, since ST2 does not allow backslashes in settings files, use a vertical bar (|) instead of a backslash to escape special regex symbols such as dots in directory names.

The other brilliant feature is:

 > The file/folder input panel supports recursive folder or file creation. If the parent path does not exist then the path will be created.

Need to create a package structure for your java/scala project? No problemo! QFC will create the full package path and your class file at the same time! :) 