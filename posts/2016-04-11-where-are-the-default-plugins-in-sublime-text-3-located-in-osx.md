---
title: Where are the Default Plugins in Sublime Text 3 Located in OSX?
author: sanjiv sahayam
description: How to find the location of the default bundled sublime text plugins on OSX and now to extract them.
tags: sublime, python
comments: true
---

The default plugins included with Sublime Text are a great source of information on how to use the [Sublime Text 3 api](http://www.sublimetext.com/docs/3/api_reference.html). Unfortunately, the default plugins can't be found in the usual Packages directory. On OSX, they are [neatly tucked away](https://forum.sublimetext.com/t/st3-where-is-default-osx-sublime-keymap-located/10757) in the application installation file located at:

```{.command .scrollx}
/Applications/Sublime Text 3.app/Contents/MacOS/Packages
```

The default plugins (and configuration) are stored within a file called: __Default.sublime-package__, which is simply a zip file.

To view the files you can extract the Default.sublime-package to a temporary directory with unzip:

```{.command .scrollx}
unzip /Applications/Sublime Text 3.app/Contents/MacOS/Packages/Default.sublime-package -d some/temp/dir
```

And now you have full access to a host of example plugins to work with.