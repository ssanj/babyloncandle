---
title: Where are the Default Packages for Sublime Text Located on OSX
author: sanjiv sahayam
description: The location of the default packages for Sublime Text on OSX.
tags: sublime
comments: true
---

If you have a look in your Sublime Text installation on OSX under:

```{.terminal}
/Users/sanj/Library/Application Support/Sublime Text 3/Packages
```

you'll notice that none of the default plugins or configurations are to be found.

They live in at another [location](http://www.sublimetext.com/forum/viewtopic.php?f=2&t=13236) at:

```{.terminal}
/Applications/Sublime Text 3.app/Contents/MacOS/Packages/
```

If you can't find the Sublime Text 3.app folder try:

```{.terminal}
/Applications/Sublime Text.app/Contents/MacOS/Packages
```

All .sublime-package files are zip files. To have a look at their contents simply rename them to .zip files and extract.