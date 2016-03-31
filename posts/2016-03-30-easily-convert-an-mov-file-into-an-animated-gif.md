---
title: Easily Convert an MOV File into an Animated Gif on OSX
author: sanjiv sahayam
description: How to convert an mov file into an animated gif on OSX.
tags: macosx
comments: true
---

Say you've recorded a screencast using Quicktime, but now want to share it as
an animated gif. How would you go about it? Luckily there is a very simple way to do it. Follow the instructions on [OS X Screencast to animated GIF](https://gist.github.com/dergachev/4627207) to get it going. You need to install [ffmpeg](https://sourceforge.net/projects/ffmpeg) and [gifsicle](http://www.lcdf.org/gifsicle) to do all the heavy lifting.

```{.terminal .scrollx}
brew install ffmpeg
brew cask install x-quartz #dependency for gifsicle, only required for mountain-lion and above
open /usr/local/Cellar/x-quartz/2.7.4/XQuartz.pkg # runs the XQuartz installer
brew install gifsicle
```

Then you can either run with the sample config:

```{.terminal .scrollx}
ffmpeg -i in.mov -s 600x400 -pix_fmt rgb24 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > out.gif
```

or customise it to suite your needs:

* -r specifies the framerate. The default is 25. In the above example it is 10.
* -s specifies the width and height. In the above example it is 600x400.
* --delay specifies the time between each gif in hundreths of a second. In the above example it is 30ms (1/100 * 3 * 1000).
* --optimize specifies the file-size optimisation. There are 3 levels. 3 is the highest.

See the [ffmpeg man page](http://linux.die.net/man/1/ffmpeg) and the [gifsicle man page](http://www.lcdf.org/gifsicle/man.html) for more information.

[derchagev](https://github.com/dergachev) has written a snazzier version of this called [screengif](https://github.com/dergachev/screengif). It requires either Docker or Vagrant or a bunch of other stuff I didn't want to install for this tool so I chose not to install it. It would be well worth having a look if you are interested.

Below is a sample gif created with the following config settings:

```{.terminal .scrollx}
fmpeg -i convert_mov_to_gif.mov -s 800x600 -pix_fmt rgb24 -r 15 -f gif - | gifsicle --optimize=3 --delay=3 > convert_mov_to_gif.gif
```

![Sample Gif](/images/convert_mov_to_gif.gif)

