---
title: How to Install El Capitan on a New SSD
author: sanjiv sahayam
description: How to install macosx El Capitan onto a new SSD.
tags: apple, macosx, elcapitan
comments: true
---

1. Download El Capitan installation from the App store. It might complain that you already have it installed but download it anyway. It will be downloaded to __/Applications__ and be named __Install OS X El Capitan.app__
2. [Create a bootable image from the El Capitan installer](https://support.apple.com/en-au/HT201372) on a USB stick larger than 6GB. In a terminal run:

```{.terminal .scrollx}
sudo /Applications/Install\ OS\ X\ El\ Capitan.app/Contents/Resources/createinstallmedia --volume /Volumes/MyVolume --applicationpath /Applications/Install\ OS\ X\ El\ Capitan.app
```

_where MyVolume is the name of your USB stick. Change this to match the name of your USB stick._

3. Physically install SSD into the target laptop.
4. Insert the El Capitan USB stick into the target laptop and power up. (_This might take a while._)
5. When the menu finally turns up, choose __Disk Utility__ and format the SSD as __Mac OS Extended (Journaled)__ with a unique name.
6. Once complete, exit from __Disk Utility__ and return the main menu.
7. Choose to __Install OS X__ on the formatted SSD.
8. Follow the prompts until El Capitan is installed.

[Here's link to a video on how to physically install the SSD as well as run the software installation](https://www.youtube.com/watch?v=F82ThP-6jeA).

If you'd rather install Yosemite on the SSD then follow the [How to Install Yosemite on a New SSD](http://sanj.ink/posts/2015-07-12-how-to-instal-yosemite-on-a-new-ssd.html) guide.