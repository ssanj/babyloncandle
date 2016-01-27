---
title: How to Install Yosemite on a New SSD
author: sanjiv sahayam
description: How to install macosx Yosemite onto a new SSD.
tags: apple, macosx, yosemite
comments: true
---

After I got my new MBP, I decided to sell my old 2010 MBP. I wanted to keep all my existing data on my old laptop, so I decided to swap out its SSD and to put in a brand new one.

The process of installing Yosemite on the new SSD was not as straightforward as I would have liked. I thought I'd blog about it to help anyone else who needed to do the same.

Basically I was trying to sell a 2010 MBP with a new SSD, with Yosemite installed but not configured.

1. Download Yosemite installation from the App store. It might complain that you already have it installed but download it anyway. It will be downloaded to __/Applications__ and be named __Install OS X Yosemite.app__
2. [Create a bootable image from the Yosemite installer](https://support.apple.com/en-au/HT201372) on a USB stick larger than 6GB. In a terminal run:

```{.terminal .scrollx}
sudo /Applications/Install\ OS\ X\ Yosemite.app/Contents/Resources/createinstallmedia --volume /Volumes/MyVolume --applicationpath /Applications/Install\ OS\ X\ Yosemite.app
```

_where MyVolume is the name of your USB stick. Change this to match the name of your USB stick._

3. Physically install SSD into the target laptop.
4. Insert the Yosemite USB stick into the target laptop and power up. (_This might take a while._)
5. When the menu finally turns up, choose __Disk Utility__ and format the SSD as __Mac OS Extended (Journaled)__ with a unique name.
6. Once complete, exit from __Disk Utility__ and return the main menu.
7. Choose to __Install OS X__ on the formatted SSD.
8. Follow the prompts until Yosemite is installed.

[Here's link to a video on how to physically install the SSD as well as run the software installation](https://www.youtube.com/watch?v=F82ThP-6jeA).

If you'd rather install El Capitan on the SSD then follow the [How to Install El Capitan on a New SSD](http://sanj.ink/posts/2016-01-21-how-to-install-el-capitan-on-a-new-ssd.html) guide.