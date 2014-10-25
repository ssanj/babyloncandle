---
title: Console Out of Sync on Changing Video Card
author: sanjiv sahayam
tags: linux, ubuntu
---

I have a Ubuntu 12.04 server box running with an nVidia 8800GT video card. I decided to replace it with a nVidia 520GT which uses a fraction of the power.

On booting after the video card swap I was greeted with a console that was out of sync (hazy) and unreadable. After trying numerous resolution changes and playing around with the grub bootup options I was not any closer to getting my console sorted out.

The answer was to add a __nomodeset__ option to the linux boot options, which fixed the hazy console :)

Here are the steps I followed:

1. Reboot your server
2. Press esc repeatedly to enter the grub menu. (This combination may vary if you have a different bios)
3. Select your primary boot image from the grub menu (usually the first option)
4. Press the 'e' key to edit the configuration
5. Add __nomodeset__ to the end of the line begining with: linux __/boot/vmlinuz....__
6. Press Ctrl+X to save and exit
7. Boot from the primary image

If the above fixes your resolution issues, you then need to make the changes permanent. You can do that as follows:

1. Edit your __/etc/default/grub__
2. Add the __nomodeset__ option to the following:

           GRUB_CMDLINE_LINUX_DEFAULT="nomodeset"

3. Save and exit
4. Update grub with: __sudo update-grub__
5. Reboot

If all went well you should have a working console again!

The __nomodeset__ option prevents the kernel from loading graphic drivers. Apparently there are incompatibilities with some older video cards that do not support this fully. Read more about it [here](http://ubuntuforums.org/showthread.php?t=1613132).

__Update:__ Unfortunately adding __nomodeset__ breaks the ability to resume the server from suspend! Read more about it [here](https://bugs.launchpad.net/ubuntu/+source/linux/+bug/960920). For now I've gone back to the hazy console login screen which does not really affect me because I ssh into this server. I hope this bug is fixed soon.

