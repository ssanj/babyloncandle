---
title: Linux RAID Failed on Boot
author: sanjiv sahayam
tags: linux, ubuntu
---

After upgrading from Ubuntu 10.04 to 10.10 I noticed that my machine refused to startup on most occasions. After a few reboots it eventually decided to startup. I am running a raid0 setup which includes the boot partition.

I got an error stating that "one or more raid devices were degraded".

The solution turned out to be a simple one. It turned out that one of the HDDs in the raid array took a longer time to spin up than the others. This difference only became apparent in Ubuntu 10.10.

The fix is to add a rootdelay parameter to your __/etc/default/grub__ file and update grub.

Edit your __/etc/default/grub__ file and update the following line to include a rootdelay:

    GRUB_CMDLINE_LINUX_DEFAULT="rootdelay=90"

You might need to increase your root delay if it still occurs after the above fix.

Update grub with:

    sudo update-grub


and you should be all good! :)