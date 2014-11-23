---
title: Install and configure RKHunter on Linux
author: sanjiv sahayam
description: How to install and configure Rootkit Hunter for Ubuntu.
tags: linux, security, ubuntu
---

Install
--------

To install rkhunter run:
```{.scrollx}
sudo apt-get install rkhunter
```
Choose __no mail server__ when prompted.

Configure
---------

1. Check for database updates with:
```{.scrollx}
sudo rkhunter --update
```

1. Run a full scan with:
```{.scrollx}
sudo rkhunter -c
```

1. Add the following to the __rkhunter.conf__ file to ignore known issues:
    a. ALLOWDEVFILE="/dev/.udev/rules.d/root.rules"
    a. ALLOWHIDDENDIR="/dev/.udev"
    a. ALLOWHIDDENFILE="/dev/.blkid.tab"
    a. ALLOWHIDDENFILE="/dev/.blkid.tab.old"
    a. ALLOWHIDDENFILE="/dev/.initramfs"
    a. SCRIPTWHITELIST="/usr/bin/unhide.rb"

1. Check the RKHunter configuration updates just made with:
```{.scrollx}
sudo rkhunter -C
```
1. Remove ssh configuration issues by disabling root login and password login by editing __/etc/ssh/sshd_config__ and updating the following:
    a. PermitRootLogin no
    a. PasswordAuthentication

1. Run a full scan to ensure there are no other errors:
```{.scrollx}
sudo rkhunter -c
```

1. Once there are no errors, update the RKHunter data file:
```{.scrollx}
sudo rkhunter --propupd
```

Maintain
---------

If your system is infected with rootkits or if you install any new packages you could get RKHunter errors. You can fix them as follows:

1. Rescan the system
```{.scrollx}
sudo rkhunter -c
```
1. Fix any errors
1. Update the data file
```{.scrollx}
sudo rkhunter --propupd
```

Other useful commands
---------------------

1. To check for a new version:
```{.scrollx}
sudo rkhunter --versioncheck
```

1. To run without manual intervention, remove colour codes and only output warnings:
```{.scrollx}
sudo rkhunter --cronjob --rwo
```

Additional Dependencies
-----------------------

1. Install Unhide to find hidden processes and ports:
```{.scrollx}
sudo apt-get install unhide
```

1. [Install Skdet for additional Suckit Rookit checks](http://blog.ssanj.net/posts/2014-11-23-install-skdet-for-rkhunter-linux.html).

1. Tripwire - It is recommended that Tripwire be installed as a standalone package and not run through RKHUnter.


Useful links
------------

1. [RKHunter Wiki](http://sourceforge.net/p/rkhunter/wiki/browse_pages/)
1. [How to use RKHunter to Guard Against Rootkits on an Ubuntu Vps](https://www.digitalocean.com/community/tutorials/how-to-use-rkhunter-to-guard-against-rootkits-on-an-ubuntu-vps)
1. [Ubuntu RKHunter](https://help.ubuntu.com/community/RKhunter)
1. [Reconfigure RKHunter to Avoid False Positive Warninngs on Debian 5.0](http://www.faqforge.com/linux/reconfigure-rkhunter-to-avoid-false-positive-warninngs-on-debian-5-0/)

