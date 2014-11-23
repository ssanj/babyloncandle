---
title: Skdet dependency for RKHunter
author: sanjiv sahayam
description: How to install and the Skdet dependency for Rootkit Hunter for Ubuntu.
tags: linux, security, ubuntu
---

Here's how to install the Skdet dependency for RKHunter.

1. Download the following files:

    1. [skdet-1.0.sha1](http://dvgevers.home.xs4all.nl/skdet/skdet-1.0.sha1)
    1. [skdet-1.0.tar.bz2](http://dvgevers.home.xs4all.nl/skdet/skdet-1.0.tar.bz2)
    1. [skdet-fix-includes.diff](http://dvgevers.home.xs4all.nl/skdet/skdet-fix-includes.diff)


1. Extract the archive:
    ```{.scrollx}
    tar -jxf skdet-1.0.tar.bz2
    ```

1. Copy the __.diff__ file into the __skdet-1.0/__ directory.

1. Update the __skdet-1.0.sha1__ file and add an extra space between the sha1 for the diff file and the file name. (otherwise the check won't run against it.)

1. Go into the __skdet-1.0/__ directory and run:
    ```{.scrollx}
    make clean
    ```

    This will remove any previously compiled files.

1. Check the sha1 values of all files from the parent directory of the skdet-1.0 directory:
    ```{.scrollx}
    sha1sum skdet-1.0.sha1
    ```

1. Make the skdet library with:

    ```{.scrollx}
    make
    ```

1. Verify that the skdet library works with:
    ```{.scrollx}
    sudo skdet -c
    ```

1. Copy the __skdet__ executable somewhere on your path with root priviledges:
    ```{.scrollx}
    /usr/bin
    ```

1. Run a RKHunter scan with:
    ```{.scrollx}
    sudo rkhunter -c --sk
    ```

    You should see this in your summary for your scan in the __/var/log/rkhunter.log__ file:
    ```{.scrollx}
    Info: Found the 'skdet' command: /usr/bin/skdet
         Running skdet command                         [ OK ]
         Suckit Rookit additional checks               [ OK ]
    ```

    You should get the following error:

    ```{.scrollx}
    Warning: The file '/usr/bin/skdet' exists on the system, but it is not present in the 'rkhunter.dat' file.
    ```

1. Update your data file with:

    ```{.scrollx}
    sudo rkhunter --propupd
    ```
    And you should see something like:

    ```{.scrollx}
    [ Rootkit Hunter version 1.4.0 ]
    File updated: searched for 168 files, found 138
```
And you're done.