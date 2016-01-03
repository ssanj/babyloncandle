---
title: Docker Machine Looses Volume Data on Restart
author: sanjiv sahayam
description: How to prevent loosing volume data on Docker Machine after a restart.
tags: docker, docker-machine
comments: true
---

I had something interesting happen to me yesterday. I had setup a [Docker](https://docs.docker.com) MySQL container which I ran on my OSX laptop using [Docker Machine](https://docs.docker.com/machine) (_version 0.4.1 (e2c88d6)_). I had mapped the MySQL data directory to the host using a volume:

```{.terminal .scrollx}
-v /home/docker/config/mysql/data:/var/lib/mysql
```

After running the container from the Dockerfile, I created an additional database, a couple of tables and some data. I could see the database files created within my mapped directory on the host under __/home/docker/config/mysql/data__. These changes survived a restart of the Docker image I was running. Everything seemed to be working swimmingly until I had to restart my laptop after a software update.

When I restarted Docker Machine and the Docker image for MySQL, none of my
changes were present in the mapped directory! The only databases and tables where
those created through the Dockerfile. All other changes had been lost! The only reason I was using volumes was to not have this problem and yet here the problem was!

I inspected the container and didn't see any issues with the mounts. My friend [Michael](http://nippysaurus.com) did some more digging around and found that the file system mounted on root was [tmpfs](https://en.wikipedia.org/wiki/Tmpfs). Since files stored in tmpfs are stored in
volatile memory, they are wiped on a restart. Running a df on the host revealed the issue more clearly:

```{.terminal .scrollx}
df -h
Filesystem                Size      Used Available Use% Mounted on
tmpfs                     1.8G    115.3M      1.6G   6% /
tmpfs                  1001.4M     64.0K   1001.3M   0% /dev/shm
/dev/sda1                18.2G      2.2G     15.0G  13% /mnt/sda1
cgroup                 1001.4M         0   1001.4M   0% /sys/fs/cgroup
none                    280.3G    113.7G    166.6G  41% /Users
/dev/sda1                18.2G      2.2G     15.0G  13% /mnt/sda1/var/lib/docker/aufs
```

The only safe place to store files seems to be in the /mnt directory. I had two
options going forward:

1. Map the MySQL data directory as a volume to a subdirectory under /mnt directory on the host.
1. Expose the MySQL data directory as a volume without an explicit host map.

I chose to go with option 2 as on my laptop as it involved less stuffing around and I knew it would be persisted between restarts.

I updated my Dockerfile and exposed the MySQL data directory as a volume:

```{.terminal .scrollx}
VOLUME  ["/var/lib/mysql"]
```

That fixed the problem. This seems like an unnecessary complication though. I do realise that
Docker Machine is in beta and this might be something they fix in the full release. In the meantime this issue is something to be aware of when running on OSX.

As this seems to be a Docker Machine issue, when deploying to production on a Linux/Unix box you can map a volume to a host directory as per usual without any dramas.