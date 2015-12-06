---
title: Port Forwarding with a Docker Machine
author: sanjiv sahayam
description: Remove the tedium of knowing the docker machine ip by using ssh and port forwarding.
tags: docker
comments: true
---

Something a little annoying about setting up a server on Docker Machine is that to use the service you need to know the ip of the machine:

```{.terminal .scrollx}
docker-machine ip machine_name
```

This can get tedious, specially if you are using GUI tools. In the last post [I described a way to ssh into a Docker machine](http://sanj.ink/2015-12-06-how-to-ssh-into-a-docker-machine.html). Because we have the power of ssh, we can now port forward:

```{.terminal .scrollx}
machine=machine_name; ssh -i ~/.docker/machine/machines/$machine/id_rsa docker@$(docker-machine ip $machine) -N -L local_port:localhost:machine_port
```

Now we never have to remember the ip address of the Docker machine. We can simply connect to our local port on localhost.