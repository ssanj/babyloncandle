---
title: How to SSH into a Docker Machine
author: sanjiv sahayam
description: How to ssh into a docker machine.
tags: docker
comments: true
---

While you can use

```{.terminal .scrollx}
docker-machine ssh <machine_name>
```

to ssh into a machine, I was wondering how to use vanilla ssh to achieve the same. I stumbled across a [Docker Github issue](https://github.com/docker/machine/issues/1758) that had a very useful snippet by [emilingerslev](https://github.com/emilingerslev):

```{.terminal .scrollx}
machine=machine_name; ssh -i ~/.docker/machine/machines/$machine/id_rsa docker@$(docker-machine ip $machine)
```

That's all there is to it!