---
title: Docker Cheat Sheet
author: sanjiv sahayam
description: Frequently used Docker and Docker Machine commands.
tags: docker
comments: true
---

Here are some common [Docker](http://www.docker.com) and [Docker-Machine](https://docs.docker.com/machine) commands. You only need Docker Machine if you are on OSX.

## Docker Machine ##

Create the default machine:
```{.terminal .scrollx}
docker-machine create --driver virtualbox default
```

The default machine is created at:
```{.terminal .scrollx}
~/.docker/machine/machines/default
```

List machines:
```{.terminal .scrollx}
docker-machine ls
```

Show environment properties of machine:
```{.terminal .scrollx}
docker-machine env machine_name
```

Shows ip of machine:
```{.terminal .scrollx}
docker machine ip machine_name
```

Start a machine:
```{.terminal .scrollx}
docker-machine start machine_name
```

SSH into a machine:
```{.terminal .scrollx}
docker-machine ssh machine_name
```

Show detailed information about a machine:
```{.terminal .scrollx}
docker-machine inspect machine_name
```

## Docker ##

Create a terminal bound to machine. _You have to do this before you run any docker commands_:
```{.terminal .scrollx}
eval "$(docker-machine env machine_name)"
```

Create a tagged image from a Dockerfile:

```{.terminal .scrollx}
docker build -t repository/image:tag .
```

Run a container as a daemon:
```{.terminal .scrollx}
docker run -d ...
```

Run a container exposing ports:
```{.terminal .scrollx}
docker run -p external_port:internal_port ...
```

Run a contain exposing random ports:
```{.terminal .scrollx}
docker run -P (maps internal port to random external port) ...
```

Run a container with a mapped volume:
```{.terminal .scrollx}
docker run -v host_path:container_path ...
```

Run a container with environment arguments:
```{.terminal .scrollx}
docker run -e ENV_NAME_1=value1 -e ENV_NAME_2=value2 ...
```

Run a container with mapped ports, volumns and an enviroment variable:
```{.terminal .scrollx}
docker run --name some-mysql \
           -p 3306:3306 \
           -v /mydata:/var/lib/mysql \
           -e MYSQL_ROOT_PASSWORD=some_secret \
           -d mysql
```

Connect to a running container:
```{.terminal .scrollx}
docker exec -it container_name bash
```

List Docker processes:
```{.terminal .scrollx}
docker ps
```

List last run container:
```{.terminal .scrollx}
docker ps -l
```

List all containers running or otherwise:
```{.terminal .scrollx}
docker ps -a
```

List Docker images:
```{.terminal .scrollx}
docker images
```

Start a Docker container:
```{.terminal .scrollx}
docker start container
```

Stop a Docker container:
```{.terminal .scrollx}
docker stop container
```

Restart a stopped container:
```{.terminal .scrollx}
docker restart container
```

Remove a Docker container:
```{.terminal .scrollx}
docker rm container
```

Show logs of a container:
```{.terminal .scrollx}
docker logs -f container
```

Show port mappings for a container:
```{.terminal .scrollx}
docker port container
```
