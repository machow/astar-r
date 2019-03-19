Minecraft Path Finding Example
==============================

## Server Setup

```
docker build -t mc-example .
docker run --name spigot -it -p 25565:25565 -p 4711:4711 -e EULA=true mc-example
```

**Note:** It's helpful to add yourself as a server admin..

```
# to see issue, use: docker exec spigot mc_log
docker exec -it spigot /bin/bash

# in container ----
rm /minecraft/ops.json
echo <YOUR_USERNAME> >> /minecraft/ops.txt
mc_restart
exit
```

**Useful Game Commands:**

* set creative mode: `/gamemode c`
* teleport: `/tp PLAYER e/w u/d n/s`
* daytime please: `/set time 0`

## Running Example

```
# remotes::install_github('machow/astar-r')
# remotes::install_github('ropensci/miner')
# run, or open in file editor
source('minecraft.R')
```
