---
title: Xiaomi Mi A1 Hotspot Fix on Telstra
author: sanjiv sahayam
description: How to fix the Xiaomi Mi A1 hotspot issue on the Telstra Network.
tags: android
comments: true
---

![xma](https://i01.appmifile.com/webfile/globalimg/zh/goods/mi_a1/kind-red.jpg)

I'm really enjoying having moved to the [Xiaomi Mi A1](https://www.mi.com/in/mi-a1) from my iPhone 6. For $A250 that's an absolute steal. One issue that has plague this phone is that the hotspot does not work as advertised. This has been an issue until recently when I stumbled across a fix that worked for me:

1. Got to _Settings_ > _Network & Internet_ > _Mobile Network_ > _Advanced_ > _Access Point Names_ > _Telstra Internet_

1. Change __APN__ Type from:

```{.terminal .scrollx}
default, supl
```

to

```{.terminal .scrollx}
default,supl,dun
```

And that should be about it. Happy hotspotting! :)