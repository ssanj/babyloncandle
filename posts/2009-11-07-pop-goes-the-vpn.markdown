---
title: Pop goes the VPN
author: sanjiv sahayam
description: If your vpnc stops working have a look at your dns resolution.
tags: linux, ubuntu, vpn
---

Recently my vpn stopped working for no apparent reason. I could connect to the vpn using vpnc, but from there I couldn't ping any of the machines on the network. I had not changed any settings so it was quite puzzling. Friends' using Windows clients were able to connect with the same vpn parameters without any problems. It looked like a dns lookup problem.

Running: ```cat /etc/resolv.conf``` confirmed that the vpn dns was not being used.

After googling around a bit I came across [this](http://prystash.blogspot.com/2009/09/vpnc-linux-vpnc-no-response-from-target.html) site which gave me a clue on how to configure my dns look ups. I just had to add the following line to my vpnc config file:

__NAT Traversal Mode cisco-udp__

Now when I ran vpnc, and ran ```cat /etc/resolv.conf```, it confirmed that my vpn dns was used! :)

The complete vpnc config file looks like this:

```
IPSec gateway xxx.xx.xxx.xxx
IPSec ID your_id
IPSec secret your_secret
IKE Authmode psk
NAT Traversal Mode cisco-udp
```

