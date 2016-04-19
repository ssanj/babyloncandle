---
title: Dash Version
author: sanjiv sahayam
description: Dash Version is a website that gives you easy access to the latest versions of your language or framework.
tags: dashversion, java, scala
comments: true
---

One of the most irksome aspects of installing a new JDK or JRE is navigating the Oracle download site. Yurg! It's always been confusing and user-non-friendly. Another annoying aspect is that, the Java updater shows up at the most inconvenient times. Mostly this just means that you don't update at the point there is a new JDK. A few weeks later you wonder how many versions behind you are.

To solve these problems, I created [Java Dash Version](http://java.dashversion.com). The name is a play on __java -version__, which displays the version of your installed JDK. It provides the following features:

1. Simple page displaying the latest version of the Oracle JDK.
1. A download link that takes you straight to the download page. No more faffing around looking for the downloads page.
1. A link to the release notes. It currently supports public and BPR (Bundled Patch Release) versions.
1. A [REST API](http://java.dashversion.com/api) to return the latest version.

![Showing the latest Java Version](/images/java-dash-version-com.png)

Example json payload:

```{.json .scrollx}
{"versions":[{"version_number":"77","version_string":"1.8.0_77","download_url":"http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html"}]}
```

In addition on Mac OSX, I have a [Bitbar](https://getbitbar.com) [Java plugin](https://getbitbar.com/plugins/Dev/Java/java_version.12h.py) that displays the your  currently installed Java version and allows you to click-through to the latest version on Dash Version.

![Bitbar Java Plugin](/images/bitbar-java-dash-version-plugin.jpg)

While I currently only support Java, in the future I plan to support additional languages and frameworks. My hope is that [Dash Version](http://java.dashversion.com) will become your first port-of-call to find out the latest version of any language or framework.

![Only Java is Currently Supported](/images/dash-version-com.png)

Give it a whirl and let me know what you think.