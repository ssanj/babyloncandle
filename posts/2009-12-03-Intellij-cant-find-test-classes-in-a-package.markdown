---
title: Intellij can&apos;t find Test Classes in a Package
author: sanjiv sahayam
description: How to fix the &quote;there are no tests in package&quote; issue in Intellij 8.1.3
tags: intellij, java
---

 I came across a very annoying problem while using Intellij 8.1.3 today. I could run all the tests from the source package until I introduced a compilation error. After fixing the compilation error, I could no long run all the tests from the source package. Intellij kept insisting that "there are no tests in package xyz" .. which of ofcourse there where.

I had a quick look around the intellij configuration files (usually in your __home_directory/.IntelliJIdea8x/system directory__) and I came across 2 cache directories:

1. caches
2. compiler

Both had cached files of the project I was working on. The cache files are of the format: __project_name_unique_number__.

I deleted the cached files of my project from each of the above directories and restarted intellij. This fixed the problem of not finding any tests! :)