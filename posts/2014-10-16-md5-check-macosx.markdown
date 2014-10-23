---
title: How to verify your downloads with MD5
author: sanjiv sahayam
description: How to verify the validity of your MD5-signed downloads on Macosx.
tags: macosx, security
---

1. Download your artifact.
1. Download the SHA sums for your artifact.
1. Generate SHA sums for your downloaded artifact.
1. Verify the generated SHA of the download is found in the SHA sums file. If it is not found, your download is not to be trusted.

Let's take Vagrant 1.6.5 as an example.

1. [Download Vagrant 1.6.5.dmg](https://dl.bintray.com/mitchellh/vagrant/vagrant_1.6.5.dmg)

2. [Download the SHA256 sum file for Vagrant](https://dl.bintray.com/mitchellh/vagrant/1.6.5_SHA256SUMS?direct). The content of file is:

```bash
a94a16b9ed...38f8d826c8  vagrant_1.6.5.dmg
d79b1408be...9ab3043e40  vagrant_1.6.5.msi
78cd956742...100aebb46c  vagrant_1.6.5_i686.deb
997f69514d...84b85b07ac  vagrant_1.6.5_i686.rpm
e2c7af6d03...c9fb96a122  vagrant_1.6.5_x86_64.deb
90730fd10c...f8399852df  vagrant_1.6.5_x86_64.rpm
```

3. Generate a SHA256 against Vagrant 1.6.5.dmg:

```bash
openssl dgst -sha256 vagrant_1.6.5.dmg
```

The output will be something like:

```bash
SHA256(vagrant_1.6.5.dmg)= a94a16b9ed...38f8d826c8
```

4. Grep for your generated SHA256 within the SHA sums file:

```bash
grep 'a94a16b9ed...38f8d826c8' 1.6.5_SHA256SUMS
```

The result:
```bash
a94a16b9ed...38f8d826c8  vagrant_1.6.5.dmg
```

If a result is not found that would indicate that Vagrant download was malicious or corrupt.

