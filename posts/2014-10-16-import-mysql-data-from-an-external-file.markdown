---
title: Import MySQL data from an External File
author: sanjiv sahayam
description: How to import DDL and SQL into MySQL from an external file.
tags: mysql
---

Seeding a database is something every developer needs to do from time to time. With MySQL, running some external DDL or SQL is pretty easy. The general syntax is:

    mysql -u username -p password database_name < filename.sql

Here's a simple example:

    mysql -u datauser -p's@meth!ng_5ec^r3' datadb < seed.sql