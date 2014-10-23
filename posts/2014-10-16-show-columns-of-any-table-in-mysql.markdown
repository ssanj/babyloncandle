---
title: Show columns of any table in MySQL
author: sanjiv sahayam
description: How to show the columns of any table in MySQL.
tags: mysql
---

Frequently I need to look up some information about the columns of a given database table. I usually resort to looking up the [MySQL docs](http://dev.mysql.com/doc/refman/5.0/en/show-columns.html). I thought I'd document it here for easy access.

The general syntax is:

show columns from __table__ from __db__

Here's a simple example:

    mysql> show columns from quote from quotedb;
    +--------+---------------+------+-----+---------+----------------+
    | Field  | Type          | Null | Key | Default | Extra          |
    +--------+---------------+------+-----+---------+----------------+
    | ID     | smallint(6)   | NO   | PRI | NULL    | auto_increment |
    | QUOTE  | varchar(1000) | NO   |     | NULL    |                |
    | ATTRIB | varchar(100)  | NO   |     | NULL    |                |
    +--------+---------------+------+-----+---------+----------------+

If you are already using the database in question you can simply use:

show columns from __table__