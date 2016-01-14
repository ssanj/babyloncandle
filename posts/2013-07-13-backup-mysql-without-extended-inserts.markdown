---
title: Backup MySql Without Extended Inserts
author: sanjiv sahayam
description: How to create MySql exports without lengthy inserts which lead to import errors.
tags: mysql
---

When you export a mysql database using the default configuration for mysqldump, it uses something called extended-insert format. What this basically means is that it bundles all the inserts for a particular table onto a single line with multiple value pairs. This is supposed to be faster when importing this exported data. Unfortunately this single insert line can grow to unmanageable lengths when you have a lot of data and lead to import errors.

The default settings also don't specify the column names within the inserts.

To solve both of the above problems you could run mysqldump with the following to export the content of a single database:

```{.scrollx}
mysqldump -u USER -pYOUR_PASSWORD --complete-insert --skip-extended-insert --no-create-info --compact YOUR_DATABASE > YOUR_OUTPUT_FILE.sql
```

If you need to export all your database data as insert statements do the following:

```{.scrollx}
mysqldump -u USER -pYOUR_PASSWORD --all-databases --complete-insert --skip-extended-insert > YOUR_OUTPUT_FILE.sql
```