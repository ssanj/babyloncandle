---
title: Basic Mysql Installation and Configuration
author: sanjiv sahayam
tags: linux, mysql, ubuntu
---

Here are some basic installation and configuration options for mysql on Ubuntu.

Install the latest mysql with

```
sudo apt-get update
sudo apt-get install mysql-server
```

__Note__: _Ensure you remember the root password specified while installing mysql._

Once the installation completes ensure you can log into your mysql installation.

```
mysql -u root -p
```

Some basic defaults to configure would be to use UTF-8 as your character encoding and expose
the mysql server so it can be accessed externally. Before you modify any configuration settings,
stop your mysql server with:

```
sudo service mysql stop
```

Edit your mysql config file __/etc/mysql/my.cnf__ and add the following under the __[mysqld]__ section:

```
default-storage-engine=InnoDB
skip-character-set-client-handshake
collation-server=utf8_unicode_ci
character-set-server=utf8
bind-address = your_server_ip
```

__Note__: __skip-character-set-client-handshake__ implies to use the server default character set
irrespective of what the client specifies. Match __your_server_ip__ to the ip address of the
machine you are running mysql on. The __default-storage-engine__ has been set to __InnoDB__ to enable transactional behaviour. If you need to swap in another database simply change the [value](http://dev.mysql.com/doc/refman/5.0/en/storage-engines.html) to the one you require.

Restart mysql with:

```
sudo service mysql start
```

Verify your default database engine with:

```
show engines;
```

The default database engine will have a value of __DEFAULT__ under the __Support__ column.

```{.scrollx}
+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+
| Engine             | Support | Comment                                                        | Transactions | XA   | Savepoints |
+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+
| MyISAM             | YES     | MyISAM storage engine                                          | NO           | NO   | NO         |
| MRG_MYISAM         | YES     | Collection of identical MyISAM tables                          | NO           | NO   | NO         |
| MEMORY             | YES     | Hash based, stored in memory, useful for temporary tables      | NO           | NO   | NO         |
| BLACKHOLE          | YES     | /dev/null storage engine (anything you write to it disappears) | NO           | NO   | NO         |
| CSV                | YES     | CSV storage engine                                             | NO           | NO   | NO         |
| FEDERATED          | NO      | Federated MySQL storage engine                                 | NULL         | NULL | NULL       |
| ARCHIVE            | YES     | Archive storage engine                                         | NO           | NO   | NO         |
| InnoDB             | DEFAULT | Supports transactions, row-level locking, and foreign keys     | YES          | YES  | YES        |
| PERFORMANCE_SCHEMA | YES     | Performance Schema                                             | NO           | NO   | NO         |
+--------------------+---------+----------------------------------------------------------------+--------------+------+------------+
```

Now let's create your first database. Log into your installation as root with the password specified during installation

```
mysql -u root -p
```

Create a database with:

```
CREATE DATABASE database_name
```

__Note__: Replace database with the name of the database you want to create.

Ensure the database has been created with:

```
show databases;
```

Ensure the characterset of the database is UTF-8:

```
use database_name;
show variables like 'char%';
```

Create a local user switch to the mysql db:

```
use mysql;
```

and then execute:

```{.scrollx}
CREATE USER 'your_user'@'your_server_ip' IDENTIFIED BY 'your_local_password';
GRANT ALL PRIVILEGES ON your_database.* TO 'your_user'@'your_server_ip' WITH GRANT OPTION;
```

Note: Substitute __your_user__, __your_server_ip__, __your_local_password__ and __your_database__ with values appropriate values.

A local user allows you to log into the mysql server only from the server. If you want to log into the mysql server remotely you also need to create a remote user:

```
CREATE USER 'your_user'@'%' IDENTIFIED BY 'your_remote_password';
GRANT ALL PRIVILEGES ON your_database.* TO 'your_user'@'%' WITH GRANT OPTION;
```

Note: Substitute __your_user__, __your_server_ip__, __your_remote_password__ and __your_database__ with values appropriate values. The main difference between local and remote users is that the remote user connects from __%__ not the __your_server_ip__ address.

To verify privileges for the above accounts use:

```
SHOW GRANTS FOR 'your_user'@'your_server_ip';
SHOW GRANTS FOR 'your_user'@'%';
```

To drop a user do:

```
DROP USER 'your_user'@'your_server_ip';
DELETE FROM USER WHER USER='your_user';
```
If you keep getting the following error message when you try to login:

```{.scrollx}
ERROR 1045 (28000): Access denied for user 'your_user'@'your_server' (using password: YES)
```

and you are sure your password is correct, you could have 1 of 2 problems:

1. Verify that the server the error message specifies is the same as that as the user you created it for.

Eg. If you created 'your_user'@'10.5.2.1' and the error message says 'your_user'@'domainname' then you need
to create the user for the specified server name or use the following connection string:

```
mysql -u your_user -p -hserver_name
```

2. Your password could have special characters that seem to befuddle mysql sometimes. Try changing the password to a plain alpanumeric one and see if you can login then.

```
UPDATE USER SET PASSWORD=PASSWORD("YOUR NEW PASSWORD") WHERE USER="your_user";
```

The above configuration should give you enough information to get started on your own projects.