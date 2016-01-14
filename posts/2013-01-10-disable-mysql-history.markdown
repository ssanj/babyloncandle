---
title: Disable MySql History
author: sanjiv sahayam
description: How to prevent all your mysql commands from being recorded.
tags: mysql
---

Sometimes it's a good idea to prevent mysql from writing a history file. The mysql history file contains all the commands you type across all sessions.

Your default mysql history file is  ~/.mysql_history.

As this is an obvious security risk, it is sometimes a good idea to disable this file.

To disable mysql history do the following:

1. Delete your existing ~/.mysql_history file.

2. Edit your ~/.profile file and add the following:

```
export MYSQL_HISTFILE=/dev/null
```

This directs your mysql history to /dev/null

3. Apply your changes with:

```
source ~/.profile
```

4. Login into mysql and run some commands.

5. Logout.

6. Verify that the ~/.mysql_history file has not been created :)