---
title: Naming a tmux Session
author: sanjiv sahayam
description: How to name a tmux session
tags: tmux
comments: true
---

When you start a new [tmux](https://github.com/tmux/tmux) session with:

```{.terminal .scrollx}
tmux
```

If you list your tmux session with:

```{.terminal .scrollx}
tmux ls
```

You'll see a default name used for your sessions:
```{.terminal .scrollx}
2: 1 windows (created Sat Jul 16 15:59:49 2022) (attached)
```

In the above the name of the tmux session is **2**. Remembering these numbers can become unwheldly when you have many tmux sessions running, each with a different purpose.

The easier way to create a new session is by giving it a name of your choosing:

```{.terminal .scrollx}
tmux new -s your_cool_name_here
```

And now when you list your tmux sessions, you can easily find the session you just created:

```{.terminal .scrollx}
your_cool_name_here: 1 windows (created Sat Jul 16 16:03:46 2022) (attached)
```
