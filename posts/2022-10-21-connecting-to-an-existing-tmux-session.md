---
title: Connecting To An Existing Tmux Session
author: sanjiv sahayam
description: Connecting to an existing tmux session
tags: tmux
comments: true
---

One of the cool things about [tmux](https://github.com/tmux/tmux) is that, even if you close the terminal that tmux runs in, the tmux server process keeps your session around. That also means that you can reconnect to it from another terminal.

If you accidentally or purposely closed a terminal session that was running tmux, you can simply launch a new terminal instance and list your existing tmux sessions with:


```{.terminal .scrollx}
tmux ls
```

You might get a list of the sort:

```{.command .scrollx}
git: 2 windows (created Tue Oct  4 09:30:12 2022) (attached)
projects: 6 windows (created Tue Oct  4 09:30:44 2022) (attached)
```

Now to connect to any of the existing session simply use:


```{.terminal .scrollx}
tmux attach -t <session_name>
```

For example to reconnect to the `projects` tmux session use:

```{.terminal .scrollx}
tmux attach -t projects
```

And you're back to where you left of!

There's even a way to [Reconnect to Broken tmux Session ](https://timvisee.com/blog/reconnect-to-broken-tmux-session/)
