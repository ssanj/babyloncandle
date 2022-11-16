---
title: macOS Loses Connection To Bluetooth Mouse
author: sanjiv sahayam
description: How to fix macOS Blueooth connection issues
tags: alfred, macosx, bluetooth
comments: true
---

I am not a big fan of using Bluetooth for connecting to peripherals. They inevitably stop working at some point and
you're left without a working keyboard or mouse or both.

Recently I suspended my scepticism and purchased the [Logitech Lift for Mac](https://www.logitech.com/en-au/products/mice/lift-vertical-ergonomic-mouse-mac.910-006470.html). I was getting some RSI and wanted to try out this vertical mouse.

![Logitech Lift for Mac](/images/macos-loses-connection-to-bluetooth-mouse/logitech-lift-for-mac.png)

The mouse is exceptional. It was working really well, until one day, as expected, the mouse could not be detected by macOS (Monterey).

> Curse your sudden but inevitable betrayal!

Since I knew this day would come, I had a handy wired USB mouse close by. Surprisingly even after disabling and enabling
Bluetooth the mouse would not pair on its previous channel.

I changed channels and finally got the mouse to pair. This was quite annoying as now I had two channels bound to
the same computer. I did some Googling and found that [killing the Bluetooth daemon](https://apple.stackexchange.com/questions/251842/how-to-restart-bluetooth-service-from-command-line) seemed to solve the problem. Once the daemon is killed, a new instance is automatically started.

## Script it

I created a small Bash script to do that. In order to use the script I had use `gnu-sed` as opposed to the default `sed` implementation that ships with macOS, because I wanted to do some Regex replacements.

You can install it via [brew](https://brew.sh/):

```{.terminal .scrollx}
brew install gnu-sed
```

If you want to make `gnu-sed` your default `sed`, then you can just add it to your path as mentioned in  [this](https://medium.com/@bramblexu/install-gnu-sed-on-mac-os-and-set-it-as-default-7c17ef1b8f64) article.


The kill script: `kill-bluetooth.sh` is as follows:

```{.bash .scrollx}
#!/bin/bash

PROC_ID=$(ps aux | grep 'bluetoothd' | grep -v 'grep' | /usr/local/opt/gnu-sed/libexec/gnubin/sed -E 's/\s+/ /g' | cut -d' ' -f2)

echo "bluetoothd Proc Id: $PROC_ID"

sudo kill -9 "$PROC_ID"
```

These are the steps followed in the script:

1. Find any processes named "bluetoothd" (`ps aux | grep 'bluetoothd'`)
1. Unfortunately this also returns the grep we ran above. Exclude it. (`grep -v 'grep'`)
1. Any time we have more than a single space, replace it with a single space (`sed -E 's/\s+/ /g'`)
1. Split the output by spaces and get the second field which is the process id(`cut -d' ' -f2`)
1. Assign the process id to `PROC_ID`
1. Write out `PROC_ID` to the terminal (`echo "bluetoothd Proc Id: $PROC_ID"`)
1. Kill the process with the process id of `PROC_ID` (`sudo kill -9 "$PROC_ID"`)


I've explicitly specified the path to the `gnu-sed` implementation because I want to run this script through [Alfred](https://www.alfredapp.com/). More on that later.

Running the above script fixes the problem when Bluetooth fails to detect my mouse!

![Hooray](https://media.giphy.com/media/uJw7UcWYutgQM/giphy.gif)

## Make Alfred do it

But what if we could make it even easier? And we can, by running it through an [Alfred Workflow script](https://www.alfredapp.com/help/workflows/actions/run-script/).

One thing we will need is to give the script administrative privileges in order to run the `sudo` command. We can do this by wrapping it in an [AppleScript](https://developer.apple.com/library/archive/technotes/tn2065/_index.html#//apple_ref/doc/uid/DTS10003093-CH1-TNTAG1-HOW_DO_I_PASS_AN_APPLESCRIPT_VARIABLE_TO_MY_SHELL_COMMAND_):

```{.command .scrollx}
do shell script "/bin/bash PATH_TO_SCRIPT/kill-bluetooth.sh" with administrator privileges
```

![Alfred Workflow Script](/images/macos-loses-connection-to-bluetooth-mouse/alfred-workflow-script.png)


This will now prompt you for the administrator password when running the script.

And now you can simply define a keyword to such as "Relaunch Bluetooth" to launch your Alfred Workflow and restart your `bluetoothd` daemon.

![Alfred Workflow](/images/macos-loses-connection-to-bluetooth-mouse/alfred-workflow.png)

![Relaunch Bluetooth](/images/macos-loses-connection-to-bluetooth-mouse/relaunch-bluetooth.png)
