---
title: How To Add Different Icons To Application Instances On Macosx
author: sanjiv sahayam
description: How to add different icons to application instances on macosx
tags: macosx
comments: true
---

I run multiple [Alacritty](https://alacritty.org/) instances for my development workflow. Each instance uses [tmux](https://github.com/tmux/tmux) to manage multiple windows - one per project. I have one Alacritty instance that runs [GitUI](https://github.com/Extrawurst/gitui) in full screen mode and the other instance runs the continuous compilation of each project in its own window. Oh, and they also run on separate monitors.

## Monitor Configuration

I have one horizontal 32" monitor which is my primary screen and a 27" monitor turned vertical for my secondary screen.

![Monitors](/images/add-different-icons-to-applications-macos/monitors.png)

### Primary Monitor

The main monitor runs Alacritty with GitUI.

![Gitui](/images/add-different-icons-to-applications-macos/gitui.png)

### Secondary Monitor

The secondary monitor runs another Alacritty instance with continuous compilation with the respective project's compiler. If the project doesn't have a compiler, the window opens to the project location from which I can run various commands.

![Continuous Compilation](/images/add-different-icons-to-applications-macos/compilation.png)


### The Issue

So what's the problem that requires different application icons per instance?

When I run two instances of Alacritty, this is what my Application Switcher (`CMD` + `TAB`) looks like:

![Application Switcher](/images/add-different-icons-to-applications-macos/application-switcher.png)

It becomes very hard to distinguish between the `Gitgui` Alacritty and the `Project` Alacritty instances. More often than not I pick the wrong one and have to try and choose a second time. It's a little annoying.

I can use [Misson Control](https://support.apple.com/en-us/HT204100) to display all the windows and choose from there.

![Mission Control - Apple.com](https://support.apple.com/library/content/dam/edam/applecare/images/en_US/macos/Catalina/macos-catalina-mission-control-add-space-callout.jpg)


The reason I don't want to do this is because I've got an old MacBook Pro and zooming into Misson Control is slow. The whole process of:

- Go to Mission Control
- Choose your Alacritty Window
- Switch to that Window

takes about 2 seconds. That's way too long to maintain any kind of flow state when coding.


### The Solution

1. Create a copy of the application folder


  For example to create an Alacritty instance for `Whatever`s I can create a copy with:

```{.terminal .scrollx}
cp -r /Applications/Alacritty.app /Applications/Alacritty-Whatever.app
```

2. Open `Finder` and browse to the freshly copied application folder (`/Applications/Alacritty-Whatever.app` in the above example)

![Alacritty-Whatever.app](/images/add-different-icons-to-applications-macos/whatever-app.png)

3. Open the information panel with `CMD` + `I`:

![Icon Placeholder](/images/add-different-icons-to-applications-macos/icon-placeholder.png)

4. Find a new image (use [Google Images](https://images.google.com), [macosicons](https://macosicons.com/#/) etc)
5. Drag the new image to the icon placeholder

![Drag in the New Image](/images/add-different-icons-to-applications-macos/drag-icon.png)

![Update Icon](/images/add-different-icons-to-applications-macos/replace-icon.png)

6. Launch Copied app through Spotlight or [Alfred](https://www.alfredapp.com/)

![Launch Copied App](/images/add-different-icons-to-applications-macos/launch-new-app.png)

7. Check the Application Switcher

![Updated Icon](/images/add-different-icons-to-applications-macos/updated-app-switcher.png)


### Final Result

I used the above steps to create custom icons for Alacritty for both my projects and GitUI instances.

![Alacritty Icons](/images/add-different-icons-to-applications-macos/custom-icons-workflow.png)

The key is to launch the Alacritty-Projects application for projects and Alacritty-GitUI application for git-related work.

### Downsides

This is the simplest way to give different application instances different icons that I have found. You have to essentially create a new application (by copying an existing one) just to change an icon for a different instance. You need one copy per instance you want to customise the icon for. This seems a little crazy.

![Make it Easier](https://media.giphy.com/media/tn9LtuEXQRJqT6dWrx/giphy.gif)

If you know of an easier/better way to do this please drop me comment.

Unfortunately any time you update the original application (Alacritty in this instance) you need to recreate your copies and do the whole updating icon dance.

### Scripting

This whole process is pretty easy to script, providing you can create an icns file for the image you require or already have an alternate icns file. But it is not as reliable as the steps outlined above, as sometimes icon caching issues makes it hard to refresh the new icons. See below for more details

#### Generating an icns File

There's a program called [mkicns](http://www.amnoid.de/icns/makeicns.html) that lets you convert images from various formats like jpg, png etc to icns files that are used for your application icons.

You can install it through [Homebrew](https://brew.sh/):

```{.terminal .scrollx}
brew install makeicns
```

You can generate an icns file for a given image with:

```{.terminal .scrollx}
makeicns -in your-image_file -out your.icns
```

There are a bunch [more options for makeicn](http://www.amnoid.de/icns/makeicns.html), so make sure to check the `--help` option for any customizations you want to make.

#### Icon Refresh Issues

Now that we have our own icns file, we can do the following:

1. Create a copy of the application folder as above
1. Copy across the custom icns file into the `Content/Resources` folder and replace the **existing** icns file you want to replace.

For example for Alacritty, the main ics file is `Content/Resources/alacritty.icns`, so you'd do:

```{.terminal .scrollx}
cp your.icns /Applications/Alacritty-Whatever.app/Content/Resources/alacritty.icns
```

Now if you try to launch your custom application with your new shiny icon you will notice that it has not been updated.

![Sad Face](https://media.giphy.com/media/OPU6wzx8JrHna/giphy.gif)

In order to do that you need to help macos understand that the application has changed and you may need to some or all of the steps below:

1. touch the application folder

```{.terminal .scrollx}
touch /Applications/Alacritty-Whatever.app
```

If that doesn't work you may also need to do the following:

2. Kill all the things

```{.terminal .scrollx}
sudo killall Finder
sudo killall Dock
```

These steps are from [Changing Mac OS X Application Icons Programmatically](https://www.sethvargo.com/replace-icons-osx/).

A full working script:

```{.terminal .scrollx}
#!/bin/bash

cp -r /Applications/Alacritty.app /Applications/Alacritty-Whatever.app
cp whatever.icns /Applications/Alacritty-Whatever.app/Content/Resources/alacritty.icns
touch /Applications/Alacritty-Whatever.app
sudo killall Finder && sudo killall Dock
```


And hopefully that should be it. But sometimes it isn't.

![Aaaaaaaaaaaaaaaargh](https://media.giphy.com/media/22CEvbj04nLLq/giphy.gif)

If that doesn't work you may need more [drastic](https://osxdaily.com/2022/05/23/clear-icon-cache-mac/) workarounds.

And at end of those drastic workarounds you might revert to just using the list of steps at the top of this article.

![Small Smile](https://media.giphy.com/media/B0vFTrb0ZGDf2/giphy.gif)

A big thank you to Apple for making all this so damn hard.

![The End](https://media.giphy.com/media/l0MYJlyOwdlT0SeU8/giphy.gif)

### Links

- [Changing Mac OS X Application Icons Programmatically](https://www.sethvargo.com/replace-icons-osx/)
- [How to Clear Icon Cache on Mac](https://osxdaily.com/2022/05/23/clear-icon-cache-mac/)
- [How do I set the icon for my application's Mac OS X app bundle?](https://stackoverflow.com/questions/646671/how-do-i-set-the-icon-for-my-applications-mac-os-x-app-bundle)
- [Icon for Mac OSX bundle](https://stackoverflow.com/questions/14362063/icon-for-mac-osx-bundle)

