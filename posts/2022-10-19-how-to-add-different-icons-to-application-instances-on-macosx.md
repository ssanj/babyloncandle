---
title: How To Add Different Icons To Application Instances On Macosx
author: sanjiv sahayam
description: How to add different icons to application instances on macosx
tags: macosx
comments: true
---

I run multiple [Alacritty](https://alacritty.org/) instances for my development workflow. Each instance uses [tmux](https://github.com/tmux/tmux) to manage multiple windows - one per project. I have one Alacritty instance that runs [gitui](https://github.com/Extrawurst/gitui) in full screen mode and the other instance runs the continuous compilation of each project in its own window. Oh, and they also run on separate monitors.

## Monitor Configuration

I have one horizontal 32" monitor which is my primary screen and a 27" monitor turned vertical for my secondary screen.

![Monitors](/images/add-different-icons-to-applications-macos/monitors.png)

### Primary Monitor

The main monitor runs Alacritty with Gitui.

![Gitui](/images/add-different-icons-to-applications-macos/gitui.png)

### Secondary Monitor

The secondary monitor runs another Alacritty instance with continuous compilation in the respective project's compiler. If the project doesn't have a compiler, the window opens to the project location from which I can run various commands.

![Continuous Compilation](/images/add-different-icons-to-applications-macos/compilation.png)


### The Issue

So what's the problem that requires different application icons per instance?

When I run two instances of Alacritty, this is what my Application Switcher (`CMD` + `TAB`) is like:

![Application Switcher](/images/add-different-icons-to-applications-macos/application-switcher.png)

It becomes very hard to distinguish between the `Gitgui` Alacritty and the `Project` Alacritty instances. More often than not I pick the wrong one and have to try and choose a second time. It's a little annoying.

I can use [Misson Control](https://support.apple.com/en-us/HT204100) to display all the windows and choose from there.

![Mission Control - Apple.com](https://support.apple.com/library/content/dam/edam/applecare/images/en_US/macos/Catalina/macos-catalina-mission-control-add-space-callout.jpg)


The reason I don't want to do this is because I've got an old MacBook Pro and zooming into Misson Control is slow. The whole process of:

- Go to Mission Control
- Choose your Alacritty Window
- Switch to that Window

takes about 2 seconds. That's way to long to maintain any kind of flow state when coding.


### The Solution

1. Create a copy of the application folder


  For example to create an Alacritty instance for `Whatever`s I can create a copy with:

```{.terminal .scrollx}
  cp -r /Applications/Alacritty.app /Applications/Alacritty-Whatever.app
```

_Now at this point you can try and update the `/Applications/Alacritty-Whatever.app/Contents/Info.plist` file with your new icon file at the following keys_:

```{.xml  .scrollx}
  <key>CFBundleIconFile</key>
  <string>your_icon.icns</string> <!-- your_icon.icns should be at Contents/Resources/your_icon.icns
-->
```

_I couldn't get this to work._ Follow the next steps for what did work.

2. Open `Finder` and browse to the freshly copied application folder (`/Applications/Alacritty-Whatever.app` in the above example)

![Alacritty-Whatever.app](/images/add-different-icons-to-applications-macos/whatever-app.png)

3. Open the information panel with `CMD` + `I`:

![Icon Placeholder](/images/add-different-icons-to-applications-macos/icon-placeholder.png)

4. Find a new image - just use [Google Images](https://images.google.com)
5. Drag the new image to the icon placeholder

![Drag in the New Image](/images/add-different-icons-to-applications-macos/drag-icon.png)

![Update Icon](/images/add-different-icons-to-applications-macos/replace-icon.png)

6. Launch Copied app through Spotlight or [Alfred](https://www.alfredapp.com/)

![Launch Copied App](/images/add-different-icons-to-applications-macos/launch-new-app.png)

7. Check the Application Switcher

![Updated Icon](/images/add-different-icons-to-applications-macos/updated-app-switcher.png)


### Final Result

![Alacritty Icons](/images/add-different-icons-to-applications-macos/custom-icons-workflow.png)


### Downsides

This is the simplest way to give different application instances different icons that I have found. You have to essentially create a new application (by copying an existing one) just to change an icon for a different instance. You need one copy per instance you want to customise the icon for. This seems a little crazy.

![Make it Easier](https://media.giphy.com/media/tn9LtuEXQRJqT6dWrx/giphy.gif)

If you know of an easier/better way to do this please drop me comment.

Unfortunately any time you update the original application (Alacritty in this instance) you need to recreate your copies and do the whole updating icon dance.

There are some ways to automate the icon creation which can get around some of this hassle, but these techniques seemed a little overkill for what they provide. YMMV. Some links are provided below:

- [How do I set the icon for my application's Mac OS X app bundle?](https://stackoverflow.com/questions/646671/how-do-i-set-the-icon-for-my-applications-mac-os-x-app-bundle)
- [Icon for Mac OSX bundle](https://stackoverflow.com/questions/14362063/icon-for-mac-osx-bundle)

