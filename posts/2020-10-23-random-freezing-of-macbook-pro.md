---
title: Random Freezing of MacBook Pro
author: sanjiv sahayam
description: How I fixed my random freezes on my MacBook Pro
tags: apple, macosx
comments: true
---

Quite recently my 2017 MacBook Pro would freeze up for anywhere between five to thirty seconds. And what I mean by "freeze up" is that it would not accept any keyboard commands at all. I checked to see if the keyboard was working by pressing the `Capslock` button and it was - I could see the `Capslock` light turning on and off. Surprisingly I could still move the mouse pointer around the screen but I couldn't click on any of the UIs and make them do anything.

As you can imagine this was quite frustrating and I was wondering if I should upgrade my Macosx from `Mojave` to `Catalina`. I had been dreading this upgrade because Apple usually ends up breaking something and then I have to waste my time tending to those issues.

I also seemed to be able to quite regularly make the system freeze by switching between windows in [Sublime Text](https://www.sublimetext.com/) or moving to another window or typing in the terminal. Almost anything really, but nothing conclusive.

I had also had a look in `Activity Monitor` after a freeze and didn't find anything out of the ordinary using of a lot of CPU or RAM. I also had plenty of disc space.


# Suspects

## com.apple.hiservices-xpcservice

One of the services I did notice in `Activity Monitor` was `com.apple.hiservices-xpcservice` - which was `not responding`. I killed it manually but it didn't make things better (or worse). It was not always `not responding` after a freeze.

Some people had [different](https://apple.stackexchange.com/questions/342706/com-apple-hiservices-xpcservice-results-in-frequent-hangs-and-freezes-in-my-mac) results though.

## Alfred

The freezes were getting progressively more frequent. Suddenly one day I couldn't launch [Alfred](https://www.alfredapp.com/) with my usual key combination. I could see that `Alfred` was working and I could manually launch it by clicking on its icon in the system tray but my key combinations no longer worked :(


## Keyboard

I had recently purchased a [Durgod K320 Taurus](https://www.amazon.com.au/Durgod-Taurus-Mechanical-Gaming-Keyboard/dp/B07VZVY1NT) and I thought that maybe it was
failing - after all, most of my freezes had happened when using the keyboard to do one thing or another.

After the next freeze, I noticed that my character set had changed and now when I typed I was seeing some weird characters. This seemed to confirm my suspicions of the `Durgod` and I unplugged and replugged it into my USB hub. The character set was back to normal. So I figured that the keyboard was faulty. I did some Google searches but didn't find too many people complaining about this issue with the `Durgod`.


## Sublime Text
After a while I came across a sure-fire way to cause my laptop to freeze - Switching between windows of by various Sublime Text windows.

So maybe the problem was with Sublime Text? Given that I did most of my development in Sublime Text it might be the culprit. I found a newer version of Sublime Text than I was using and installed it. I even considered installing an alpha version of `Sublime Text 4` to see if that was any better.

Unfortunately that did not solve the problem.

## Hardware Failure
At this point I launched into a full hardware [diagnostic](https://support.apple.com/en-au/HT202731):

> Restart your Mac and then hold the `D` key when it starts up

It came back clean except for the battery which was not at a 100%. This was fine given its age.

The diagnostic recommended unplugging all peripherals before running it. So I unplugged my keyboard, mouse, 4K monitor and headphones before proceeding.

The interesting thing was that, when I used the laptop by itself, I couldn't cause the freeze by switching between `Sublime Text` windows. Wow! At least my laptop was not dying and I probably didn't have to reinstall or upgrade MacOS versions. Yay?

## Monitor

At this point it looked like the output to the 4K monitor maybe causing the issue. I did some investigation and some people had [issues](https://discussions.apple.com/thread/7370642) around using [4K monitors](https://discussions.apple.com/thread/6777878) with the Mac.

## NVRam

One of the [remedies](https://support.apple.com/en-au/HT204063) recommended with hardware issues was to reset the `NVRAM` and `PRAM`:

> NVRAM (nonvolatile random-access memory) is a small amount of memory that your Mac uses to store certain settings and access them quickly. PRAM (Parameter RAM) stores similar information, and the steps for resetting NVRAM and PRAM are the same.

I noticed that `display resolution` was among the settings that were stored in NVRAM:

> Settings that can be stored in NVRAM include sound volume, display resolution, startup disk selection, time zone and recent kernel panic information. The settings stored in NVRAM depend on your Mac and the devices that you’re using with your Mac

 I followed the recommended instructions to reset the NVRam:

- Shutdown mac
- Press the power button
- Press and hold `ALT` + `CMD` + `P` + `R` until you hear the second chime sound

And voila! All my issues were fixed!! Hooray!!


So there you have it. If you have similar issues NVRAM might be the culprit! Also try just using the laptop without any peripherals plugged in to see if any of them are causing the issue.

# Links

- [whats-causing-my-macbook-to-freeze-and-reset-keyboard-settings-when-i-bring-it](https://apple.stackexchange.com/questions/348876/whats-causing-my-macbook-to-freeze-and-reset-keyboard-settings-when-i-bring-it)
- [https://support.apple.com/en-au/HT202731](https://support.apple.com/en-au/HT202731)
- [https://www.ifixit.com/Answers/View/141693/Why+is+my+keyboard-trackpad+not+working](https://www.ifixit.com/Answers/View/141693/Why+is+my+keyboard-trackpad+not+working)
- [https://discussions.apple.com/thread/251312151](https://discussions.apple.com/thread/251312151)
- [https://support.apple.com/en-au/HT204063](https://support.apple.com/en-au/HT204063)
- [https://apple.stackexchange.com/questions/342706/com-apple-hiservices-xpcservice-results-in-frequent-hangs-and-freezes-in-my-mac](https://apple.stackexchange.com/questions/342706/com-apple-hiservices-xpcservice-results-in-frequent-hangs-and-freezes-in-my-mac)
- [https://discussions.apple.com/thread/7370642](https://discussions.apple.com/thread/7370642)
- [https://discussions.apple.com/thread/6777878](https://discussions.apple.com/thread/6777878)