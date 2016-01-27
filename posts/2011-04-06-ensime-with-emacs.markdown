---
title: Ensime with Emacs
author: sanjiv sahayam
description: How to setup Enime to work with Emacs for Scala development.
tags: sbt, scala
---

I've been toying with the idea of looking for an alternative to Intellij for Scala development for a while now. I tried to use [Ensime](https://github.com/ensime/ensime-emacs) with Emacs a few months ago and never got it going for one reason or another. More recently, I've got Ensime and Emacs working together and thought I'd blog about it for anyone else who had trouble getting everything to work together.

Here are the minimum requrements as stated on the Ensime user [manual](http://aemon.com/file_dump/ensime_manual.html):

1.Unix(y) or Windows OS
2.JVM Version 6
3.Scala 2.8.1 compatible source and libraries
4.Emacs 22 or later (23 is recommended)


__Configuring Emacs for Scala__


1. Ensure you have a working installation of scala 2.8.1final.

2. Install emacs. On Ubuntu you can do this with

```
sudo apt-get install emacs
```

and on the Mac you can:

```
brew install emacs
```

Ensure you have at least version 22 or later.

3. Go to your __scala_installation_dir/misc/scala-tool-support/emacs/__ directory. Copy all __.el__, __.elc__ files and the __Makefile__ into a location where you want to store these files.

Eg. ~/scalaemacs

4. Copy the __contrib/dot-ctags__ file to your __~/.ctags__ file

5. Using a command shell, change to the above directory and run __make__ to convert the __.el__ files to __.elc__ files.

6. Add the following to your __~/.emacs__ file:

```
(add-to-list 'load-path "/path_to_your_elc_files")
(require 'scala-mode-auto)
```

eg:

```
(add-to-list 'load-path "~/scalaemacs")
(require 'scala-mode-auto)
```

You may need to create this file if it does not exist.

7. Open a .scala file in emacs to verify syntax highlighting works and other basic scala functionality works.

__Installing Ensime__

1. Ensime can be downloaded from [here](https://github.com/aemoncannon/ensime/downloads). Download the latest version. The current version is -> __ensime_2.8.1-0.5.0.tar.gz__

2. Extract the archive downloaded to a know location. This will be your __ENSIME_ROOT__.

Eg. ~/opt/ensime

3. Verify that the __ENSIME_ROOT/bin/server.sh__ file has execute permissions.

4. Add the following to your __~/.emacs__ file substituting __ENSIME_ROOT__ for where you extracted the archive:

```
;; Load the ensime lisp code...
(add-to-list 'load-path "ENSIME_ROOT/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO:
;; Open .scala file. M-x ensime (once per project)
```

Your complete .emacs files should look something like:

```
(add-to-list 'load-path "~/scalaemacs")
(require 'scala-mode-auto)

;; Load the ensime lisp code...
(add-to-list 'load-path "~/opt/ensime/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO:
;; Open .scala file. M-x ensime (once per project)
```

__Creating a Project with SBT__

You need to create an Ensime project for each project you want to use with Ensime. The project details are written to a .ensime file in your project root. Ensime currently has support for SBT, Maven and Ivy. If you don't have a build system you can also generate a .ensime file through the wizard or by hand.

The following is how to create a project for an existing SBT project:

1. Launch emacs
2. Type M-x to open the mini-buffer and then type: __ensime-generate-ensime-config-gen__.
A note on the Meta key (or M-) combinations: On linux M-x is Alt+x, while on the Mac it's Esc+x. Play around until you find which meta key is used on your flavour of OS.
3. Specify the root of your project.
4. If your project is an SBT project, it automatically detects most settings and you should see a message like:
"Your project seems to be of type 'sbt', continue with this assumption? (yes or no)". Choose yes.
5. Enter all the other information requested.
6. At the end you will see something like "Your project config has been written to /xyz/.ensime. Use 'M-x ensime' to launch ENSIME." Your ensime file has been written and you are ready to use ensime.
7. Type M-x and in the mini-buffer type: ensime to launch the Ensime server for your project. Reconfirm the location of your project.
8. That's it! :)

__Neat Features__


1. Type inspection - will dive into details of the type at the cursor
2. Automatic member completion (eg. typing "blah". followed by the Tab key will give you a list of the methods on String.
3. Navigation between sources
4. Refactoring (Renaming, Optimizing imports etc)
5. Source formatting
6. SBT support
7. Dropping files into the Scala REPL
8. Debugging (I haven't had much luck getting this to work)

For a full list have a look at the online Ensime user [manual](http://aemon.com/file_dump/ensime_manual.html).

![screenshot of ensime's autocomplete feature](/images/ensime_autocomplete.jpg)