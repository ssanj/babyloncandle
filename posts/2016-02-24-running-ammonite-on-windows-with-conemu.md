---
title: Running Ammonite on Windows 7 with ConEmu
author: sanjiv sahayam
description: If Ammonite fails with&#58; RuntimeException&colon; Nonzero exit value&colon; 1, here&apos;s one way to fix it.
tags: ammonite, scala, windows, conemu
comments: true
---

Trying to run [Ammonite](https://github.com/lihaoyi/Ammonite) on Windows is a nightmare. Trying to do any useful development on Windows is a huge waste of time. Why would you even bother? Unfortunately while working at a client site I was forced to use Windows. I still needed to install my usual tools, which included Ammonite. After trying to run Ammonite within [GitBash](https://git-for-windows.github.io) on [ConEmu](http://conemu.github.io/en), I was greeted with this error:

```{.terminal .scrollx}
error] (run-main-0) java.lang.RuntimeException: Nonzero exit value: 1
java.lang.RuntimeException: Nonzero exit value: 1
        at scala.sys.package$.error(package.scala:27)
        at scala.sys.process.ProcessBuilderImpl$AbstractBuilder.slurp(ProcessBuilderImpl.scala:132)
        at scala.sys.process.ProcessBuilderImpl$AbstractBuilder.$bang$bang(ProcessBuilderImpl.scala:102)
        at ammonite.terminal.TTY$.stty(Utils.scala:95)
        at ammonite.terminal.TTY$.init(Utils.scala:74)
        at ammonite.terminal.TermCore$.x$7$lzycompute$1(TermCore.scala:297)
        at ammonite.terminal.TermCore$.x$7$1(TermCore.scala:297)
        at ammonite.terminal.TermCore$.initialConfig$lzycompute$1(TermCore.scala:297)
        at ammonite.terminal.TermCore$.initialConfig$1(TermCore.scala:297)
        at ammonite.terminal.TermCore$.readLine(TermCore.scala:307)
        at ammonite.repl.frontend.AmmoniteFrontEnd.readLine(AmmoniteFrontEnd.scala:114)
        at ammonite.repl.frontend.AmmoniteFrontEnd.action(AmmoniteFrontEnd.scala:26)
        at ammonite.repl.Repl.action(Repl.scala:56)
        at ammonite.repl.Repl.loop$1(Repl.scala:89)
        at ammonite.repl.Repl.run(Repl.scala:107)
        at ammonite.repl.Main$.run(Main.scala:135)
        at ammonite.repl.Main$$anonfun$main$2.apply(Main.scala:82)
        at ammonite.repl.Main$$anonfun$main$2.apply(Main.scala:81)
        at scala.Option.foreach(Option.scala:257)
        at ammonite.repl.Main$.main(Main.scala:81)
        at ammonite.repl.Main.main(Main.scala)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(Unknown Source)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source)
        at java.lang.reflect.Method.invoke(Unknown Source)
```

After a little digging around I found the [Ammonite-REPL doesn&apos;t work on Windows](https://github.com/lihaoyi/Ammonite/issues/119) issue in the issue tracker. Oh noes!! But I had to have some Ammonite power for what I was working on. One of comments on the issue by [avakhrenev](https://github.com/avakhrenev) mentioned that swapping out the default frontend in the [Repl.scala](https://github.com/lihaoyi/Ammonite/blob/86525283be1be5896c1a6488b98ce47581005349/repl/src/main/scala/ammonite/repl/Repl.scala#L20-L22) class to JLineWindows seemed fix the problem.

After cloning the Ammonite project, I updated frontend in the repl/src/main/scala/ammonite/repl/Repl.scala file as follows:

```{.scala .scrollx}
val frontEnd = Ref[FrontEnd](ammonite.repl.frontend.FrontEnd.JLineWindows)
```

I then tested it through sbt with:

```{.command .scrollx}
sbt repl/run
```

The workaround had fixed the issue! Yay!

I proceeded to build the patched version of Ammonite with:

```{.command .scrollx}
sbt repl/assembly
```

I then copied the generated executable into my bin directory:

```{.terminal}
cp repl/target/scala-2.11/ammonite-repl-0.5.6-SNAPSHOT-2.11.7 ~/bin/amm
```

And now I can run amm as per usual on Windows.

![Ammonite on Windows](/images/ammonite_on_windows7_conemu.png)
