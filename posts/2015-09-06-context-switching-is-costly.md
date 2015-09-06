---
title: Context Switching is Costly
author: sanjiv sahayam
description: Thoughts on how to reduce the context switching overhead when working on multiple tasks.
tags: work
comments: true
---

Multitasking. It's almost an expected way to work these days. There have been many [articles](http://www.petrikainulainen.net/software-development/processes/the-cost-of-context-switching) [written](http://blog.codinghorror.com/the-multi-tasking-myth) on this subject and its [fallacies](http://sirupsen.com/multitasking-makes-you-dumb). There's also some interesting [commentary](https://signalvnoise.com/posts/3401-multitasking?24#comments
) on it. [A scientific study found some interesting results on multitasking](http://www.multitaskingtest.net.au/the-science/impacts-of-multi-tasking):

> His study found the average worker's functioning IQ, a temporary qualitative state, drops 10 points when multitasking. That is more than double the four-point drop that occurs when someone smokes marijuana.

Context switching is the process by which you leave an existing train of thought when working on task and take on a completely different train of thought when you start a different task. Multitasking begets context switching. The more tasks you do at once the more context switching you will have. 

[Joel Spolsky writes](http://www.joelonsoftware.com/articles/fog0000000022.html):

> As it turns out, if you give somebody two things to work on, you should be grateful if they "starve" one task and only work on one, because they're going to get more stuff done and finish the average task sooner. 

He talks about good management being:

> Good managers see their responsibility as removing obstacles so that people can focus on one thing and really get it done. When emergencies come up, think about whether you can handle it yourself before you delegate it to a programmer who is deeply submersed in a project.

So why is this important? What's so bad about asking programmers to multitask and switch contexts frequently?

> The trick here is that when you manage programmers, specifically, task switches take a really, really, really long time. That's because programming is the kind of task where you have to keep a lot of things in your head at once. The more things you remember at once, the more productive you are at programming. A programmer coding at full throttle is keeping zillions of things in their head at once: everything from names of variables, data structures, important APIs, the names of utility functions that they wrote and call a lot, even the name of the subdirectory where they store their source code. If you send that programmer to Crete for a three week vacation, they will forget it all. The human brain seems to move it out of short-term RAM and swaps it out onto a backup tape where it takes forever to retrieve.

I can definitely identify with the "zillions of things in their head" statement. When we as programmers are solving an intricate problem we have a lot of "state" to maintain. When we context switch we loose all this state and have to start afresh on something. When we come back to our previous task we need to spend a while figuring out where we were up to. We have to remember all the important nuances of what we were working on. All the gotchas. All the things "we need to remember to do".

Even with CPUs task switching has a cost:

> Actually, on real CPUs, a task switch takes a little bit of time... basically enough time to save out the state of the CPU registers and load the CPU registers for the other task.

So here's an idea: what if we "saved our state" on task before moving to our next interruption? We could do that by simply writing it down or adding it to [Jira](https://www.atlassian.com/software/jira) or [Youtrack](https://www.jetbrains.com/youtrack/) or simply having a list in [Sublime](http://www.sublimetext.com/).

In this way, just like a CPU, we would have a small context switching overhead, where we save our state. The benefit of this is that when we do come back to our task after completing our interruption, we have a cheatsheet on where we were up to and what was important to the problem at hand.

So when the next interruption comes your way, don't just drop everything that you're working on. Take the time to save your state. Make sure your manager/team leaders are aware of your new way of working. No matter how many times you are asked to switch priorities and work on different tasks, you can continue to work in a consistent and systematic way that benefits everyone.