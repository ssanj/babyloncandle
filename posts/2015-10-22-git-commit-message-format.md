---
title: Git Commit Message Format
author: sanjiv sahayam
description: How to write a good Git commit message.
tags: git
comments: true
---

There are [rules](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) on how to [structure](https://wiki.openstack.org/wiki/GitCommitMessages) a Git [commit message](https://github.com/phonegap/phonegap/wiki/Git-Commit-Message-Format). Here are some of the common ones.

## The 50 character subject ##

The first line of a Git commit should be 50 characters long. This is because commits were initially meant to be shared via email where this first line acted as the subject. As such it should not have an ending period.

Some reasons for following this are:

1. The output from git log --oneline is easier to read.
1. Github subjects that are over 72 characters are truncated.

![Github truncates subjects over 72 characters](/images/git_comment_wrapped_subject.png)

## The blank second line ##

The second line should be blank and separates the subject from the commit body.

This ensures proper parsing when sending commits by email and Github.

## The 72 character body ##

The following lines should be a maximum of 72 characters in length. This can include multiple paragraphs each of 72 characters which are separated by a blank line.

This helps with reading the commit message on terminals that are usually around 80 characters wide.

## Link to feature/issue ##

Another convention is to add a link to the issue or feature being worked on as the last line. This could be a link to Jira or Youtrack issue etc.

Here's an example of a Git commit message with all of the above rules:

```{.terminal .scrollx}
Add support for reverse sorting of imports

Natural sorting, sorts from upper to lowercase. This reverses the sort
order because we want packages to sort first before class names.

An example would be:
import scala.concurrent._
import ExecutionContext.Implicits.global

[https://yourtracker/project/PORT-120]
```

## Some additional rules ###

### Imperative mood ###

Some people advocate using [imperative mood](http://www.grammar-monster.com/glossary/imperative_mood.htm) for the subject. This is basically a command-like phrase:

```{.terminal .scrollx}
Add W
Fix X
Delete Y
Create Z
```

Your commit message is meant to complete the sentence:

> This commit will ...

When you write the subject in imperative mode it completes as:

> This commit will Fix X

If you wrote "Fixes X" then it won't read correctly:

> This commit will Fixes X

I've tried this for a few months now and don't see any real benefit in using one tense over the other. As long as what the commit does is clear it should be fine. Your mileage may vary.

![Docker commits using imperative mood](/images/git-comment_imperative_mood.png)


### Include issue number in subject ###

Some use the issue number as the first entry in the subject.

![Issue number at start of the subject](/images/git_comment_issue_no_in_subject.png)

Others use it somewhere in the subject.

![Issue number in the subject](/images/git_comment_issue_no_in_subject_end.png)

### Explain what and why ###

Another [guideline](http://chris.beams.io/posts/git-commit) on writing good comments is to use the body to explain the __what__ and __why__ instead of the how.

![Explain what and why](/images/git_comment_what_and_why.png)

If you find all these rules too constrictive, then [you're not alone](http://zachholman.com/posts/git-commit-history). I agree with Zach on this for personal projects. I think following the rules leads to better commit messages that are easier to understand. This is important when we need to collaborate with other developers.