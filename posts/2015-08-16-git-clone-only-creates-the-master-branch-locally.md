---
title: Git Clone Only Creates the Master Branch Locally
author: sanjiv sahayam
description: Git clone only creates the master branch locally. How to access some of the other remote branches in the repository? The solution is easier than expected.
tags: git
comments: true
---

[When you clone a repository through Git, it does the  following](http://git-scm.com/docs/git-clone):

> clones a repository into a newly created directory, creates remote-tracking branches for each branch in the cloned repository (visible using git branch -r), and creates and checks out an initial branch that is forked from the cloned repository’s currently active branch.

What if you wanted some of the remote branches in addition to the "active branch" (which is usually __master__) ? 

While I initially thought that Git only downloaded the master branch, my friend [Michael](http://nippysaurus.com) showed me that that was not the case. Thanks Michael :).

So taking [my fork of the Sublime package control repository](https://github.com/ssanj/package_control_channel) as an example, when I clone the repository my branches look like this:

```{.command}
git branch -avv
```

![package control clone](/images/git_clone_repository.jpg)

It looks like we only have access to the master branch locally and that we have to remotely checkout the other branches.

The Git already has tracking information for all the remote branches following the clone. We can access any of the branches with a simple checkout.

```{.command}
git checkout branchname
```

![checking out a branch](/images/git_checkout_branch.jpg)

And that's all there is to it.