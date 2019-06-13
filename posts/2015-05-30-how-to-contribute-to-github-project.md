---
title: How to contribute to an open source project on Github
author: sanjiv sahayam
description: Workflow needed to collaborate across an open source project
tags: git, github
comments: true
---

Something I've started doing recently is contributing to open source projects on Github. The following is an overview of the workflow needed for submitting your contributions to a project of your choice.

1. Fork the project you are interested in on github. The fork provides you with a copy of the project in your own repository. At this point the two projects are not linked.

If you perform a

```{.command}
git remote -v
```

You'll see that only your forked version of the git repository is listed.

2. Add the original project as a remote repository. We do this so we can rebase onto it later if need be.

```{.command}
git add remote upstream git://project_you_forked.git
```

If you look up remotes

```{.command}
git remote -v
```

You'll see the upstream repository listed as well.

3. Perform a fetch to retrieve the index for the upstream project.

```{.command}
git fetch upstream
```

This will update your local index with the upstream branches.

4. Create a branch from master for your feature

```{.command}
git checkout -b your_feature
```

If you do a diff between your current HEAD and the upstream/master

```{.command}
git log upstream/master..HEAD
```

you should have no differences. This is your base line. All changes from this point will become part of your pull request.

5. Implement your feature.

6. Create a pull request on your fork of the project through Github. There should be a button indicating that your feature changes can be created in a pull request.

7. After you create the pull request, the maintainer of the upstream project will get a notification of your feature modifications.

8. The simple scenario is where your changes are accepted and the upstream project merges in your changes to the upstream/master and you are done at this point. You can safely delete your fork and your feature branch.

9. The usual scenario is where other pull requests have been merged into upstream/master while you were coding away on your feature. The upstream maintainer may ask you to rebase onto upstream/master to make the merge easy.

You can do this with

```{.command}
git fetch upstream/master
```

to retrieve the latest upstream changes into your index. Then you git rebase

```{.command}
git rebase -i upstream/master
```

This will open up the rebase text editor and allow you to choose which commits to apply or fix any issues that arise.

Once you are done rebasing locally it's time to push up your changes. All your changes will be part of the pull request you created for this feature.

Since you have rebased with upstream/master, your remote/feature and your local feature branch have diverged - meaning they don't share the same ancestral commit they once did.

You can overcome this in many ways, but the simplest would be to do a force push.

## Force Push ##

```{.command}
git push origin feature --force-with-lease
```

The --force-with-lease ensures that you can't force push changes if someone else has committed to your remote branch, which would lead to lost commits.

This will update your remote/feature branch and your pull request. This will allow the upstream maintainer to merge in your changes with minimal effort and you're done.

## Create a separate branch ##

If you don't want to force push your changes, as it is generally not recommended, then you can create a new branch from your rebased changes.

After you rebase, create a new branch from the rebase:

git checkout -b your_new_branch

Run any tests that your repository may have.

Push the new branch to your remote

```{.command}
git push -u origin your_new_branch
```

Create a new pull request. Add comment in the new request and mention the old pull request number. This will help the upstream maintainer to link the two pull requests.
.
Eg. if the old pull request was #54 then add in a comment to the new pull request:

Creating this PR to replace #54, with a rebase.

Now The old PR (#54) references the new one. You can now safely close the old PR with a comment about the new PR.

And you should be done!

Happy contributing.

### References ###

[Git SCM](http://git-scm.com)
[EDX Platform](https://www.instapaper.com/read/594207036)