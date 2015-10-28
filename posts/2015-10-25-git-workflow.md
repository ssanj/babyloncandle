---
title: Git Workflow
author: sanjiv sahayam
description: A summary of a Gitflow variation.
tags: git
comments: true
---

There are quite a [few](https://www.atlassian.com/git/tutorials/comparing-workflows/) git workflows out there. [Vincent Driessen](http://nvie.com/posts/a-successful-git-branching-model) came up with the first Gitflow that I followed for my own projects. Other companies like Github use a [simpler version](http://scottchacon.com/2011/08/31/github-flow.html) to that:

* Anything in the master branch is deployable.
* To work on something new, create a descriptively named branch off of master (ie: new-oauth2-scopes).
* Commit to that branch locally and regularly push your work to the  same named branch on the server.
* When you need feedback or help, or you think the branch is ready for merging, open a pull request.
* After someone else has reviewed and signed off on the feature, you can merge it into master.
* Once it is merged and pushed to ‘master’, you can and should deploy immediately.

Even with the variety of flows linked to above, we are experimenting with yet another variant at my current workplace. I prefer the simplicity of the Gihub flow but we had additional requirements like deploying to staging that it does not seem to support. Here are the steps we used:

* To work on something new, create a descriptively named branch off of develop (ie: new-oauth2-scopes).
* Commit to that branch locally and regularly push your work to the  same named branch on the server. Committing to the server will run an automated build and test suite.
* When you need feedback or help, or you think the branch is ready for merging, open a pull request against develop.
* At this point your PR will be reviewed by one or more developers. You make any changes suggested and push again to verify that your reviewers are happy with the changes.
* Once your PR is merged into develop, another automated build and test suite will run. Once it passes, your build is deployed to a staging environment for additional scripted or manual testing.
* When the changes of your branch need to be pushed to production, a PR is created between develop and master.
* Someone will review all the changes from develop that will be merged into master. There could be many so it's a good idea to have at least a glance at everything that will hit master. Once the PR is merged into master it will run an automated build and test suite.
* Once the master build passes we create a Github release with release notes and a tag.
* When we are ready to deploy the changes from master to production we manually kick off a build to create our production deployment.

![Git Workflow](/images/git_workflow_2.png)

