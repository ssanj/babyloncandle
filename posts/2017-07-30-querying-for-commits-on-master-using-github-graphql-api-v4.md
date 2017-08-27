---
title: Querying for Commits on Master using Github Graphql API v4
author: sanjiv sahayam
description: Using the Github API v4 with Graphql to retrieve the latest x commits from master.
tags: github, github-api, graphql
comments: true
---

I was pretty excited to see that the new [Github API (v4) used Graphql](https://developer.github.com/v4/). I wanted to retrieve the last x commits from master for a project I was working on. Should be easy right? After a bunch of time wasted on [GitHub's GraphQL Explorer](https://developer.github.com/v4/explorer/) I came up with the following which didn't really solve my issue:

```{.javascript .scrollx}
query{
  repository(owner: "typelevel", name: "cats") {
    defaultBranchRef {
      name
      prefix
      associatedPullRequests(states: [MERGED], last: 5) {
        edges {
          node {
            title
          }
        }
      }
    }
  }
}
```

It was limited to PRs and didn't really seem like a nice solution.

I then [asked this question on Stackoverflow](https://stackoverflow.com/questions/45397333/get-last-x-commits-from-github-repo-using-github-api-v4).

Meanwhile while rummaging around the [Github platform Forum](https://platform.github.community) I came across [this post](https://platform.github.community/t/getting-commits-parents/1965) that gave me some clues:

```{.javascript .scrollx}
{
  repository(owner: "golang", name: "go") {
    defaultBranchRef {
      target {
        ... on Commit {
          history(first: 10) {
            pageInfo {
              hasNextPage
              endCursor
            }
            edges {
              node {
                oid
                messageHeadline
              }
            }
          }
        }
      }
    }
  }
}
```

So I modified the above to suit my needs and came up with this query that returned the most recent commits on master:

```{.javascript .scrollx}
query {
  repository(owner: "typelevel", name: "cats") {
    ref(qualifiedName: "master") {
      target {
        ... on Commit {
          history(first: 10) {
            pageInfo {
              hasNextPage
              endCursor
            }
            edges {
              node {
                oid
                messageHeadline
              }
            }
          }
        }
      }
    }
  }
}
```

Graphql was supposed to make understanding the structure of an API much easier with automatically generated documentation and tools like [Graphiql](https://github.com/graphql/graphiql). What still seems to be missing is how to sort through [interfaces](http://graphql.org/learn/schema/#interfaces) and their implementations easily to figure out what type of objects are returned from query.

For example in the Github API, the __Repository__ object implements the following other interfaces:

1. Node
1. ProjectOwner
1. Subscribable
1. Starrable
1. UniformResourceLocatable
1. RepositoryInfo

And if we just look at the __Node__ interface, we see that it has the following implementations:

1. Organization
1. Project
1. ProjectColumn
1. ProjectCard
1. Issue
1. User
1. Repository
1. CommitComment
1. Reaction
1. Commit
1. Status
1. StatusContext
1. Tree
1. Ref
1. PullRequest
1. Label
1. IssueComment
1. PullRequestCommit
1. Milestone
1. ReviewRequest
1. PullRequestReview
1. PullRequestReviewComment
1. CommitCommentThread
1. PullRequestReviewThread
1. ClosedEvent
1. ReopenedEvent
1. SubscribedEvent
1. UnsubscribedEvent
1. MergedEvent
1. ReferencedEvent
1. CrossReferencedEvent
1. AssignedEvent
1. UnassignedEvent
1. LabeledEvent
1. UnlabeledEvent
1. MilestonedEvent
1. DemilestonedEvent
1. RenamedTitleEvent
1. LockedEvent
1. UnlockedEvent
1. DeployedEvent
1. Deployment
1. DeploymentStatus
1. HeadRefDeletedEvent
1. HeadRefRestoredEvent
1. HeadRefForcePushedEvent
1. BaseRefForcePushedEvent
1. ReviewRequestedEvent
1. ReviewRequestRemovedEvent
1. ReviewDismissedEvent
1. Language
1. ProtectedBranch
1. PushAllowance
1. Team
1. ReviewDismissalAllowance
1. Release
1. ReleaseAsset
1. RepositoryTopic
1. Topic
1. Gist
1. GistComment
1. OrganizationIdentityProvider
1. ExternalIdentity
1. Blob
1. Bot
1. RepositoryInvitation
1. Tag
1. AddedToProjectEvent
1. BaseRefChangedEvent
1. CommentDeletedEvent
1. ConvertedNoteToIssueEvent
1. MentionedEvent
1. MovedColumnsInProjectEvent

That's a few too many options to manually sort through.

The [example documentation](http://graphql.org/learn/schema/#union-types) has a very simple example with a few implementations of an interface:

```{.javascript .scrollx}
{
  search(text: "an") {
    ... on Human {
      name
      height
    }
    ... on Droid {
      name
      primaryFunction
    }
    ... on Starship {
      name
      length
    }
  }
}
```

While this seems manageable it can easily get out of hand as per the Github API.

So even though Graphql does provide automatic documentation, if your domain model is complex enough, you probably still need to provide the users of your API some documentation on how everything hangs together.