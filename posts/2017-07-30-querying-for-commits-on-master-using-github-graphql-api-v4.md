---
title: Querying for Commits on Master using Github Graphql API v4
author: sanjiv sahayam
description: ???
tags: github, github-api, graphql
comments: true
---

//did not work

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

asked this question: https://stackoverflow.com/questions/45397333/get-last-x-commits-from-github-repo-using-github-api-v4

//worked: https://platform.github.community/t/getting-commits-parents/1965

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