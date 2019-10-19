---
title: Logging Objects in Javascript
author: sanjiv sahayam
description: Three different ways to log out an object to the console with varying outputs.
tags: javascript
comments: true
---

There seem to be three ways you can log out an object in Javascript and each gives you different results. A co-worker pointed out these differences to me so I thought I'd share that information here.

_These objects are logged to the Chrome console_

Let's assume we have the following __contribs__ object in scope:

```{.json .scrollx}
const contribs = {
  "data": {
    "viewer": {
      "login": "ssanj",
      "contributedRepositories": {
        "nodes": [
          {
            "name": "giter8"
          },
          {
            "name": "scalacheck"
          },
          {
            "name": "sbt-assembly"
          },
          {
            "name": "package_control_channel"
          },
          {
            "name": "dotty"
          },
          {
            "name": "scapegoat"
          },
          {
            "name": "website"
          },
          {
            "name": "ssanj.github.io"
          },
          {
            "name": "babyloncandle"
          },
          {
            "name": "ensime.github.io"
          }
        ]
      }
    }
  }
};
```

## Use toString

With

```{.terminal .scrollx}
console.log(contribs.toString());
```

we get the following:

```{.terminal .scrollx}
"[object Object]"
```

And this is not very useful. If you do see this output when you are trying to log out the contents of an object then choose on of the other two options mentioned below.

## Use JSON.stringify

With

```{.terminal .scrollx}
console.log(JSON.stringify(contribs));
```

we get:

```{.json .scrollx}
"{"data":{"viewer":{"login":"ssanj","contributedRepositories":{"nodes":[{"name":"giter8"},{"name":"scalacheck"},{"name":"sbt-assembly"},{"name":"package_control_channel"},{"name":"dotty"},{"name":"scapegoat"},{"name":"website"},{"name":"ssanj.github.io"},{"name":"babyloncandle"},{"name":"ensime.github.io"}]}}}}"
```

While that is more useful than the toString() option, it would be nice if we can pretty print the same information. Luckily that functionality is built into JSON.stringify:

```{.terminal .scrollx}
console.log(JSON.stringify(contribs, null, 2));
```

which results in:

```{.json .scrollx}
"{
  "data": {
    "viewer": {
      "login": "ssanj",
      "contributedRepositories": {
        "nodes": [
          {
            "name": "giter8"
          },
          {
            "name": "scalacheck"
          },
          {
            "name": "sbt-assembly"
          },
          {
            "name": "package_control_channel"
          },
          {
            "name": "dotty"
          },
          {
            "name": "scapegoat"
          },
          {
            "name": "website"
          },
          {
            "name": "ssanj.github.io"
          },
          {
            "name": "babyloncandle"
          },
          {
            "name": "ensime.github.io"
          }
        ]
      }
    }
  }
}"
```

## Use the object

Finally if we directly log the object:

```{.terminal .scrollx}
console.log(contribs);
```

we get the Javascript object graph for __contrib__:

![Direct Logging](/images/logging-objects-in-js/log-object-directly.jpg)

which we can then explore at our leisure.