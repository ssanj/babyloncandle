---
title: Hello World Lift
author: sanjiv sahayam
tags: lift, sbt, scala
---

Recently I've been trying to get into [Lift](http://liftweb.net). While there's a lot of information out there there's nothing that really lets a beginner start with the basics.

This post basically gives you the bare minimum configuration and dependencies to get a simple Lift application running. This example just writes some text to the main web page of your application - The ubiquitous "Hello World".

If you find any errors or inaccuracies please let me know as I'm still figuring this stuff out!

The source for this project can be found [here](http://github.com/ssanj/HelloWorldLift).

I'm using SBT for building this project.

The basic structure of this Lift project is as follows:

The SBT properties and [SBT](http://code.google.com/p/simple-build-tool) project can be found in:

```
project > build.properties
project > build > LiftProject.scala
```

The scala source can be found in:

```
src > main > scala > bootstrap > liftweb (contains a bootstrap class)
src > main > scala > au > com > testlift > snippet (contains snippet code)
```

Properties are defined in:

```
src > main > resources > props > default.props (an empty file for now)
```

The web application structure can be found in:

```
src > main > webapp (contains html pages that call snippets)
src > main > webapp > WEB-INF
src > main > webapp > templates-hidden (contains templates for your snippets)
```

With the structure out of the way, lets focus on the LiftProject file for SBT:

```{.scala}
import sbt._

class LiftProject(info: ProjectInfo) extends DefaultWebProject(info) {
val liftVersion = "2.1-RC2"

override def libraryDependencies = Set(
"net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
"net.liftweb" %% "lift-common" % liftVersion % "compile->default",
"net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
"org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default"
) ++ super.libraryDependencies

}
```

The above basically defines the minimum dependencies to get this example running. I've left out database drivers and any other dependencies we don't need. We need jetty because we are going to use it to run the example through SBT. I'm using Lift 2.1-RC2 which supports Scala 2.8.

My __build.properies__ file defines basic project attributes:

```
#Project properties
#Thu Sep 16 20:04:08 EST 2010
project.organization=blah
project.name=HelloWorldLift
sbt.version=0.7.4
project.version=1.0
build.scala.versions=2.8.0
project.initialize=false
```

As you can see we are building against Scala 2.8. That's it for project setup.

Lift looks for a boostrap class in the __bootstrap.liftweb.Boot__ class.

Boot.scala has the following contents:


```{.scala}
package bootstrap.liftweb

import net.liftweb.http.LiftRules
import net.liftweb.sitemap.{SiteMap, Menu}

class Boot {

  def boot {
    LiftRules.addToPackages("au.com.testlift")
    LiftRules.setSiteMap(SiteMap(Menu("Home") / "index"))
  }
}
```

Basically what this code is doing is:

1. Notifying Lift that the classes it needs can be found in the au.com.testlift package. Snippets can be found in __au.com.testlift.snippet__ package.
2. Setting up a __Sitemap__ or __Menu__ with a single entry named "Home" which maps to __/index__ url. You need to setup a __Menu entry__ on the __SiteMap__ for each page you wish to expose through Lift.

That's it for boot setup.

We create a HelloWorld class in the __au.com.testlift.snippet__ package with the following contents:

```{.scala}
package au.com.testlift.snippet

class HelloWorld {
  def howdy = <span>World!! The current date is: {new java.util.Date}</span>
}
```

As you can see this snippet is pretty straightforward. The howdy method simply returns a scala.xml.NodeSeq when called.

The main webpage that drives this functionality is the index.html page which can be found under src > main > webapp. Its contents is as follows:

```{.xml}
<lift:surround with="default" at="content">
<h1>Hello World Snippet</h1>
Hello <lift:HelloWorld.howdy />
</lift:surround>
```

Basically what the above block says is to get the value of the howdy method on the au.com.testlit.snippet.HelloWorld class and replace the <lift:HelloWorld.howdy> tag with what it returns. The <life:surround> defines that the <lift:HelloWorld.howdy> tag should be embedded (or surrounded with) the contents of the default.html file at a bind position named "content". The default.html file can be found under src > main > webapp > templates-hidden.

The contents of the default.html file are as follows:

```{.html}
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift='http://liftweb.net'>
<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <meta name="description" content="" />
    <meta name="keywords" content="" />
    <title>Hello World</title>
</head>
<body>
    <lift:bind name="content" />
    <lift:Menu.builder />
</body>
</html>
```

And that's all the main components to get this example running.

Issue the following commands to SBT:

```
update
jetty-run
```
If all goes well you should see something like:

```
[info] jetty-6.1.22
[info] NO JSP Support for /, did not find org.apache.jasper.servlet.JspServlet
[info] Started SelectChannelConnector@0.0.0.0:8080
```

After all the dependencies have been downloaded and the project compiled, jetty should launch on port 8080. You can verify that everything works by hitting [http://localhost:8080/index](http://localhost:8080/index) or [http://localhost:8080](http://localhost:8080)

The way I think this works is as follows:

1. The user issues the _/index_ call to the application. This maps to the src > main > webapp > index.html file.
2. When index.html is launched it invokes HelloWorld.howdy to get the contents of its inner <lift:HelloWorld.howdy> tag.
3. It then embeds that result in the src > main > webapp > templates-hidden > default.html template at the "content" bind point. It further adds the single Menu entry "Home" at the <lift:Menu.builder> tag
4. The fully resolved page is sent back to the user.

Further information on Lift can be found [here](http://www.assembla.com/wiki/show/liftweb).
