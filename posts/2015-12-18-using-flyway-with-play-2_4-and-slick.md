---
title: Using Flyway with Play 2.4.x and Slick 3.x
author: sanjiv sahayam
description: How to integrate a Play 2.4.x application with Slick 3.x and use Flyway for database migrations.
tags: activator, flyway, play, sbt, scala, slick
comments: true
---

Recently I wanted to use [Flyway](http://flywaydb.org) to manage my database migrations for a Play 2.4 application using slick 3.x. Unfortunately as this stack is quite new, the [play-flyway](https://github.com/tototoshi/play-flyway) plugin no longer works with Play 2.4.

Fortunately there is a module called [flyway-play](https://github.com/flyway/flyway-play) which integrates Flyway into Play 2.4. Unfortunately there is no Slick 3.x support at present. But the good news is that there is a [PR that adds support for Slick 3.x](https://github.com/flyway/flyway-play/pull/18). The bad news is that this PR has been languishing unmerged since September 15 2015. So if you are desperately in need of this support, you might want to go ahead and patch it yourself locally and be done with it until it is merged.

Another alternative is to use the [Flyway sbt plugin](http://flywaydb.org/documentation/sbt) to manage your migrations. It needs a few tweaks to get it to work with Slick 3.x so that you can reuse configurations from your __application.conf__ file.

_These steps are for sbt version 0.13.8. You may need to tweak them if you are using another version of SBT_

Here are the steps required to get this working.

1. Add the Flyway resolver and plugin to your project/plugins.sbt file:

```{.scala .scrollx}
resolvers += "Flyway" at "http://flywaydb.org/repo"

addSbtPlugin("org.flywaydb" % "flyway-sbt" % "3.2.1")
```

2. Create a flyway.sbt file in your project root to link up your application.conf database settings with Flyway:

```{.scala .scrollx}
import sbt._
import com.typesafe.config.ConfigFactory

lazy val flywayDBName = "YOUR_DB_NAME"

lazy val flywayDbConf = settingKey[(String, String, String)]("Typesafe config file with slick settings")

flywayDbConf := {
    val cfg = ConfigFactory.parseFile((resourceDirectory in Compile).value / "application.conf")
    val prefix = s"slick.dbs.${flywayDBName}.db"
    (cfg.getString(s"$prefix.url"), cfg.getString(s"$prefix.user"), cfg.getString(s"$prefix.password"))
}

flywaySettings

flywayUrl := flywayDbConf.value._1
flywayUser := flywayDbConf.value._2
flywayPassword := flywayDbConf.value._3
```

And basically you are done! :) You should be able to run flywayMigrate in SBT and have it use all the Slick settings you've defined in your conf/application.conf file:

```{.terminal .scrollx}
flywayMigrate
```

What about if you want to automatically run migrations when you run up your Play application?

3. I've create a task that displays the existing Flyway migration status and prompts you whether to run a migration. If you choose 'y' it then runs any pending migrations and starts the Play application. To do this update your build.sbt file with:

```{.scala .scrollx}
lazy val migrate = taskKey[Unit]("Migrate DB with Flyway")
migrate := Def.taskDyn {
  flywayInfo.value
  println("do you want to migrate the schema? y/n")
  val confirm = readLine()
  if (confirm == "y") {
    Def.task {
      flywayMigrate.value
      flywayInfo.value
    }
  } else {
    Def.task { println("-- skipping migration --") }
  }
}.value

addCommandAlias("runM", ";migrate;run")
```

Now you can run migrations and then start your Play application through SBT with:

```{.terminal .scrollx}
runM
```

### Running migrations ###
<iframe class="movie" src="https://player.vimeo.com/video/149355602" width="800" height="681" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

### Skipping migrations ###
<iframe class="movie" src="https://player.vimeo.com/video/149355601" width="800" height="681" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>