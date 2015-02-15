---
title: Logstash and Other Things
author: sanjiv sahayam
description: A presentation by Jordan Sissel of DreamHost at PuppetConf 2012
tags: link, monitoring
---

An very interesting and insightful presentation by Jordan Sissel about why and how [Logstash](http://logstash.net) came about. This is from PuppetConf 2012.

<iframe id="movie1" width="800" height="470" src="https://www.youtube.com/embed/RuUFnog29M4" frameborder="0" allowfullscreen></iframe>


1. Yearly [Sysadvent](http://sysadvent.blogspot.com.au) blog.
2. [FPM - Build packages for multiple platforms (deb, rpm, etc) with great ease and sanity](https://github.com/jordansissel/fpm)
3. There's too much data to read in a log file. We need some way of filtering it to make sense.

<p class="quote">What else sucks? Shitty error messages!</p>

4. Write better error messages.
5. Hacks work as one-offs - not everyday. Hard to maintain. You are asked to write hacks all the time.

![xkcd - regular expressions](http://imgs.xkcd.com/comics/regular_expressions.png)

6. People are using you as their computer interface.

<p class="quote">Don't be a human keyboard.</p>

7. What is a log?

<p class="quote">DATA + TIMESTAMP = LOG</p>

8. Lifecycle of a log entry: record > transmit > analyse > store > delete
9. Opensource tools:
   transport: flume, fluentd, scribe, rsyslog, syslog-ng
   search+analytics: hadoop, graylog2, elsa
   storage: hdfs, cassandra, elasticsearch

10. Use Grok:
    - named pattern: %{patternName:Name}.
    - reuse matched patterns and transformations.
    - has types: Numbers, Strings etc.
    - patterns are unit tested.
    - multiline matches for Stacktraces etc.

<p class="quote">Stop inventing shitty time formats!</p>

12. Statsd metrics can be visualized with tools like:
    - graphite
    - ganglia
    - circonus
    - boundary
    - librato
    - opentsdb
    - graylog2
13. Apache uses gettimeofday() which changes when NTP synchronizes its clock. Leads to negative time values.

<p class="quote">Does Apache have a Time Machine?</p>

14. Features:
    - Transport and process logs to and from anywhere.
    - Search and analytics.
15. Design:
    - Logstash should fit your infrastructure.
    - Logstash is extendable (via plugins).
16. Community:
    - If a newbie has a hard time it's a bug (in the code or documentation etc).
    - Contributions are more than code (file bugs, feature requests, ideas,documentation etc).
    - Tools: Kibana, puppet module, logstash cli.
17. Links:
    - [logstash.net](http://logstash.net)
    - [logstash-user@googlegroups.com](https://groups.google.com/forum/#!forum/logstash-users)
    - &#35;logstash on freenode (I am Whack)
    - [issues](https://github.com/elasticsearch/logstash/issues)
    - [&commat;jordansissel](https://twitter.com/jordansissel)
