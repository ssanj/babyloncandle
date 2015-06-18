---
title: Scala Compilation Speed on MacBook Pros
author: sanjiv sahayam
description: Scala compilation speed across a range of MacbookPros against a number of well-known projects.
tags: apple, life, scala, work
comments: true
graphs: true
---

[After deciding to take the plunge and buy the new MBP 15"](http://sanj.ink/posts/2015-05-21-between-apple-and-despair.html), I thought a little benchmarking would be a nice confirmation that my "upgrade" made sense. Most times I use my laptop for Scala development and writing my blog. I thought I'd run some tests around the Scala compilation times for some well-known projects.

Here's how I tested each project:

1. Git cloned the project.
1. Connected my laptop to the mains (to ensure no energy saving was sacrificing performance).
1. Downloaded the world with:
```{.terminal}
sbt clean compile
```
1. Ran a clean compilation four times and took the average, min and max times:
```{.terminal}
sbt "set offline := true" clean compile
```

Scala open source projects used:

1. [Scalaz](https://github.com/scalaz/scalaz)
1. [Spray](https://github.com/spray/spray)
1. [Shapeless](https://github.com/milessabin/shapeless)

* Model A => 2010 15" MBP 2.4GHZ i5, 8GB, SSD.
* Model B => 2014 15" MBP 2.2GHZ i7, 16GB, SSD.
(_This is not the 2015 model but is pretty close to it_.)


All compilation times are in seconds (s).
<!-- Remove indents because when you have four spaces it converts this block into a <pre></pre>-->
<div>
<table>
<thead>
<tr>
<th>Project</th>
<th colspan="2">Average (s)</th>
<th colspan="2">Minimum (s)</th>
<th colspan="2">Maximum (s)</th>
</tr>
<tr>
<th></th>
<th>2010</th>
<th>2014</th>
<th>2010</th>
<th>2014</th>
<th>2010</th>
<th>2014</th>
</tr>
</thead>
<tbody>
<tr>
<td class="left">Scalaz</td>
<td>234</td>
<td>77</td>
<td>232</td>
<td>76</td>
<td>264</td>
<td>79</td>
</tr>
<tr>
<td class="left">Spray</td>
<td>183</td>
<td>49</td>
<td>173</td>
<td>48</td>
<td>203</td>
<td>52</td>
</tr>
<tr>
<td class="left">Shapeless</td>
<td>189</td>
<td>57</td>
<td>178</td>
<td>55</td>
<td>208</td>
<td>59</td>
</tr>
</tbody>
</table>
</div>

<div class="caption section">Averages</div>
<div id="averageChart"></div>
<div class="caption section">Min/Max Variations</div>
<div id="variationsChart"></div>
<div class="caption section">Speed Increase</div>
<div id="speedIncreaseChart"></div>

<!-- Load d3.js and c3.js -->
<script src="../js/d3.min.js" charset="utf-8"></script>
<script src="../js/c3.min.js"></script>

<script>
c3.generate({
    bindto: '#averageChart',
    data: {
      columns: [
        ['2010 Avg', 234, 183, 189],
        ['2014 Avg', 77, 49 ,57],
      ],
      types: {
        '2010 Avg': 'bar',
        '2014 Avg': 'bar'
      }
    },
    axis: {
        x: {
            type: 'category',
            categories: ['scalaz', 'spray', 'shapeless']
        }
    },
});

c3.generate({
    bindto: '#variationsChart',
    data: {
      columns: [
        ['2010 Min', 232, 173, 178],
        ['2010 Max', 264, 203, 208],
        ['2014 Min', 76, 48, 55],
        ['2014 Max', 79, 52, 59]
      ],
    },
    axis: {
        x: {
            type: 'category',
            categories: ['scalaz', 'spray', 'shapeless']
        }
    }
});

c3.generate({
    bindto: '#speedIncreaseChart',
    data: {
        columns: [
            ['speed increase', 69]
        ],
        type: 'gauge'
    }
});
</script>
