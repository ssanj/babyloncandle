---
title: Feedback on Java Planet Enum in Haskell
author: sanjiv sahayam
description: Some feedback on my attempt at writing the Java Planet Enum example in Haskell
tags: haskell
comments: true
---

A few weeks ago I [posted](https://sanj.ink/posts/2021-08-26-java-planet-enum-in-Haskell.html) my attempt at implementing [Java's Planet Enumeration example](https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html) in Haskell. There was some useful feedback from [Reddit](https://www.reddit.com/r/haskell/comments/petl9t/encoding_the_java_planet_enumeration_example_in/), so I though I'd discuss the different implementation options mentioned.

[brandonchinn178](https://www.reddit.com/user/brandonchinn178/) mentioned some great changes. The first was to do away with `PlanetStat` and replace it with functions that pulled out the necessary data when given a Planet:

> You mentioned Java having a "mapping for free". In Haskell, you can also get mappings for free with functions; after all, functions are maps from inputs to outputs

```{.haskell .scrollx}
radius :: Planet -> Radius
radius Mercury = ...
radius ... = ...

mass :: Planet -> Mass
mass Mercury = ...
mass ... = ...
```

> This gets rid of the need for the PlanetStat data type, which I think is better, but wouldnt be good if eventually, you want to load the stats from a JSON file (for example)

This is more idiotmatic Haskell. The only down side is that you don't pull out all the information you need at the same time and have to write two functions in this case.

He also mentioned a few other changes:

> you dont need type annotations for minBound/maxBound, since you have the final result typed as [Planet]

This is true and an easy fix.

```{.haskell .scrollx}
planetValues :: [Planet]
planetValues = [minBound .. maxBound]
```

> I personally wouldnt have a newtype for literally every single function output. I would say its fine to just return a Double for surface gravity / weight

I erred on the side of "readability" for this one, but adding in a lot of newtypes does add noise.

> "massOnEarth" is misleading: its the mass regardless of what planet youre on

I am not so sure about this one. From the Java example it does seem like the mass supplied is the one on Earth which is then compared to its mass on other planets.

> instead of traversing a simple putStrLn over a formatted list of strings, I would defer the string rendering as much as possible,
> e.g.

```{.haskell .scrollx}
let planetToWeight = map (\p -> (p, surfaceWeight massOnEarth p)) [minBound .. maxBound]
    render (p, weight) = "Your weight on " <> show p <> ...
in mapM_ (putStrLn . render) planetToWeight
```

I've always found Haskell's String concatenation/interpolation a bit clunky so I've taken to separating the String generation to separate functions. But this suggestion makes a lot of sense - collect all your data and then render it once without rendering pieces of your data as you go.

> or even just go all-in with the iteration

```{.haskell .scrollx}
runPlanets earthWeight =
  forM_ [minBound .. maxBound] $ \planet -> do
    let newWeight = surfaceWeight mass planet
    putStrLn $ "Your mass on " <> show planet <> ...
  where
    mass =
      let SurfaceGravity earthGravity = surfaceGravity EARTH
      in Mass $ earthWeight / earthGravity
```
Another interesting suggestion.

I think the biggest trap I fell into while encoding the Java solution into Haskell was just that - Encoding a Java solution into Haskell. Instead, I should have solved it in the way Haskell enables you to.


[asthetaperlight](https://www.reddit.com/user/asthetaperlight/) had some ideas on the use of newtypes:

> For throwaway code like this, sure. But it would also be fine to write it as a shell script, if for some reason you really felt like it. Dimensional analysis is the static typing of physics. Not the fancy stuff that makes you want to use Haskell instead of Java - the "has literally any types at all" that makes you want to use C instead of B. Better to build good habits before you need them.

> That said, SurfaceWeight doesn't make much sense as written. Either go all in and tag it with a phantom Planet ...

`SurfaceWeight` didn't really make much sense on its own. I love the idea about saying "this is the weight on this planet" and tagging the weight with a phantom Planet.

The warning around using the above technique is funny and cautions against using unnecessary complexity:

> Then seriously rethink whatever aspect of your design made that seem necessary ...

😂

> or make it what it is: newtype Weight = Weight Double.

`SurfaceWeight` seems too specific and `Weight` seems like a more natural wrapper type.

[friedbrice](https://www.reddit.com/user/friedbrice/) has some interesting insights:

> Nice post, I hope you're enjoying Haskell.

> One thing that I find interesting, you bring up a decisive difference between object oriented programming and functional programming: how object oriented programming packages data and behavior together, and how functional programming separates data and behavior. it may seem like a bummer at first that you have to write each enum value twice, once when the data is defined, and once when the behavior is defined, but there's a very good reason that functional programming encourages this separation.

> I think we anthropomorphize the computer too much, and so I don't like calling it behavior. I like calling it interpretation, specifically interpretation of the data. One common theme of object oriented programming is that the downstream user is not allowed to interpret the data. The data is hidden from them, and the interpretation is defined solely by the upstream user. sometimes, this is exactly what you want. Haskell has many ways to hide data, not least of all are simple things like partial application and closures, hiding through scope instead of access modifiers.

> The problem is that object oriented programming insists that you always hide your data like this. most of the time, you don't want to hide your data. Your behavior, your planet stats, is one interpretation of the data. The power of functional programming is that it need not be the only interpretation.

This idea of "data" having multiple "interpretations" is pretty cool. It's something I had not thought of explicitly before. As previously mentioned by brandonchinn178, we could define a function each to get the `mass` and `radius` from a `Planet` as opposed to a `PlanetStat`. This is yet another interpretation of the `Planet` "data".

## Code with Recommendations

<details>
  <summary>brandonchinn178's recommendations</summary>

### brandonchinn178 - 1

[code](https://github.com/ssanj/java-plants-enum-in-haskell/blob/main/src/Recommendations/Brandonchinn178/Planets_1.hs)

```{.haskell .scrollx}
module Recommendations.Brandonchinn178.Planets_1(runPlanets) where

import Data.Foldable (traverse_)

-- Haskell implementation of the Java Enum: Planets example
-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

import Data.Foldable (traverse_)


data Planet = MERCURY
            | VENUS
            | EARTH
            | MARS
            | JUPITER
            | SATURN
            | URANUS
            | NEPTUNE deriving (Enum, Bounded, Show)


newtype Mass = Mass Double


newtype Radius = Radius Double


gConstant :: Double
gConstant = 6.67300E-11


radius :: Planet -> Radius
radius MERCURY = Radius 2.4397e6
radius VENUS   = Radius 6.0518e6
radius EARTH   = Radius 6.37814e6
radius MARS    = Radius 3.3972e6
radius JUPITER = Radius 7.1492e7
radius SATURN  = Radius 6.0268e7
radius URANUS  = Radius 2.5559e7
radius NEPTUNE = Radius 2.4746e7


mass :: Planet -> Mass
mass MERCURY = Mass 3.303e+23
mass VENUS   = Mass 4.869e+24
mass EARTH   = Mass 5.976e+24
mass MARS    = Mass 6.421e+23
mass JUPITER = Mass 1.9e+27
mass SATURN  = Mass 5.688e+26
mass URANUS  = Mass 8.686e+25
mass NEPTUNE = Mass 1.024e+26


surfaceGravity :: Planet -> Double
surfaceGravity planet =
    let (Mass m)   = mass planet
        (Radius r) = radius planet
    in gConstant * m / (r * m)


surfaceWeight :: Mass -> Planet -> Double
surfaceWeight (Mass otherMass) planet =
    let sg = surfaceGravity planet
    in otherMass * sg


runPlanets :: Double -> IO ()
runPlanets sampleWeight =
    let earthSurfaceGravity = surfaceGravity EARTH

        massOnEarth :: Mass
        massOnEarth = Mass $ sampleWeight / earthSurfaceGravity

        planetToWeight = map (\p -> (p, surfaceWeight massOnEarth p)) [minBound .. maxBound]
        render (p, weight) = "Your weight on " <> show p <> " is " <> (show weight)

    in mapM_ (putStrLn . render) planetToWeight
```

### brandonchinn178 - 2

[code](https://github.com/ssanj/java-plants-enum-in-haskell/blob/main/src/Recommendations/Brandonchinn178/Planets_2.hs)

```{.haskell .scrollx}
module Recommendations.Brandonchinn178.Planets_2(runPlanets) where

import Data.Foldable (forM_, traverse_)

-- Haskell implementation of the Java Enum: Planets example
-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

import Data.Foldable (traverse_)


data Planet = MERCURY
            | VENUS
            | EARTH
            | MARS
            | JUPITER
            | SATURN
            | URANUS
            | NEPTUNE deriving (Enum, Bounded, Show)


newtype Mass = Mass Double


newtype Radius = Radius Double


gConstant :: Double
gConstant = 6.67300E-11


radius :: Planet -> Radius
radius MERCURY = Radius 2.4397e6
radius VENUS   = Radius 6.0518e6
radius EARTH   = Radius 6.37814e6
radius MARS    = Radius 3.3972e6
radius JUPITER = Radius 7.1492e7
radius SATURN  = Radius 6.0268e7
radius URANUS  = Radius 2.5559e7
radius NEPTUNE = Radius 2.4746e7


mass :: Planet -> Mass
mass MERCURY = Mass 3.303e+23
mass VENUS   = Mass 4.869e+24
mass EARTH   = Mass 5.976e+24
mass MARS    = Mass 6.421e+23
mass JUPITER = Mass 1.9e+27
mass SATURN  = Mass 5.688e+26
mass URANUS  = Mass 8.686e+25
mass NEPTUNE = Mass 1.024e+26


surfaceGravity :: Planet -> Double
surfaceGravity planet =
    let (Mass m)   = mass planet
        (Radius r) = radius planet
    in gConstant * m / (r * m)


surfaceWeight :: Mass -> Planet -> Double
surfaceWeight (Mass otherMass) planet =
    let sg = surfaceGravity planet
    in otherMass * sg


runPlanets :: Double -> IO ()
runPlanets earthWeight =
  forM_ [minBound .. maxBound] $ \planet -> do
    let newWeight = surfaceWeight mass planet
    putStrLn $ "Your weight on " <> show planet <> " is " <> (show newWeight)
  where
    mass =
      let earthGravity = surfaceGravity EARTH
      in Mass $ earthWeight / earthGravity

```
</details>


<details>
  <summary>asthetaperlight's recommendations</summary>

### asthetaperlight

[code](https://github.com/ssanj/java-plants-enum-in-haskell/blob/main/src/Recommendations/Asthetaperlight/Planets.hs)

```{.haskell .scrollx}
module Recommendations.Asthetaperlight.Planets(runPlanets) where

-- Haskell implementation of the Java Enum: Planets example
-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

import Data.Foldable (traverse_)


data Planet = MERCURY
            | VENUS
            | EARTH
            | MARS
            | JUPITER
            | SATURN
            | URANUS
            | NEPTUNE deriving (Enum, Bounded, Show)


newtype Mass = Mass Double


newtype Radius = Radius Double


data PlanetStat =
    PlanetStat {
        mass   :: Mass
    ,   radius :: Radius
    }


newtype SurfaceGravity = SurfaceGravity Double


newtype Weight = Weight Double


gConstant :: Double
gConstant = 6.67300E-11

planetStat :: Planet -> PlanetStat
planetStat MERCURY = PlanetStat (Mass 3.303e+23) (Radius 2.4397e6 )
planetStat VENUS   = PlanetStat (Mass 4.869e+24) (Radius 6.0518e6 )
planetStat EARTH   = PlanetStat (Mass 5.976e+24) (Radius 6.37814e6)
planetStat MARS    = PlanetStat (Mass 6.421e+23) (Radius 3.3972e6 )
planetStat JUPITER = PlanetStat (Mass 1.9e+27  ) (Radius 7.1492e7 )
planetStat SATURN  = PlanetStat (Mass 5.688e+26) (Radius 6.0268e7 )
planetStat URANUS  = PlanetStat (Mass 8.686e+25) (Radius 2.5559e7 )
planetStat NEPTUNE = PlanetStat (Mass 1.024e+26) (Radius 2.4746e7 )


surfaceGravity :: Planet -> SurfaceGravity
surfaceGravity planet =
    let (PlanetStat (Mass mass) (Radius radius)) = planetStat planet
    in SurfaceGravity $ gConstant * mass / (radius * radius)


surfaceWeight :: Mass -> Planet -> Weight
surfaceWeight (Mass otherMass) planet =
    let (SurfaceGravity sg)= surfaceGravity planet
    in Weight $ otherMass * sg


runPlanets :: Double -> IO ()
runPlanets earthWeight =
    let (SurfaceGravity earthSurfaceGravity) = surfaceGravity EARTH

        massOnEarth :: Mass
        massOnEarth = Mass $ earthWeight / earthSurfaceGravity

        planetValues :: [Planet]
        planetValues = [(minBound :: Planet) .. (maxBound :: Planet)]

        printSurfaceWeight :: Planet -> Weight -> String
        printSurfaceWeight planet (Weight sw) = "Your weight on " <> (show planet) <> " is " <> (show sw)

        planetStatsStrings :: [String]
        planetStatsStrings = (\p -> printSurfaceWeight p (surfaceWeight massOnEarth p)) <$> planetValues
    in
       traverse_ putStrLn planetStatsStrings
```

</details>

<details open>
  <summary>Final Update</summary>

And here's the final implementation I decided to use given the above recommendations:

[code](https://github.com/ssanj/java-plants-enum-in-haskell/blob/main/src/Final/Planets.hs)

```{.haskell .scrollx}
module Final.Planets(runPlanets) where

import Data.Foldable (traverse_)

-- Haskell implementation of the Java Enum: Planets example
-- https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

import Data.Foldable (traverse_)


data Planet = MERCURY
            | VENUS
            | EARTH
            | MARS
            | JUPITER
            | SATURN
            | URANUS
            | NEPTUNE deriving (Enum, Bounded, Show)


newtype Mass = Mass Double


newtype Radius = Radius Double


gConstant :: Double
gConstant = 6.67300E-11


radius :: Planet -> Radius
radius MERCURY = Radius 2.4397e6
radius VENUS   = Radius 6.0518e6
radius EARTH   = Radius 6.37814e6
radius MARS    = Radius 3.3972e6
radius JUPITER = Radius 7.1492e7
radius SATURN  = Radius 6.0268e7
radius URANUS  = Radius 2.5559e7
radius NEPTUNE = Radius 2.4746e7


mass :: Planet -> Mass
mass MERCURY = Mass 3.303e+23
mass VENUS   = Mass 4.869e+24
mass EARTH   = Mass 5.976e+24
mass MARS    = Mass 6.421e+23
mass JUPITER = Mass 1.9e+27
mass SATURN  = Mass 5.688e+26
mass URANUS  = Mass 8.686e+25
mass NEPTUNE = Mass 1.024e+26


newtype Gravity = Gravity Double deriving Show

newtype Weight = Weight Double deriving Show


surfaceGravity :: Planet -> Gravity
surfaceGravity planet =
    let (Mass mass')     = mass planet
        (Radius radius') = radius planet
    in Gravity $ gConstant * mass' / (radius' * radius')


surfaceWeight :: Mass -> Planet -> Weight
surfaceWeight (Mass otherMass) planet =
    let (Gravity sg)= surfaceGravity planet
    in Weight $ otherMass * sg


runPlanets :: Double -> IO ()
runPlanets sampleWeight =
    let (Gravity earthSurfaceGravity) = surfaceGravity EARTH

        massOnEarth :: Mass
        massOnEarth = Mass $ sampleWeight / earthSurfaceGravity

        planetToWeight :: [(Planet, Weight)]
        planetToWeight = map (\p -> (p, surfaceWeight massOnEarth p)) [minBound .. maxBound]

        render :: (Planet, Weight) -> String
        render (p, weight) = "Your weight on " <> show p <> " is " <> (show weight)

    in mapM_ (putStrLn . render) planetToWeight
```

</details>
