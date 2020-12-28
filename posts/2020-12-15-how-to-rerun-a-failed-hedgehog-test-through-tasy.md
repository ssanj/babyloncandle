---
title: How to Rerun a Failed Hedgehog Test Through Tasty with Stack
author: sanjiv sahayam
description: How run a failed Hedgehog property test through Tasty with Stack
tags: haskell, stack, tasty, hedgehog, pbt
comments: true
---

I recently had a failing [Hedgehog](https://hackage.haskell.org/package/hedgehog) property while running some tests on a personal project. The output was something like:

```{.terminal .scrollx}
    versionRange failure: FAIL
        ✗ versionRange failure failed at test/DBPropSpec.hs:41:54
          after 1 test and 2 shrinks.

             ┏━━ test/DBPropSpec.hs ━━━
          25 ┃ hprop_versionRange_failure :: H.Property
          26 ┃ hprop_versionRange_failure =
          27 ┃   H.property $ do
          28 ┃     minR  <- H.forAll $ Gen.int (Range.linear 0 99)
             ┃     │ 0
          29 ┃     maxR  <- H.forAll $ Gen.int (Range.linear (minR + 1) 200)
             ┃     │ 1
          30 ┃     let upperG :: H.Gen Int =  Gen.int (Range.linear maxR (maxR + 100))
          31 ┃         lowerG :: H.Gen Int =  Gen.int (Range.linear (minR - 100) minR)
          32 ┃         minMax = (D.VersionRange minR maxR)
          33 ┃     versionE <- H.forAll $ Gen.either lowerG upperG
             ┃     │ Left 0
          34 ┃     either (assertVersionRangeFailure minMax) (assertVersionRangeFailure minMax) versionE
          35 ┃       where
          36 ┃             assertVersionRangeFailure :: D.VersionRange -> Int -> H.PropertyT IO ()
          37 ┃             assertVersionRangeFailure maxMin version =
          38 ┃               let range = D.versionRange maxMin (D.mkNoteVersion version)
          39 ┃               in case range of
          40 ┃                   (D.InvalidNoteVersionRange v r) -> (r H.=== maxMin) >> (v H.=== version)
          41 ┃                   (D.ValidNoteVersionRange _)     -> H.failure
             ┃                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          This failure can be reproduced by running:
          > recheck (Size 99) (Seed 15737640735508047734 16943050916655939693) versionRange failure

      Use '--hedgehog-replay "Size 99 Seed 15737640735508047734 16943050916655939693"' to reproduce.
```

While the output mentions that we can rerun the failed property with:

```{.terminal .scrollx}
Use '--hedgehog-replay "Size 99 Seed 15737640735508047734 16943050916655939693"' to reproduce.
```

We can't use this information directly if we are running the tests through [Tasty](https://hackage.haskell.org/package/tasty) and [Stack](https://github.com/commercialhaskell/stack). I had previously blogged about [How to run specific tests through Stack with Tasty in Haskell](https://sanj.ink/posts/2018-02-01-how-to-run-specific-tests-through-stack-with-tasy-in-haskell.html). From that article we know about the `--ta` parameter which enables us to pass parameters to the underlying testing framework:

```{.terminal .scrollx}
--ta,--test-arguments TEST_ARGS Arguments passed in to the test suite program
```

Using that information we can now rerun the Hedgehog property like so:

```{.terminal .scrollx}
stack test --ta '--hedgehog-replay "Size 100 Seed 15737640735508047734 16943050916655939693"'
```

Unfortunately this will run all the tests including the ones that are not affected by the failing seed. From the post mentioned previously we know how to use the `-p` parameter to run specific tests:

```{.terminal .scrollx}
-p,--pattern ARG         Select only tests that match pattern
```

Using that information we can now rerun just the affect Hedgehog test with the provided seed:

```{.terminal .scrollx}
stack test --ta '--hedgehog-replay "Size 100 Seed 15737640735508047734 16943050916655939693" -p "your_spec_name"'
```

While this is quite cumbersome to use I hope this will help anyone else who was stumped by how to simply rerun a failed Hedgehog property through tasty and Stack.
