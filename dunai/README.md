<div align="center">

# Dunai

[![Build Status](https://api.travis-ci.com/ivanperez-keera/dunai.svg?branch=develop)](https://app.travis-ci.com/github/ivanperez-keera/dunai)
[![Version on Hackage](https://img.shields.io/hackage/v/dunai.svg)](https://hackage.haskell.org/package/dunai)

Dunai is a **generalized reactive programming library** on top of which other
variants like Classic FRP, Arrowized FRP and Reactive Values can be
implemented.

[Installation](#installation) •
[Examples](#examples) •
[Documentation](#documentation) •
[Related projects](#related-projects) •
[Technical information](#technical-information) •
[Contributions](#contributions) •
[History](#history)

</div>

## Features

- Intuitive syntax and semantics.

- Composition of effects via use of different monads and transformers.

- Isolation of effectful and effect-free reactive functions at type level.

- Time-free (time is not explicitly included) and time-able (time can be
  added).

- Fully extensible.

- Can be used to implement other FRP libraries/flavors on top.

- Supports applicative, functional and arrowized style.

- Programs can be tested with QuickCheck and debugged using Haskell Titan.

## Table of Contents

- [Installation](#installation)
  - [Pre-requisites](#pre-requisites)
  - [Compilation](#compilation)
- [Examples](#examples)
- [Documentation](#documentation)
  - [Publications](#publications)
  - [Videos](#videos)
- [Related projects](#related-projects)
  - [Games](#games)
  - [Libraries](#libraries)
- [Technical information](#technical-information)
  - [Performance](#performance)
- [Contributions](#contributions)
  - [Discussions, issues and pull requests](#discussions-issues-and-pull-requests)
  - [Structure and internals](#structure-and-internals)
  - [Style](#style)
  - [Version control](#version-control)
  - [Versioning model](#versioning-model)
- [History](#history)

# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Pre-requisites
<sup>[(Back to top)](#table-of-contents)</sup>

To use Dunai, you must have a Haskell compiler installed (GHC). We currently
support `GHC` versions 7.6.3 to 9.8.1. It likely works with other versions as
well.

On Debian/Ubuntu, both can be installed with:

```sh
$ apt-get install ghc cabal-install
```

On Mac, they can be installed with:

```sh
$ brew install ghc cabal-install
```

## Compilation
<sup>[(Back to top)](#table-of-contents)</sup>

Once you have a working set of Haskell tools installed, install Dunai by
executing:

```sh
$ cabal update
$ cabal install --lib dunai
```

Running the following will print the word `Success` if installation has gone
well, or show an error message otherwise:

```
$ runhaskell <<< 'import Data.MonadicStreamFunction; main = putStrLn "Success"'
```

# Examples
<sup>[(Back to top)](#table-of-contents)</sup>

Open a GHCi session and import the main Dunai module:

```haskell
$ ghci
ghci> import Data.MonadicStreamFunction
```

An MSF is a time-varying transformation applied to a series of inputs as they
come along, one by one.

Use the primitive `arr :: (a -> b) -> MSF m a b` to turn any pure function into
an MSF that applies the given function to every input. The function `embed ::
MSF m a b -> [a] -> m [b]` runs an MSF with a series of inputs, collecting the
outputs:

```haskell
ghci> embed (arr (+1)) [1,2,3,4,5]
[2,3,4,5,6]
```

MSFs can have side effects; hence the `m` that accompanies the type `MSF` in
the signatures of `arr` and `embed`. The function `arrM` turns a _monadic
function_ of type `a -> m b` into an MSF that will constantly apply the
function to each input.

For example, the function `print` takes a value and prints it to the terminal
(a side effect in the `IO` monad), producing an empty `()` output. Elevating or
lifting `print` into an `MSF` will turn it into a processor that prints each
input passed to it:

```haskell
ghci> :type print
print :: Show a => a -> IO ()
ghci> :type arrM print
arrM print :: Show a => MSF IO a ()
```

If we now run that MSF with five inputs, all are printed to the terminal:

```haskell
ghci> embed (arrM print) [1,2,3,4,5]
1
2
3
4
5
[(), (), (), (), ()]
```

As we can see, after all side effects, `embed` collects all the outputs, which
GHCi shows at the end.

When we only care about the side effects and not the output list, we can
discard it with `Control.Monad.void`. (Dunai provides an auxiliary function
`embed_` for the same purpose.)

```haskell
ghci> import Control.Monad (void)
ghci> void $ embed (arrM print) [1,2,3,4,5]
1
2
3
4
5
```

MSFs can be piped into one another with the functions `(>>>)` or `(.)`, so that
the output of one MSF is fed as input to another MSF _at each point_:

```haskell
ghci> void $ embed (arr (+1) >>> arrM print) [1,2,3,4,5]
2
3
4
5
6
```

A monadic computation without arguments can be lifted into an MSF with the
function `constM`:

```haskell
ghci> :type getLine
getLine :: IO String
ghci> :type constM getLine
constM getLine :: MSF IO a String
```

This MSF will get a line of text from the terminal every time it is called,
which we can pipe into an MSF that will print it back.

```haskell
ghci> void $ embed (constM getLine >>> arrM putStrLn) [(), ()]
What the user types, the computer repeats.
What the user types, the computer repeats.
Once again, the computer repeats.
Once again, the computer repeats.
```

Notice how we did not care about the values in the input list to `embed`: the
only thing that matters is how many elements it has, which determines how many
times `embed` will run the MSF.

Simulations can run indefinitely with the function `reactimate :: MSF m () ()
-> m ()`, which is useful when the input to the MSFs being executed is being
produced by another MSFs, like in the case above with `constM getLine`
producing inputs consumed by `arrM putStrLn`:

```haskell
ghci> reactimate (constM getLine >>> arr reverse >>> arrM putStrLn)
Hello
olleH
Haskell is awesome
emosewa si lleksaH
^C
```

Dunai has a very extensive API and supports many programming styles. MSFs are
applicatives, so we can transform them using applicative style, and they are
categories, so they can be piped into one another with `Control.Category.(.)`.
For example, the line above can also be written as:

```haskell
ghci> reactimate (arrM putStrLn . (reverse <$> constM getLine))
```

which is equivalent to:

```haskell
ghci> reactimate (arrM putStrLn . fmap reverse . constM getLine)
```

Other writing styles (e.g., arrow notation) are also supported. This
versatility makes it possible for you to use the notation you feel most
comfortable with.

MSFs are immensely expressive. With MSFs, you can implement stream programming,
functional reactive programming (both classic and arrowized), reactive
programming, and reactive values, among many others. The real power of MSFs
comes from the ability to carry out temporal transformations (e.g., delays), to
apply different transformations at different points in time, and to work with
different monads. See the documentation below to understand how capable they
are.

# Documentation
<sup>[(Back to top)](#table-of-contents)</sup>

## Publications
<sup>[(Back to top)](#table-of-contents)</sup>

The best introduction to the fundamentals of Monadic Stream Functions is:

- [Functional Reactive Programming, Refactored](https://dl.acm.org/doi/10.1145/2976002.2976010?cid=99658741366) ([official ACM page](http://dl.acm.org/citation.cfm?id=2976010)) ([mirror](https://ivanperez.io/#FRPRefactored)) ([updated paper examples](https://github.com/ivanperez-keera/dunai/tree/develop/dunai-examples/paper)).

The following papers are also related to MSFs:

- [Fault Tolerant Functional Reactive Programming](https://dl.acm.org/doi/10.1145/3236791?cid=99658741366) ([mirror](https://ivanperez.io/#ftfrp))

- [Fault Tolerant Functional Reactive Programming (extended version)](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/abs/faulttolerant-functional-reactive-programming-extended-version/F0C270C83E218FA5627D96A7FD6C56E9)

- [Rhine: FRP with type-level clocks](https://dl.acm.org/doi/10.1145/3242744.3242757?cid=99658741366) ([mirror](https://ivanperez.io/#rhine))

- [Back to the Future: time travel in FRP](https://dl.acm.org/doi/10.1145/3122955.3122957?cid=99658741366) ([mirror](https://ivanperez.io/#backtothefuture))

- [Testing and Debugging Functional Reactive Programming](https://dl.acm.org/doi/10.1145/3110246?cid=99658741366) ([mirror](https://ivanperez.io/#testingfrp))

## Videos
<sup>[(Back to top)](#table-of-contents)</sup>

- [Actors Design Patterns and Arrowised FRP](https://youtu.be/wO_jX8wGhU0?t=781). Talk by Diego Alonso Blas, describing Monadic Stream Functions and an encoding in scala.

- [Functional Reactive Programming, Refactored](https://www.youtube.com/watch?v=FmwOd4z9LdM). Original talk describing MSFs. Haskell Symposium 2016.

- [Back to the Future: Time Travel in FRP](https://www.youtube.com/watch?v=p2jJGjbjbig). Talk describing how to do time transformations in FRP and MSFs. Haskell Symposium 2017.

- [Fault Tolerant Functional Reactive Programming](https://www.youtube.com/watch?v=owojLkI5YyY). Talk describing how MSFs can be used to add fault tolerance information. ICFP 2018.

- [Rhine: FRP with Type-level Clocks](https://www.youtube.com/watch?v=Xvgz11D7xqs). Talk describing how MSFs can be extended with clocks. Haskell Symposium 2018.

# Related projects
<sup>[(Back to top)](#table-of-contents)</sup>

## Games
<sup>[(Back to top)](#table-of-contents)</sup>

- [The Bearriver Arcade](https://github.com/walseb/The_Bearriver_Arcade). Fun arcade games made using Bearriver.

- [Haskanoid](https://github.com/ivanperez-keera/haskanoid). Haskell breakout game implemented using the Functional Reactive Programming library Yampa (compatible with Dunai/Bearriver).

## Libraries

- [ivanperez-keera/Yampa](https://github.com/ivanperez-keera/Yampa): a full
  FRP implementation that has been used extensively in academia, open source
  and industry.

- [turion/rhine](https://github.com/turion/rhine): extension of Dunai with
  type-level clocks and explicit coordination.

- [keera-studios/haskell-titan](https://github.com/keera-studios/haskell-titan):
  an advanced, interactive testing framework with support for step-by-step
  execution and record-and-replay. Haskell-titan supports connecting to dunai
  systems via its Yampa-compatible interface library bearriver, via a flag in
  the libraries
  [`titan-debug-yampa`](https://hackage.haskell.org/package/titan-debug-yampa)
  and
  [`titan-record-yampa`](https://hackage.haskell.org/package/titan-record-yampa).

# Technical information
<sup>[(Back to top)](#table-of-contents)</sup>

## Performance
<sup>[(Back to top)](#table-of-contents)</sup>

Simpler games will be playable without further optimisations. For example, the
game [haskanoid](https://github.com/ivanperez-keera/haskanoid) works well with
Dunai/Bearriver. You can try it with:

```sh
$ git clone https://github.com/ivanperez-keera/haskanoid.git
$ cd haskanoid/
$ cabal install -f-wiimote -f-kinect -fbearriver
```

It uses unaccelerated SDL 1.2, the speed is comparable to Yampa's:

```
$ haskanoid
Performance report :: Time per frame: 13.88ms, FPS: 72.04610951008645, Total running time: 1447
Performance report :: Time per frame: 16.46ms, FPS: 60.75334143377886, Total running time: 3093
Performance report :: Time per frame: 17.48ms, FPS: 57.20823798627002, Total running time: 4841
Performance report :: Time per frame: 19.56ms, FPS: 51.12474437627812, Total running time: 6797
Performance report :: Time per frame: 19.96ms, FPS: 50.100200400801604, Total running time: 8793
Performance report :: Time per frame: 19.44ms, FPS: 51.440329218106996, Total running time: 10737
```

It runs almost in constant memory, with about 50% more memory consumption than
with `Yampa`: 200k for Yampa and 300K for Dunai/Bearriver. (There is very minor
leaking, probably we can fix that with seq.)

We have obtained different figures tracking different modules. In [the
paper](https://dl.acm.org/authorize?N34896), we provided figures for the whole
game, but we need to run newer reliable benchmarks including every module and
only definitions from `FRP.Yampa`, `FRP.BearRiver` and
`Data.MonadicStreamFunction`.

Dunai includes some benchmarks as part of the main library. You are encouraged
to use them to evaluate your pull requests, and to improve the benchmarks
themselves.

# Contributions
<sup>[(Back to top)](#table-of-contents)</sup>

If this library helps you, you may want to consider [buying the maintainer a
cup of coffee](https://github.com/sponsors/ivanperez-keera/).

## Discussions, issues and pull requests
<sup>[(Back to top)](#table-of-contents)</sup>

**Discussions**

If you have any comments, questions, ideas, or other topics that you think will
be of interest to the Dunai community, start a new discussion
[here](https://github.com/ivanperez-keera/dunai/discussions). Examples include:

- You've created a new game or application that uses Dunai or BearRiver.
- You've written or found a library that helps use Dunai/BearRiver in a
  particular domain, or apply it to a specific platform.
- You've written or found a paper that mentions Dunai/BearRiver.
- You have an idea for an extension that will enable writing programs that are
  not currently possible or convenient to capture.
- You think you've found a bug.
- You want to propose an improvement (e.g., make the code faster or smaller).
- You have a question.
- Something in the documentation, a tutorial or a Dunai / BearRiver / FRP paper
  is unclear.
- You like the project and want to introduce yourself.

**Issues**

If a specific change is being proposed (either a new feature or a bug fix), you
can *open an issue* documenting the proposed change
[here](https://github.com/ivanperez-keera/dunai/issues).

If you are unsure about whether your submission should be filed as an issue or
as a discussion, file it as a discussion. We can always move it later.

**Pull requests**

Once we determine that an issue will be addressed, we'll decide who does it and
when the change will be added to Dunai. Even if you implement the solution,
someone will walk you through the steps to ensure that your submission conforms
with our version control process, style guide, etc. More information on our
process is included below.

Please, do not just send a PR unless there is an issue for it and someone from
the Dunai team has confirmed that you should address it. The PR is *very*
likely to be rejected, and we really want to accept your contributions, so it
will make us very sad. Open a discussion / issue first and let us guide you
through the process.

## Structure and internals
<sup>[(Back to top)](#table-of-contents)</sup>

This project is split in three parts:

- _Dunai_: a reactive library that combines monads and arrows.
- _BearRiver_: Yampa implemented on top of Dunai.
- _Examples_: ballbounce
  - sample applications that work both on traditional Yampa and BearRiver.

Dunai also includes some benchmarks as part of the main library. You are
encouraged to use them to evaluate your pull requests, and to improve the
benchmarks themselves.

## Style
<sup>[(Back to top)](#table-of-contents)</sup>

We follow [this style guide](https://keera.co.uk/wp-content/uploads/2021/11/haskellguide-v1.3.0.pdf).

## Version control
<sup>[(Back to top)](#table-of-contents)</sup>

We follow [git flow](http://nvie.com/posts/a-successful-git-branching-model/).
In addition:

- Please document your commits clearly and separately.
- Always refer to the issue you are fixing in the commit summary line with the
  text `Refs #<issue_number>` at the end.
- If there is no issue for your change, then open an issue first and document
  what you are trying to achieve/improve/fix.
- Do not address more than one issue per commit or per PR. If two changes are
  not directly related to one another, they belong in different PRs, issues and
  commits.
- Document what you did in the respective CHANGELOGs in a separate commit
  before you send a PR. This commit should be the last one in the PR.
- If your commit pertains to one package only, name the package at the
  beginning of the summary line with the syntax `<package_name>:
  <...rest_of_summary...>`.
- Make sure your changes conform to the [coding
  style](https://keera.co.uk/wp-content/uploads/2021/11/haskellguide-v1.3.0.pdf).

See the recent repo history for examples of this process. Using a visual repo
inspection tool like `gitk` may help.

## Versioning model
<sup>[(Back to top)](#table-of-contents)</sup>

The versioning model we use is the standard in Haskell packages. Versions have
the format `<PUB>.<MAJOR>.<MINOR>(.<PATCH>)?` where:

- `<PUB>` is just a way to signal important milestones or used for promotional
  reasons (to indicate a major advancement). A zero on this position has no
  special meaning.

- `<MAJOR>` increases on incompatible API changes.

- `<MINOR>` increases on backwards-compatible changes.

- `<PATCH>` (optional) increases on small changes that do not affect behavior
  (e.g., documentation).

# History
<sup>[(Back to top)](#table-of-contents)</sup>

This library Dunai was created by Ivan Perez and Manuel Baerenz. It is named
after the Dunai (aka. Danube, or Дунай) river, one of the main rivers in
Europe, originating in Germany and touching Austria, Slovakia, Hungary,
Croatia, Serbia, Romania, Bulgaria, Moldova and Ukraine.

Other FRP libraries, like Yampa and Rhine, are named after rivers. Dunai has
been chosen due to the authors' relation with some of the countries it passes
through, and knowing that this library has helped unite otherwise very
different people from different backgrounds.
