This repository implements a generalized version of reactive programming, on
top of which other variants like Yampa, Classic FRP and Reactive Values can
be implemented.

# Structure, internals and current status.

This project is split in three parts:

- _Dunai_: a reactive library that combines monads and arrows.
- _BearRiver_: Yampa implemented on top of Dunai.
- _Examples_: ballbounce, haskanoid
  - sample applications that work both on traditional Yampa and BearRiver.

We need to add examples of apps written in classic FRP, reactive values, etc. A
new game, in honor of Paul Hudak, has been designed to work best with this
library. It will also be added to the repo.

Dunai uses monad transformers, but it's still very experimental and we have not
agreed on either the syntax or intended behaviour of trans-monadic reactive
networks. So, don't get too caught up in the current implementation, and don't
freak out if you can't understand why there are three implementations of the
same function.

# Examples: Haskanoid

Performance is ok, simpler games will be playable without further
optimisations. This uses unaccelerated SDL 1.2. The speed is comparable to
Yampa's.

```
2016-05-09 15:29:41 dash@dash-desktop:~/Projects/PhD/Yampa/yampa-clocks-dunai$ ./.cabal-sandbox/bin/haskanoid

Performance report :: Time per frame: 13.88ms, FPS: 72.04610951008645, Total running time: 1447
Performance report :: Time per frame: 16.46ms, FPS: 60.75334143377886, Total running time: 3093
Performance report :: Time per frame: 17.48ms, FPS: 57.20823798627002, Total running time: 4841
Performance report :: Time per frame: 19.56ms, FPS: 51.12474437627812, Total running time: 6797
Performance report :: Time per frame: 19.96ms, FPS: 50.100200400801604, Total running time: 8793
Performance report :: Time per frame: 19.44ms, FPS: 51.440329218106996, Total running time: 10737
```

It runs almost in constant memory, with about 50% more memory consumption than
with Yampa (200k for Yampa and 300K for dunai/bearriver). There is very minor
leaking, probably we can fix that with seq.

We have obtained different figures tracking different modules. In the paper, we
provided figures for the whole game, but we need to run newer reliable
benchmarks including every module and only things that live in FRP.Yampa,
FRP.BearRiver and Data.MonadicStreamFunction.

You can try it with:

```
git clone https://github.com/ivanperez-keera/dunai.git
cd dunai
cabal sandbox init
cabal install
cabal install examples/bearriver
cabal install -f-wiimote -f-kinect -fbearriver examples/haskanoid
```

You may have to use `cabal sandbox add-source` and install all libs with one
`cabal install` invocation to link against the same versions of all common
dependencies.

# Contributions

We are following this guide:

http://nvie.com/posts/a-successful-git-branching-model/

Please, feel free to open new issues. In particular, we are looking for:

- Unexplored ways of using MSFs.
- Other games or applications that use MSFs (including but not limited to Yampa games).

# About the name

Dunai (aka. Danube, or Дунай) is one of the main rivers in Europe, originating
in Germany and touching Austria, Slovakia, Hungary, Croatia, Serbia, Romania,
Bulgaria, Moldova and Ukraine.

The name is chosen following the idea of naming FRP libraries after
rivers, and knowing that this library can be used to implement many others.
I've chosen Dunai specifically taking into account the authors' relation with
some of the countries it passes through, and knowing that this library has
helped unite otherwise very different people from different backgrounds.
