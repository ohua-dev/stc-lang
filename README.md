# STCLang: A library for implicit monadic dataflow parallelism

STCLang is a library that enables stateful, implicit, monadic parallel
computation in Haskell. The core ideas come from the
[ohua](https://ohua-dev.github.io) project.

STCLang lets you create parallel dataflows with stateful nodes without having to
explicitly wire complex graph structures. Instead the program is written with an
embedded, monadic DSL and automatically transformed into a graph and executed in
parallel.

On top of the base abstraction we have also built an FRP (functional reactive
programming) interface. This allows you to run reactive programs on sequential
streams of values and leverage pipeline parallelism to peed up computation.

We also [published](#publication) the theory and concepts behind this library.

## Publication

We documented the principles in this library in a paper at Haskell'2019.

A link to the publication will appear here once we have one, e.t.a. is 22th of
August (date of the conference). Should it be after this date now, but there's
still no link, I probably forgot. In that case open an issue, shoot
[me](https://github.com/JustusAdam) an email or tweet me at @justusadam_.
