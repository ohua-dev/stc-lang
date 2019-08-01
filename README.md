# <img src="https://raw.githubusercontent.com/ohua-dev/ohua-dev.github.io/master/logos/fileIcons/iconCompact/haskell/export_compact.png" width="40px"/> STCLang: A library for implicit monadic dataflow parallelism

[![Build Status](https://travis-ci.org/ohua-dev/stc-lang.svg?branch=master)](https://travis-ci.org/ohua-dev/stc-lang)
[![Hackage](https://img.shields.io/hackage/v/stc-lang)](https://hackage.haskell.org/package/stc-lang)
[![GitHub](https://img.shields.io/github/license/ohua-dev/stc-lang)](https://github.com/ohua-dev/stc-lang/blob/master/LICENSE)
[![Publication](https://img.shields.io/badge/published_paper-Haskell'19-informational)](#publication)

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

We documented the principles in this library in a paper at the Haskell Symposium
2019.

https://doi.org/10.1145/3331545.3342600
