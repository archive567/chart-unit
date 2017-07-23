
[chart-unit](https://tonyday567.github.io/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

[repo](https://tonyday567.github.com/chart-unit)

`chart-unit` is an experimental haskell chart library.  

At the moment, only SVG is supported, and github markdown doesn't include support for svg images.

See https://tonyday567.github.io/chart-unit for an extended description.

bling
===

line chart

![](other/exampleLine.svg)

scatter chart

![](other/exampleScatter.svg)

histogram

![](other/exampleHist.svg)

pixel chart

![](other/examplePixels.svg)

skinny

![](other/exampleOneDim.svg)

histogram diff

![](other/exampleHistCompare.svg)

arrow chart

![](other/exampleArrow.svg)

compound

![](other/exampleCompound.svg)

animation

<img style="border:5px solid grey" src="other/anim.gif">









compile recipe
--------

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/chart-unit-examples" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/examples.hs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

to do
---

- legends, text (titles, etc)
