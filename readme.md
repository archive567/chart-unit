
[chart-unit](https://tonyday567.github.io/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

[repo](https://github.com/tonyday567/chart-unit)

`chart-unit` is an experimental haskell chart library.  

See https://tonyday567.github.io/chart-unit for an extended description.

chart bling
===

line chart
---
<img src="https://tonyday567.github.io/other/exampleLine.svg">

scatter chart
---
<img src="https://tonyday567.github.io/other/exampleScatter.svg">

histogram
---
<img src="https://tonyday567.github.io/other/exampleHist.svg">

pixel chart
---
<img src="https://tonyday567.github.io/other/examplePixels.svg">

histogram diff
---
<img src="https://tonyday567.github.io/other/exampleHistCompare.svg">

arrow chart
---
<img src="https://tonyday567.github.io/other/exampleArrow.svg">

compound
---
<img src="https://tonyday567.github.io/other/exampleCompound.svg">

animation
---
<img src="https://tonyday567.github.io/other/anim.gif">

compile recipe
--------

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/chart-unit-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i examples/examples.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

to do
---

- legends, text (titles, etc)
