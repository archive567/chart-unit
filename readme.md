[chart-unit](https://github.com/tonyday567/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

`chart-unit` is a haskell chart library focusing on a small set of high-quality charts using native haskell. Here's a recent example, chock-a-block fill of titles and legends, since they've just been added:

<img style="border:2px solid grey" src="https://tonyday567.github.io/chart-unit/other/exampleChart.svg">

gallery
---

<img style="height:50%;" src="https://tonyday567.github.io/chart-unit/other/exampleLine.svg"> 
<img style="height:50%;" src="https://tonyday567.github.io/chart-unit/other/exampleGlyph.svg">
<img style="height:50%;" src="https://tonyday567.github.io/chart-unit/other/exampleGline.svg">
<img style="height:50%;" src="https://tonyday567.github.io/chart-unit/other/exampleLGlyph.svg">

___


chart bling
===

line chart
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleLine.svg">

scatter chart
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleScatter.svg">

histogram
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleHist.svg">

pixel chart
---
<img src="https://tonyday567.github.io/chart-unit/other/examplePixels.svg">

histogram diff
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleHistCompare.svg">

arrow chart
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleArrow.svg">

compound
---
<img src="https://tonyday567.github.io/chart-unit/other/exampleCompound.svg">

animation
---
<img src="https://tonyday567.github.io/chart-unit/other/anim.gif">

compile recipe
--------

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/chart-unit-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i examples/examples.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
