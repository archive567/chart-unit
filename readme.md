```include
other/header.md
```

[chart-unit](https://github.com/tonyday567/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

`chart-unit` is a haskell chart library focusing on a small set of high-quality charts using native haskell. Here's a recent example, chock-a-block fill of titles and legends, since they've just been added:

![](https://tonyday567.github.io/other/mainExample.svg)

Chart Types
===

So far, there are 8 major chart types:

![](https://tonyday567.github.io/other/textHudExample.svg)

![](https://tonyday567.github.io/other/glyphHudExample.svg)

![](https://tonyday567.github.io/other/lglyphHudExample.svg)

![](https://tonyday567.github.io/other/lineHudExample.svg)

![](https://tonyday567.github.io/other/glineHudExample.svg)

![](https://tonyday567.github.io/other/rectHudExample.svg)

![](https://tonyday567.github.io/other/pixelHudExample.svg)

![](https://tonyday567.github.io/other/arrowHudExample.svg)

Gallery
===

The main chart types and the power of diagrams make it easy to invent new charts.  Hrere's some inspiration (all code for these charts is in [examples/sourceExamples.hs](examples/sourceExamples.hs)).

schoolbook
---

![](https://tonyday567.github.io/other/schoolbookExample.svg)

labelled bar
---

![](https://tonyday567.github.io/other/labelledBarExample.svg)

histogram diff
---

![](https://tonyday567.github.io/other/histDiffExample.svg)


combined scatter histogram
---

![](https://tonyday567.github.io/other/scatterHistExample.svg)


clipping
---

![](https://tonyday567.github.io/other/clippingExample.svg)

skinny
---

![](https://tonyday567.github.io/other/skinnyExample.svg)


animation
---

![](https://tonyday567.github.io/other/animationExample.gif)


workflow
===

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/chart-source-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i readme.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

```include
other/footer.md
```
