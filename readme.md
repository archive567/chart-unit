```include
other/header.md
```

[chart-unit](https://github.com/tonyday567/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

`chart-unit` is a haskell chart library focusing on a small set of high-quality charts using native haskell. Here's a recent example, chock-a-block fill of titles and legends, since they've just been added:

![](other/mainExample.svg)

Chart Types
===

So far, there are 8 major chart types:

![](other/textHudExample.svg)

![](other/glyphHudExample.svg)

![](other/lglyphHudExample.svg)

![](other/lineHudExample.svg)

![](other/glineHudExample.svg)

![](other/rectHudExample.svg)

![](other/pixelHudExample.svg)

![](other/arrowHudExample.svg)

Gallery
===

The main chart types and the power of diagrams make it easy to invent new charts.  Hrere's some inspiration (all code for these charts is in [examples/sourceExamples.hs](examples/sourceExamples.hs)).

schoolbook
---

![](other/schoolbookExample.svg)

labelled bar
---

![](other/labelledBarExample.svg)

histogram diff
---

![](other/histDiffExample.svg)


combined scatter histogram
---

![](other/scatterHistExample.svg)


clipping
---

![](other/clippingExample.svg)

skinny
---

![](other/skinnyExample.svg)


animation
---

![](other/animationExample.gif)


workflow
===

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/chart-source-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i readme.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

```include
other/footer.md
```
