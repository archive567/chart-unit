[chart-unit](https://github.com/tonyday567/chart-unit)
===

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.svg)](https://travis-ci.org/tonyday567/chart-unit) [![Hackage](https://img.shields.io/hackage/v/chart-unit.svg)](https://hackage.haskell.org/package/chart-unit) [![lts](https://www.stackage.org/package/chart-unit/badge/lts)](http://stackage.org/lts/package/chart-unit) [![nightly](https://www.stackage.org/package/chart-unit/badge/nightly)](http://stackage.org/nightly/package/chart-unit)

`chart-unit` is a haskell chart library focusing on a small set of high-quality charts using native haskell. Here's a recent example, chock-a-block full of titles and legends (that needed testing):

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

Why the name chart-unit?
===

Most of the behind-the-scenes grunt work is scaling data, projecting points from one range to another, and computing position.  A key to making these computations neat was `one = Range -0.5 0.5` and `one = Rect -0.5 0.5 -0.5 0.5` as the (multiplicative) units of a chart range.  See [numhask-range](https://github.com/tonyday567/numhask-range) for a monologue.

What's with the funny names?
===

Charting is an age-old craft, and stuffed to the brim with cliche and jargon.  I wanted to cut through the cruft of what is thought of as a chart (and charting has been dominated by excel for 30 years), and approach charting from a haskelly perspective.  The funny names were the cohorts that popped out.  A rough translation:


| ye-old Chart type                                    | chart-unit type       |
|------------------------------------------------------|-----------------------|
| line                                                 | LineChart             |
| line chart with markers                              | GLineChart            |
| scatter                                              | GlyphChart            |
| pie                                                  | pull requests welcome |
| heatmap                                              | PixelChart            |
| [Bar](https://en.wikipedia.org/wiki/Bar_chart)       | RectChart             |
| [Histogram](https://en.wikipedia.org/wiki/Histogram) | RectChart             |
| sparkline                                            | a skinny Aspect       |
|                                                      |                       |

Gallery
===

The main chart types and the power of diagrams make it easy to invent new charts.  Hrere's some inspiration (all code for these charts is in [examples/sourceExamples.hs](https://github.com/tonyday567/chart-unit/blob/master/examples/sourceExamples.hs)).

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
stack build --test --exec "$(stack path --local-install-root)/bin/chart-source-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md readme.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
