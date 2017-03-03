
[chart-unit](https://tonyday567.github.io/chart-unit) [![Build Status](https://travis-ci.org/tonyday567/chart-unit.png)](https://travis-ci.org/tonyday567/chart-unit)
===

[repo](https://tonyday567.github.com/chart-unit)

`chart-unit` is an experimental haskell chart library.  

At the moment, only SVG is supported, and github markdown doesn't include support for svg images.

See https://tonyday567.github.io/chart-unit for a readme.

compile recipe
--------

    tack build --test --copy-bins --exec  "chart-unit-examples" --exec "pandoc -f markdown -t html -i examples/examples.md -o index.html" --file-watch

to do
---

- legends, text (titles, etc)
- one-dimensional charts, based on an axis
- pixel chart (rectangles filling the XY plane)
- variable-width hitsogram (aka quantile-based histogram)
