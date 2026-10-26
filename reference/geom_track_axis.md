# Radial axes for circular tracks

Draws the two edges of a track band as rings, a radial tick at every
node boundary for each break, and the break labels outside the band. The
scale is always explicit: `limits` is required, so the axis can never
disagree with how the track layer scaled its data.

## Usage

``` r
geom_track_axis(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  band,
  limits,
  breaks = NULL,
  labels = ggplot2::waiver(),
  text.size = 3,
  line.size = 0.2,
  tick.length = 0.02,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, data, stat, position, show.legend, inherit.aes, na.rm, ...:

  Standard
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  arguments.

- band:

  Track name or `c(r0, r1)` radius interval.

- limits:

  Value range of the track; required.

- breaks:

  Break positions. `NULL` uses `pretty(limits, 4)`.

- labels:

  Break labels; `waiver()` formats them with the default formatter.

- text.size:

  Size of the break labels.

- line.size:

  Width of the rings and ticks.

- tick.length:

  Extra radius reserved for the labels.

## Value

A ggplot2 layer.

## Examples

``` r
set.seed(1)
vals <- data.frame(
    id = rep(colnames(mtcars), each = 4),
    value = runif(ncol(mtcars) * 4),
    stringsAsFactors = FALSE
)
ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
    geom_track(data = vals, aes(x = value), node = id, band = "values") +
    geom_track_axis(band = "values", limits = c(0, 1))
```
