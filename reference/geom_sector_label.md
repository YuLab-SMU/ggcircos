# Sector labels

One label per sector, placed at the sector's mid-angle just outside the
node ring.

## Usage

``` r
geom_sector_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  offset = 0.16,
  angle = "horizontal",
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

- offset:

  Distance outside the node ring.

- angle:

  `"horizontal"` keeps the label upright and flips `hjust` on the left
  half; `"radial"` rotates it with the circle; a number is used as
  given.

## Value

A ggplot2 layer.

## Examples

``` r
nodes <- data.frame(
    id = c("a", "b", "c", "d"),
    genome = c("ref", "ref", "qry", "qry"),
    stringsAsFactors = FALSE
)
links <- data.frame(
    from = c("a", "c"), to = c("d", "b"),
    stringsAsFactors = FALSE
)
ggcircos(links, nodes = nodes, sectors = "genome") +
    geom_chord() +
    geom_sector_label()
```
