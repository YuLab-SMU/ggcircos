# Node layers

Layers for the node ring itself: the arc, a label, a point. They read
the node table of the plot's layout and only ever add their own data –
the layout stays read-only.

## Usage

``` r
geom_node_arc(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  width = 0.04,
  offset = 0,
  linewidth = 0.2,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_node_text(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  rotate = FALSE,
  offset = 0.1,
  repel = FALSE,
  nudge_x = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)

geom_node_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  offset = 0.02,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, show.legend, inherit.aes, na.rm, ...:

  Standard
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  arguments.

- width:

  Width of the node arc (ribbon type) or of its gap (`geom_node_text`),
  in radius units.

- offset:

  Distance between the node ring and the layer, in radius units.

- linewidth:

  Border width of the node arc.

- rotate:

  Rotate the label to follow the circle. The rotation is derived from
  where the coordinate actually puts the label, so it stays correct for
  any `start` or `clockwise`.

- repel:

  Push overlapping labels outward along their own radius. Labels that
  would collide are staggered by a small radial step instead of drifting
  sideways, which is what a circular layout needs: the angle carries the
  node, so a label may only move along its own spoke. `ggrepel` does not
  help here – it resolves collisions in a rectangular space and lines
  the labels up flat.

- nudge_x:

  Radial nudge applied to the label.

## Value

A ggplot2 layer.

## Label placement

`rotate = FALSE` keeps labels horizontal and flips `hjust` on the left
half of the circle. `rotate = TRUE` rotates them, flipping both the
angle and `hjust` on the left half. `geom_node_text()` does not take an
`angle` aesthetic – use `rotate`, so the two can never disagree.

## Examples

``` r
ggcircos(cor(mtcars), order = "hclust") +
    geom_node_arc() +
    geom_node_text()
```
