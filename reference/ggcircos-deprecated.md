# Deprecated ggcircos API

These are the 0.0.0.9000 entry points. They still work, but they warn
and forward to their 0.1.0 replacements; see the individual functions
for how the old arguments map.

## Usage

``` r
geom_curve_link(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  arrow = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_circos(
  mapping,
  geom,
  data = NULL,
  position = "auto",
  pwidth = 0.3,
  offset = 0.02,
  inherit.aes = FALSE,
  grid.params = NULL,
  axis.params = NULL,
  ...
)

geom_link_lab(
  data = NULL,
  mapping = NULL,
  hjust = 0,
  offset = 0,
  geom = "text",
  as_ylab = FALSE,
  ...
)

as.tbl_lk(x, ...)

tidy_link(x, radius = 1, ...)
```

## Arguments

- geom, mapping, pwidth, offset, grid.params, axis.params:

  Legacy arguments, mapped onto
  [`geom_track()`](https://yulab-smu.github.io/ggcircos/reference/geom_track.md)
  and
  [`geom_track_axis()`](https://yulab-smu.github.io/ggcircos/reference/geom_track_axis.md).
