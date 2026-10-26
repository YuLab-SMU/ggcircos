# Add the angle columns a track layer needs

Translates the node id column into `y` (the node's angle) and
`ymin`/`ymax` (its arc, shrunk by `width`; for `"tile"`, the arc split
into one bin per row of that node). This runs once, when the layer is
added, because ggplot2 runs positions *after* the geom setup: anything a
geom lists in `required_aes` must exist by then. The radial extent of a
tile band is the position's job, not this function's.

## Usage

``` r
track_angles(
  data,
  node,
  layout,
  width = 0.9,
  geom = "col",
  has_value = FALSE,
  ...,
  aggregate = NULL,
  layouts = NULL,
  group = NULL
)
```

## Arguments

- data:

  Layer data.

- node:

  Name of the node id column.

- layout:

  A `circos_layout` (or its node table).

- width:

  Fraction of the node arc used by the layer.

- geom:

  Track geometry name.

- has_value:

  Whether the caller already supplies the values, either as a data
  column named `x` or through a mapping. When `TRUE` no placeholder
  column is written, because a column called `x` would shadow the
  layer's own mapping and every value would scale to the same radius.

- ...:

  Ignored; kept so older calls that passed `band_width` still work.
  `aggregate` sits after `...` on purpose, so it can only be given by
  name: a positional argument there would silently land on it.

- aggregate:

  Whether this geom's stat folds a node's rows into one mark
  (`"boxplot"`, `"violin"`). Such a mark keeps the node's whole arc
  rather than one bin of it. `NULL` asks `track_geom_spec()`; pass it
  explicitly when calling this function with a geometry of your own.

- layouts:

  A `circos_layouts` list (the `by = ` result) instead of a single
  layout: every row is then placed by the node table of its own group,
  so two rings may put the same id at different angles.

- group:

  Name of the column in `data` that says which group a row belongs to.
  Required with `layouts`.

## Value

`data` with the angle columns added.

## Examples

``` r
lay <- circos_layout(cor(mtcars))
head(track_angles(
    data.frame(id = colnames(mtcars)), "id", lay, 0.9, "col"
))
#>     id x         y .circos_theta .circos_arc       ymin      ymax
#> 1  mpg 0 0.2541834     0.2541834   0.5083668 0.02541834 0.4829485
#> 2  cyl 0 0.8253821     0.8253821   0.5083668 0.59661700 1.0541471
#> 3 disp 0 1.3965807     1.3965807   0.5083668 1.16781567 1.6253458
#> 4   hp 0 1.9677794     1.9677794   0.5083668 1.73901433 2.1965445
#> 5 drat 0 2.5389781     2.5389781   0.5083668 2.31021300 2.7677431
#> 6   wt 0 3.1101767     3.1101767   0.5083668 2.88141166 3.3389418
```
