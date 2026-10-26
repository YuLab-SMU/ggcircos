# Build a circular layout

A `circos_layout` turns a set of nodes and links into polar coordinates.
Everything a plot needs is computed *once*, here, and returned as data:
the arc of every node, the sector each node belongs to, and the angular
slot reserved for every link endpoint. Layers never recompute any of it.

## Usage

``` r
circos_layout(
  x,
  nodes = NULL,
  sectors = NULL,
  order = NULL,
  gap = 0.01,
  sector_gap = 0.05,
  start = 0,
  slot_fill = 0.9,
  slot_gap = NULL,
  self_loops = FALSE,
  loop_depth = 0.5,
  tracks = NULL,
  track_offset = 0.06,
  radius = 1,
  weight = NULL,
  by = NULL,
  ...
)

# S3 method for class 'circos_layout'
circos_layout(x, ...)

# S3 method for class 'matrix'
circos_layout(x, nodes = NULL, ..., columns = NULL)

# S3 method for class 'data.frame'
circos_layout(x, nodes = NULL, ..., columns = NULL)

# S3 method for class 'tbl_graph'
circos_layout(x, nodes = NULL, ..., columns = NULL)

# S3 method for class 'igraph'
circos_layout(x, nodes = NULL, ..., columns = NULL)
```

## Arguments

- x:

  A square `matrix`, a `data.frame` with `from`/`to` columns, a
  `tidygraph`/`igraph` object, or an existing `circos_layout`.

- nodes:

  An optional node table. Needs an `id` column; `label`, `sector` and
  any further columns are carried through to the layers.

- sectors:

  Sector assignment: either the name of a column of `nodes`, or a named
  character vector mapping node ids to sectors. Nodes are grouped in the
  order given (first appearance, or the order of the vector's names).

- order:

  Node order around the circle: `NULL` keeps the input order, `"hclust"`
  clusters nodes by link strength, a character vector gives an explicit
  order, and a function is called with the node table and must return
  node ids.

- gap:

  Fraction of a full turn left as a gap between neighbouring nodes.

- sector_gap:

  Extra fraction of a turn left between sectors.

- start:

  Angle (radians) of the first node arc.

- slot_fill:

  Fraction of a node's arc that its link slots occupy (slots are centred
  inside the arc).

- slot_gap:

  Absolute seam in radians between adjacent link slots on a node, the
  way
  [`circlize::chordDiagram()`](https://rdrr.io/pkg/circlize/man/chordDiagram.html)'s
  `diffHeight` is a fixed height. `NULL` (the default) keeps the seam at
  the `1 - slot_fill` fraction, which shrinks with the node's arc; a
  fixed value keeps the ribbons visibly separate on a wide ring with
  many links.

- self_loops:

  Draw self-links (`from == to`) as small loops folded toward the centre
  (spec sec.4.7). `FALSE` (the default) drops them with a message.

- loop_depth:

  How far a self-loop folds toward the centre, as a fraction of the
  node's radius at most: the depth is
  `loop_depth * slot_span / node_width` of the radius, so a narrow node
  gets a shallow loop. In `(0, 1]`. Ignored when `self_loops = FALSE`.

- tracks:

  Named numeric vector of outer track widths, e.g.
  `c(values = 0.2, heat = 0.1)`. Widths are relative to the unit node
  ring.

- track_offset:

  Gap between the node ring, and between tracks. The first track band
  starts at `1 + track_offset`, so keep it at or above the node ring's
  outer edge (0.05 with the default
  [`geom_node_arc()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md)).

- radius:

  Base radius of the node ring. `0` puts the ring on the spine of a
  linear
  ([`coord_arc()`](https://yulab-smu.github.io/ggcircos/reference/coord_circos.md))
  layout.

- weight:

  Name of the column holding each link's weight, e.g. `weight = "r"`.
  When set, the geometry follows the values: a node's arc is
  proportional to the weight it carries and a slot is proportional to
  its link's weight (negative values count by magnitude). `NULL`, the
  default, gives every node an equal arc and every link an equal share
  of it – the value then only shows through `aes(fill = )`.

- by:

  Name of a **node-table** column to lay out one ring per group of its
  values (spec sec.9.1). Returns a `circos_layouts` list, each element a
  full layout with a `panel` attribute; a link belongs to the group
  named in its own column of the same name, or – without one – to the
  group of its two ends (a link spanning groups is dropped). `NULL`
  builds one layout.

- ...:

  Used by the methods to build the node table from graph objects.

- columns:

  Optional named character vector mapping the layout's column names onto
  your data's, e.g. `c(from = "qaccver", to = "saccver")` for BLAST
  table output. The rename is a copy; columns you do not map travel with
  the link table, so `weight =` and
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) can still
  reach them.

## Value

An object of class `circos_layout`, or – when `by` is given – a
`circos_layouts` list of them.

## See also

[`ggcircos()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos.md),
[`layout_nodes()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md),
[`layout_links()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md)

## Examples

``` r
m <- cor(mtcars)
l <- circos_layout(m, order = "hclust", tracks = c(values = 0.2))
head(layout_nodes(l))
#>     id label sector order n_links    theta0    theta1     theta     width r
#> 1 carb  carb    all     1      10 0.0000000 0.5083668 0.2541834 0.5083668 1
#> 2   wt    wt    all     2      10 0.5711987 1.0795655 0.8253821 0.5083668 1
#> 3   hp    hp    all     3      10 1.1423973 1.6507641 1.3965807 0.5083668 1
#> 4  cyl   cyl    all     4      10 1.7135960 2.2219628 1.9677794 0.5083668 1
#> 5 disp  disp    all     5      10 2.2847947 2.7931615 2.5389781 0.5083668 1
#> 6 qsec  qsec    all     6      10 2.8559933 3.3643601 3.1101767 0.5083668 1
head(layout_links(l))
#>   from   to          r self_loop .circos_w .circos_from_arc .circos_to_arc
#> 1  mpg  cyl -0.8521620     FALSE         1                8              4
#> 2  mpg disp -0.8475514     FALSE         1                8              5
#> 3  cyl disp  0.9020329     FALSE         1                4              5
#> 4  mpg   hp -0.7761684     FALSE         1                8              3
#> 5  cyl   hp  0.8324475     FALSE         1                4              3
#> 6 disp   hp  0.7909486     FALSE         1                5              3
#>   edge_id   theta0   theta1    theta theta_end0 theta_end1 theta_end x        y
#> 1       1 4.153443 4.199196 4.176319   1.739014   2.196544  1.967779 1 4.176319
#> 2       2 4.204279 4.250032 4.227156   2.551687   2.780452  2.666070 1 4.227156
#> 3       3 1.885876 1.936713 1.911294   2.297504   2.526269  2.411886 1 1.911294
#> 4       4 4.102606 4.148359 4.125482   1.489781   1.642291  1.566036 1 4.125482
#> 5       5 1.829391 1.880227 1.854809   1.150870   1.303380  1.227125 1 1.854809
#> 6       6 2.415064 2.472255 2.443659   1.320326   1.472836  1.396581 1 2.443659
#>   xend     yend .circos_node_width .circos_loop_depth
#> 1    1 1.967779          0.5083668                0.5
#> 2    1 2.666070          0.5083668                0.5
#> 3    1 2.411886          0.5083668                0.5
#> 4    1 1.566036          0.5083668                0.5
#> 5    1 1.227125          0.5083668                0.5
#> 6    1 1.396581          0.5083668                0.5
layout_tracks(l)
#>    track width   r0   r1
#> 1 values   0.2 1.06 1.26
```
