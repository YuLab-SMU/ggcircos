# Changelog

## ggcircos 0.1.0

First release of the rewrite. The package is now built on **layout as
data**:
[`circos_layout()`](https://yulab-smu.github.io/ggcircos/reference/circos_layout.md)
computes every node arc and every link’s angular slot once, and the
geoms read that table instead of managing their own canvas. Requires
ggplot2 \>= 4.0 (it is built on
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)).

### Core

- [`ggcircos()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos.md)
  builds the layout and returns a plain `ggplot` object; add layers to
  it. Accepts a correlation matrix, a link table, or an igraph /
  `tbl_graph`.
- [`geom_chord()`](https://yulab-smu.github.io/ggcircos/reference/geom_chord.md)
  — ribbons or curves between node slots; `fill`/`colour` work with any
  ggplot2 scale and legend.
- [`geom_node_arc()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md),
  [`geom_node_text()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md),
  [`geom_node_point()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md)
  — the node ring.
- [`geom_track()`](https://yulab-smu.github.io/ggcircos/reference/geom_track.md)
  — columns, bars, areas, boxplots, violins, density curves, points,
  tiles or text in a radial band;
  [`geom_track_axis()`](https://yulab-smu.github.io/ggcircos/reference/geom_track_axis.md)
  — a readable ruler on a band. `geom = "violin"` fills each node’s
  density outline with the value along the radius; `geom = "density"`
  draws the same density as an open ridgeline curve, so several
  distributions can share one band. Both take `n`, `adjust` and `trim`,
  passed to [`stats::density()`](https://rdrr.io/r/stats/density.html).
- `sectors =` groups nodes into sectors with a wider gap;
  [`geom_sector_label()`](https://yulab-smu.github.io/ggcircos/reference/geom_sector_label.md)
  labels them.
- `weight =` makes the geometry follow the values, the way
  [`circlize::chordDiagram()`](https://rdrr.io/pkg/circlize/man/chordDiagram.html)
  does: node arcs proportional to the weight they carry and slots
  proportional to their link’s weight (`weight = r` on a matrix). Left
  unset, every node keeps an equal arc and every link an equal share, as
  before.
- [`layout_nodes()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md),
  [`layout_links()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md),
  [`layout_sectors()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md),
  [`layout_tracks()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md),
  [`layout_params()`](https://yulab-smu.github.io/ggcircos/reference/layout_nodes.md)
  export the layout as plain data frames.
- A node may occupy **several radii**: list it once per radius in the
  `nodes` table and the `r` column says which. The layers are radially
  aligned and take their own slots; links name their layer with `from_r`
  / `to_r`.
- `self_loops = TRUE` draws `from == to` links as small loops folded
  toward the centre, instead of dropping them; `loop_depth` sets how
  deep they fold.
- `by =` lays out one ring **per group of a node-table column**, each
  with its own node subset and ordering – the facet “B-plan” (spec
  §9.1). Finish with `facet_wrap(~ <the same column>)`. Chords, node
  geoms and tracks all work per panel; track rows are placed by their
  own group’s node table, so the track data needs the grouping column
  too.
- [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  splits chords per panel while the node ring is shared (the A-plan;
  `by =` is the alternative).

### Rendering

- Chords use `circlize`’s geometry: a quadratic Bezier in the plane,
  sampled by arc length, closed by the two node arcs. Ribbons keep their
  width across and meet the ring flush.
- Ribbons carry no outline by default (`colour = NA`, matching
  circlize’s `link.border = NA`); `type = "line"` chords draw black.
- Overlapping ribbons blend by alpha, so crossings read as a darker tint
  rather than a hole.
- [`geom_node_arc()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md)
  sits at `r = 1` (`offset = 0`), sharing an edge with the ribbon feet.
  Pass `offset =` for a visible gap.

### Inputs

- `columns =` maps a link table whose endpoints are spelled differently:
  `ggcircos(blast, columns = c(from = "qaccver", to = "saccver"))`. The
  rename copies, so the caller’s frame is untouched, and the columns you
  did not map travel with the link table – `weight = pident` and
  `aes(fill = pident)` still reach them. Covers BLAST table output,
  GFF3, BED and anything else without a method per format.

### Tracks

- A track with several rows per node now splits the node’s arc between
  them, whatever the geometry. `geom = "col"` used to stack every row on
  the whole arc, so three measurements per node drew as one solid band
  with no relief, and `geom = "area"` could not express a coverage
  profile at all. Only `geom = "boxplot"` keeps the full arc, since its
  stat aggregates the rows into one mark. A track with one row per node
  is unchanged, bin for bin.
- [`geom_track_axis()`](https://yulab-smu.github.io/ggcircos/reference/geom_track_axis.md)
  no longer needs `limits` repeated: omitting it takes the range from
  the track layer filling that band, so the axis cannot label a span the
  values were not scaled against. Pass `limits` to override it.

### Labels

- `geom_node_text(repel = TRUE)` staggers labels that would collide:
  walking the ring in angle order, one that lands within a line height
  of its neighbour moves outward along its own radius. The angle never
  changes, so every label stays on its node’s spoke. `ggrepel` is not
  used here – it resolves collisions in a rectangular space and lines
  the labels up flat.

### Layering

- [`ggcircos()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos.md)
  returns a plot that carries the link table but no mapping.
  `x`/`y`/`xend`/`yend` are mapped by
  [`geom_chord()`](https://yulab-smu.github.io/ggcircos/reference/geom_chord.md)
  itself, so a standard ggplot2 layer needs only its own
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) and
  `data`:

  ``` r

  ggcircos(m) + geom_rect(data = nd, aes(xmin = r, xmax = r + 0.04,
                                         ymin = theta0, ymax = theta1))
  ```

  Previously the chord columns sat on the plot, and every layer
  inherited them, which meant
  [`geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  failed on the inherited `x`/`y` and each layer had to opt out with
  `inherit.aes = FALSE`.

### Behaviour notes

- Each sector carries a wider gap at both of its ends, so sectors read
  as separate blocks. The angular budget reserves one seam per sector,
  and `sector_gap` is clamped to `0.2 / S`.
- `scales = "free"` on a track scales each node against its own range,
  not against the whole column.
- [`geom_node_text()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md)
  rejects `aes(angle = )` with `ggcircos_error_data`, since the
  coordinate owns the rotation; use `rotate = TRUE`.
- A violin crossing the angular seam is cut at it and continued on the
  other side, so the outline is never squished into a flat edge. A
  density curve is split at the seam without adding vertices, so the
  stroke stays a stroke.
- `stack = TRUE` is refused for `"boxplot"`, `"violin"` and `"density"`:
  their stat folds a node’s rows into one mark, so there is nothing left
  to stack.
- [`position_track()`](https://yulab-smu.github.io/ggcircos/reference/position_track.md)
  holds only vectors (`band`, the node column name, the normalisation
  settings), so one position can be shared by several layers. A layer
  may take its band from it instead of repeating `band =`.

### Not yet

An angular axis, interactive output. See
[`vignette("from-circlize")`](https://yulab-smu.github.io/ggcircos/articles/from-circlize.md).

### Deprecated shims

[`geom_circos()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos-deprecated.md),
[`geom_curve_link()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos-deprecated.md),
[`geom_link_lab()`](https://yulab-smu.github.io/ggcircos/reference/ggcircos-deprecated.md)
still run with a deprecation warning and map onto the new API; they will
be removed in 0.2.0.

## ggcircos 0.0.0.9000

- Legacy implementation (ggtreeExtra-based); superseded by 0.1.0.
