# ggcircos 0.1.0

First release of the rewrite. The package is now built on **layout as data**:
`circos_layout()` computes every node arc and every link's angular slot once,
and the geoms read that table instead of managing their own canvas. Requires
ggplot2 >= 4.0 (it is built on `coord_radial()`).

## Core

* `ggcircos()` builds the layout and returns a plain `ggplot` object; add
  layers to it. Accepts a correlation matrix, a link table, or an
  igraph / `tbl_graph`.
* `geom_chord()` — ribbons or curves between node slots; `fill`/`colour`
  work with any ggplot2 scale and legend.
* `geom_node_arc()`, `geom_node_text()`, `geom_node_point()` — the node ring.
* `geom_track()` — columns, bars, areas, boxplots, points, tiles or text in a
  radial band; `geom_track_axis()` — a readable ruler on a band.
* `sectors =` groups nodes into sectors with a wider gap; `geom_sector_label()`
  labels them.
* `weight =` makes the geometry follow the values, the way `circlize::chordDiagram()`
  does: node arcs proportional to the weight they carry and slots proportional to
  their link's weight (`weight = r` on a matrix). Left unset, every node keeps an
  equal arc and every link an equal share, as before.
* `layout_nodes()`, `layout_links()`, `layout_sectors()`, `layout_tracks()`,
  `layout_params()` export the layout as plain data frames.
* `facet_wrap()` splits chords per panel while the node ring is shared.

## Rendering

* Chords use `circlize`'s geometry: a quadratic Bezier in the plane, sampled
  by arc length, closed by the two node arcs. Ribbons keep their width across
  and meet the ring flush.
* Ribbons carry no outline by default (`colour = NA`, matching circlize's
  `link.border = NA`); `type = "line"` chords draw black.
* Overlapping ribbons blend by alpha, so crossings read as a darker tint
  rather than a hole.
* `geom_node_arc()` sits at `r = 1` (`offset = 0`), sharing an edge with the
  ribbon feet. Pass `offset =` for a visible gap.

## Behaviour notes

* Each sector carries a wider gap at both of its ends, so sectors read as
  separate blocks. The angular budget reserves one seam per sector, and
  `sector_gap` is clamped to `0.2 / S`.
* `scales = "free"` on a track scales each node against its own range, not
  against the whole column.
* `geom_node_text()` rejects `aes(angle = )` with `ggcircos_error_data`, since
  the coordinate owns the rotation; use `rotate = TRUE`.
* `position_track()` holds only vectors (`band`, the node column name, the
  normalisation settings), so one position can be shared by several layers.
  A layer may take its band from it instead of repeating `band =`.

## Not yet

Self-loops, label collision avoidance, violin/density tracks, angular axes,
interactive output. See `vignette("from-circlize")`.

## Deprecated shims

`geom_circos()`, `geom_curve_link()`, `geom_link_lab()` still run with a
deprecation warning and map onto the new API; they will be removed in 0.2.0.

# ggcircos 0.0.0.9000

* Legacy implementation (ggtreeExtra-based); superseded by 0.1.0.
