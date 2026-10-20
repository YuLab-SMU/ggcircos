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
* `layout_nodes()`, `layout_links()`, `layout_sectors()`, `layout_tracks()`,
  `layout_params()` export the layout as plain data frames.
* `facet_wrap()` splits chords per panel while the node ring is shared.

## Not yet

Self-loops, label collision avoidance, violin/density tracks, angular axes,
interactive output. See `vignette("from-circlize")`.

## Deprecated shims

`geom_circos()`, `geom_curve_link()`, `geom_link_lab()` still run with a
deprecation warning and map onto the new API; they will be removed in 0.2.0.

# ggcircos 0.0.0.9000

* Legacy implementation (ggtreeExtra-based); superseded by 0.1.0.
