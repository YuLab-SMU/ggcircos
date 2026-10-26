# ggcircos geoms, stats and positions

The building blocks behind
[`geom_chord()`](https://yulab-smu.github.io/ggcircos/reference/geom_chord.md),
[`geom_node_arc()`](https://yulab-smu.github.io/ggcircos/reference/geom_node.md),
[`geom_track()`](https://yulab-smu.github.io/ggcircos/reference/geom_track.md)
and
[`geom_track_axis()`](https://yulab-smu.github.io/ggcircos/reference/geom_track_axis.md).
Rarely needed directly: they are documented so that the layers' defaults
and geometry can be looked up.

`GeomPolygon` needs nothing beyond `x`/`y` and a numeric `group` per
polygon; the stat has already put the angles in drawing order.

`GeomPath` plus nothing: the stat has already placed every point, so the
geom only has to draw the line. Kept as its own class (rather than a
bare `"path"`) so `geom_track(geom = "density")` can carry density
defaults and so a future option has somewhere to live.
