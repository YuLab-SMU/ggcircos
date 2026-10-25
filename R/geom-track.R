#' Circular tracks
#'
#' A track is an ordinary ggplot2 layer whose angle comes from the plot's
#' layout and whose radius is scaled into a declared radial band. Node ids
#' are translated to angles when the layer is added; the scale factors live
#' in a [position_track()] position, so the same normalisation is reused by
#' every track and can be applied by hand to your own geoms.
#'
#' @param mapping,data,show.legend,inherit.aes,na.rm,...
#'   Standard [ggplot2::layer()] arguments.
#' @param geom Which track geometry to use: `"col"`/`"bar"` (anchored at the
#'   band's inner edge), `"area"`, `"boxplot"`, `"violin"`, `"density"`,
#'   `"point"`, `"tile"` or `"text"`. The last three sit at the band's centre
#'   when no `x` is mapped; with an `x` they land at the scaled value, the way a
#'   bar's outer edge does. `"violin"` and `"density"` both put a node's density
#'   along its arc, with the value along the radius: `"violin"` fills the
#'   outline, `"density"` draws it as an open curve, so several distributions
#'   can share a band without covering each other. Both take `n`, `adjust` and
#'   `trim`, passed to [stats::density()] (see `...`).
#' @param node Unquoted name of the column holding node ids. Required.
#' @param value Unquoted name of the column holding the values to plot, e.g.
#'   `value = gc`. Optional (`aes(x = gc)` also works), but it is the robust
#'   spelling: `value` is handed to the position directly, so it keeps working
#'   when the data has a column literally called `x` and it makes `stack`
#'   able to reject negative input instead of silently drawing a flat track.
#' @param band Either a declared track name (see the `tracks` argument of
#'   [ggcircos()]) or an explicit `c(r0, r1)` radius interval.
#' @param stack Stack values within each node instead of scaling them
#'   independently. Values must then be non-negative. Not available for the
#'   aggregating geoms (`"boxplot"`, `"violin"`, `"density"`), whose stat has
#'   already folded each node into one mark.
#' @param scales `"free"` normalises each node against its own range;
#'   `"fixed"` uses `limits` for every node.
#' @param limits Value range of the track; required when `scales = "fixed"`.
#' @param baseline Lower bound when `scales = "free"`. By default `0` if all
#'   values are non-negative, else the smallest value.
#' @param width Fraction of a node's arc covered by the layer.
#' @param offset Distance from the band's inner edge. Only meaningful when
#'   `band` is an explicit interval.
#' @param slot Optional: a column of the track data holding the **link row
#'   number** (`edge_id`) each row belongs to. Given it, the row occupies the
#'   arc that its link's slot occupies -- a track drawn per link sits exactly
#'   on the link's foot, the way circlize's per-link annotation rectangles do
#'   (spec §4.10). Without it, a node's rows split its arc evenly.
#' @param domain Optional: a **node-table** column holding each node's
#'   coordinate ceiling (a chromosome length). Only used by interval tracks
#'   (`aes(xmin = start, xmax = end)`), where it decides the scale a node's
#'   `start`-to-`end` range is normalised against, per node. Without it each node is
#'   scaled to the largest `xmax` its own rows report.
#' @param origin Lower bound of the interval coordinates, `0` by default.
#'   Give `1` for a 1-based coordinate system.
#' @param position A [position_track()] object. `NULL` (the default) builds
#'   one from this layer's `band`/`stack`/`scales`/`limits`/`node` arguments;
#'   pass an existing one to reuse the same normalisation across layers (see
#'   the escape hatch under [position_track()]). When given, those settings
#'   come from the position, not from this call.
#'
#' @return A ggplot2 layer.
#' @section Normalisation:
#' With band `[r0, r1]` and per-node scale `"free"`, a value `v` maps to
#' `r0 + (r1 - r0) * (v - base) / (max - base)`, where `base` is `baseline`.
#' With `"fixed"`, `base` and `max` are `limits`. With `stack = TRUE`, values
#' are converted to within-node fractions first. `"boxplot"` maps its
#' `xmin`/`xmax` quantiles through the same affine map; anchored geoms
#' stretch from `r0` to the mapped value. `"violin"` and `"density"` map the
#' values they sampled the density at, so the distribution is turned sideways
#' inside the band: broad where the samples pile up, narrow where they thin
#' out.
#'
#' @examples
#' set.seed(1)
#' vals <- data.frame(
#'     id = rep(colnames(mtcars), each = 4),
#'     value = runif(ncol(mtcars) * 4),
#'     stringsAsFactors = FALSE
#' )
#' ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
#'     geom_chord(aes(fill = r)) +
#'     geom_track(data = vals, aes(x = value), node = id, geom = "col", band = "values")
#'
#' # a violin: several samples per node, the density spread along the arc
#' set.seed(2)
#' samples <- data.frame(
#'     id = rep(colnames(mtcars), each = 40),
#'     value = rnorm(ncol(mtcars) * 40, 0.5, 0.1),
#'     stringsAsFactors = FALSE
#' )
#' ggcircos(cor(mtcars), order = "hclust", tracks = c(dist = 0.3)) +
#'     geom_node_arc() +
#'     geom_track(data = samples, aes(x = value), node = id,
#'                geom = "violin", band = "dist")
#'
#' # the same density as an open curve, for comparing several nodes in one band
#' ggcircos(cor(mtcars), order = "hclust", tracks = c(dist = 0.3)) +
#'     geom_node_arc() +
#'     geom_track(data = samples, aes(x = value), node = id,
#'                geom = "density", band = "dist", colour = "steelblue")
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @export
geom_track <- function(mapping = NULL,
                       data = NULL,
                       ...,
                       geom = "col",
                       node = NULL,
                       value = NULL,
                       band,
                       stack = FALSE,
                       scales = c("free", "fixed"),
                       limits = NULL,
                       baseline = NULL,
                       width = 0.9,
                       offset = 0,
                       position = NULL,
                       slot = NULL,
                       domain = NULL,
                       origin = 0,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = FALSE) {
    # A position built by `position_track()` already carries its band, so
    # repeating it on the layer is redundant. Only a layer with neither has
    # nothing to draw into.
    # An interval track places a *span* on the arc (`xmin`/`xmax`) instead of
    # a point; this predicate is asked several times below (spec sec.4.11).
    has_interval <- function(nm) nm %in% names(mapping) || nm %in% names(data)
    # the columns behind `aes(xmin = , xmax = )`: an interval row needs their
    # *names*, not the aesthetic names (a data column is called `start`)
    interval_cols <- lapply(c(lo = "xmin", hi = "xmax"), function(nm) {
        if (nm %in% names(mapping)) {
            expr <- rlang::quo_get_expr(mapping[[nm]])
            if (rlang::is_symbol(expr)) return(rlang::as_name(expr))
        }
        if (nm %in% names(data)) return(nm)
        NULL
    })
    if (missing(band) && (is.null(position) || is.null(position$band))) {
        ggcircos_abort(
            "band",
            "{.fn geom_track} needs a {.arg band}: a declared track name or an explicit {.code c(r0, r1)}."
        )
    }
    if (missing(band)) band <- NULL
    spec <- track_geom_spec(geom)
    scales <- match.arg(scales)
    # A stacked band accumulates one value per row up the band. An aggregating
    # geometry has already folded the rows into one mark per node by the time
    # the position runs, so stacking it would silently draw a single mark at
    # the top of the band -- spec sec.6.4 rejects the combination instead.
    if (isTRUE(stack) && isTRUE(spec$aggregate)) {
        ggcircos_abort(
            "data",
            c(
                "{.code stack = TRUE} cannot be combined with {.val {geom}}: the \\
                 stat collapses each node to one mark, so there is nothing to stack.",
                i = "Use {.val col} or {.val area} for a stacked band."
            )
        )
    }
    if (!rlang::quo_is_null(rlang::enquo(slot)) && has_interval("xmin")) {
        ggcircos_abort(
            "data",
            c(
                "A row cannot both sit on a link's slot and be placed by genomic \
                 coordinates.",
                i = "Drop either {.arg slot} or {.field xmin}/{.field xmax}."
            )
        )
    }
    if (is.null(data) || is_waive(data)) {
        ggcircos_abort(
            "data",
            c(
                "{.fn geom_track} needs its own {.arg data}: track values are per node, \\
                 not part of the link table.",
                i = "Pass a data frame with the node column and the value column."
            )
        )
    }
    node_quo <- rlang::enquo(node)
    # -- interval tracks (spec sec.4.11) --------------------------------------
    # A genomic feature is a range, not a point: `chr + start + end + value`.
    # The range occupies an angular span inside its node's arc, so the value
    # model is unchanged -- only the *position along the arc* moves from
    # "evenly divided rows" to "the caller's [start, end]".
    domain_col <- if (rlang::quo_is_null(rlang::enquo(domain))) NULL else
        column_or_string(rlang::enquo(domain), "domain")
    # `x` and `xmin/xmax` are two different positioning models: one places a
    # point on the arc, the other a span. Mixing them has no meaning.
    if (has_interval("xmin") && has_interval("x") &&
        !has_interval(".circos_has_value")) {
        # `x` may legitimately carry the *radius* value while xmin/xmax carry
        # the arc; that combination is fine and is handled below. What is not
        # fine is `x` pretending to be the arc position at the same time.
    }
    if (has_interval("xmin") != has_interval("xmax")) {
        ggcircos_abort(
            "data",
            c(
                "An interval track needs {.field xmin} and {.field xmax} together.",
                i = "Map both: {.code aes(xmin = start, xmax = end)}."
            )
        )
    }
    # `value = column` is the explicit way to hand the position its numbers;
    # `ggplot_add` resolves it against the layer's own data (the layer data is
    # built there, after anything the caller passed). `aes(x = column)` also
    # works, but ggplot2 resolves aesthetics before the position runs and a
    # data column literally named `x` replaces the mapped value with a
    # constant -- the bug that silently zeroed every row and hid the
    # negative-input check (review R4).
    value_name <- NULL
    value_quo <- rlang::enquo(value)
    if (!rlang::quo_is_null(value_quo)) {
        value_col <- column_or_string(value_quo, "value")
        if (!value_col %in% names(data)) {
            ggcircos_abort(
                "data",
                c("{.arg value} refers to {.field {value_col}}, which the track data does not have.",
                  i = "Available: {.val {names(data)}}.")
            )
        }
        # Keep the value under a name that cannot be confused with `x`, and
        # map through `.data` so the layer data stays the single source.
        if (identical(value_col, "x")) {
            data$.circos_value <- data$x
            data$x <- NULL
            value_col <- ".circos_value"
        }
        # `x` still needs a mapping (GeomRect derives xmin/xmax from it), but
        # it must point at a column that is NOT called `x`: a data column
        # literally named `x` wins over the mapping and zeroes every value.
        data$.circos_has_value <- TRUE
        value_name <- value_col
        mapping <- if (is.null(mapping)) ggplot2::aes() else mapping
        mapping$x <- rlang::sym(value_col)
    }
    if (is.null(position)) {
        node_col <- column_or_string(node_quo, "node")
        position <- position_track(
            band = band, node = node_col, geom_name = geom,
            anchored = spec$anchored, band_fill = spec$band,
            stack = stack, scales = scales,
            limits = limits, baseline = baseline, width = width, offset = offset
        )
    } else if (!inherits(position, "PositionTrack")) {
        ggcircos_abort(
            "data",
            c(
                "{.arg position} must come from {.fn position_track}, not {.cls {class(position)[1]}}.",
                i = "Leave {.arg position = NULL} to let {.fn geom_track} build one."
            )
        )
    } else {
        # an external position owns the normalisation settings; the node
        # column is still needed to turn node ids into angles, so fall back
        # to the one set on the position
        node_col <- if (rlang::quo_is_null(node_quo)) position$node else
            column_or_string(node_quo, "node")
        if (is.null(node_col)) {
            ggcircos_abort(
                "data",
                c(
                    "{.fn geom_track} needs to know which column holds the node ids.",
                    i = "Pass {.arg node}, or set it on the {.fn position_track}."
                )
            )
        }
    }
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = spec$stat,
            geom = spec$geom,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        ),
        "ggcircos_track",
        list(band = position$band, band_name = position$band_name,
             geom_name = geom, anchored = spec$anchored,
             node = node_col, width = position$width,
             value_name = value_name,
             # what the values were normalised against, so a later
             # geom_track_axis() can label the band without being told twice
             scales = position$scales, limits = position$limits,
             baseline = position$baseline,
             slot = if (!rlang::quo_is_null(rlang::enquo(slot)))
               column_or_string(rlang::enquo(slot), "slot") else NULL,
             # spec sec.4.11: interval rows carry their own arc
             interval = has_interval("xmin"),
             interval_lo = interval_cols$lo,
             interval_hi = interval_cols$hi,
             domain = domain_col,
             origin = origin)
    )
}

#' Geometry whitelist and how each entry is assembled
#'
#' `anchored` geoms stretch from the band's inner edge to the mapped value;
#' the others sit at the band's centre when no value is mapped. `rect` marks
#' the entries that need `ymin`/`ymax`. `band` marks the entries that fill
#' the whole radial band (`tile`): their `xmin`/`xmax` are the band edges,
#' not a mapped value, and their angular extent comes from the bin split in
#' [track_angles()]. `aggregate` marks the entries whose stat folds a node's
#' rows into one mark (`boxplot`, `violin`, `density`): that mark keeps the
#' node's whole arc instead of one bin of it, and the stat needs `node` mapped
#' to group by.
#' @keywords internal
#' @noRd
track_geom_spec <- function(geom) {
    specs <- list(
        col = list(stat = "identity", geom = GeomTrackBar, anchored = TRUE, rect = TRUE, band = FALSE, aggregate = FALSE),
        bar = list(stat = "identity", geom = GeomTrackBar, anchored = TRUE, rect = TRUE, band = FALSE, aggregate = FALSE),
        area = list(stat = "identity", geom = GeomTrackArea, anchored = TRUE, rect = TRUE, band = FALSE, aggregate = FALSE),
        boxplot = list(stat = StatTrackBox, geom = GeomTrackCrossbar, anchored = FALSE, rect = TRUE, band = FALSE, aggregate = TRUE),
        violin = list(stat = StatTrackViolin, geom = GeomTrackViolin, anchored = FALSE, rect = TRUE, band = FALSE, aggregate = TRUE),
        density = list(stat = StatTrackDensity, geom = GeomTrackDensity, anchored = FALSE, rect = TRUE, band = FALSE, aggregate = TRUE),
        point = list(stat = "identity", geom = GeomTrackPoint, anchored = FALSE, rect = FALSE, band = FALSE, aggregate = FALSE),
        tile = list(stat = "identity", geom = GeomTrackTile, anchored = FALSE, rect = TRUE, band = TRUE, aggregate = FALSE),
        text = list(stat = "identity", geom = GeomTrackText, anchored = FALSE, rect = FALSE, band = FALSE, aggregate = FALSE)
    )
    if (!is.character(geom) || length(geom) != 1L || !geom %in% names(specs)) {
        ggcircos_abort(
            "data",
            "Unknown {.arg geom} {.val {geom}} for {.fn geom_track}.",
            i = "Available: {.field {names(specs)}}, or use {.val position_track} on your own geom."
        )
    }
    specs[[geom]]
}

#' Add the angle columns a track layer needs
#'
#' Translates the node id column into `y` (the node's angle) and `ymin`/`ymax`
#' (its arc, shrunk by `width`; for `"tile"`, the arc split into one bin per
#' row of that node). This runs once, when the layer is added, because
#' ggplot2 runs positions *after* the geom setup: anything a geom lists in
#' `required_aes` must exist by then. The radial extent of a tile band is the
#' position's job, not this function's.
#'
#' @param data Layer data.
#' @param node Name of the node id column.
#' @param layout A `circos_layout` (or its node table).
#' @param width Fraction of the node arc used by the layer.
#' @param geom Track geometry name.
#' @param has_value Whether the caller already supplies the values, either as
#'   a data column named `x` or through a mapping. When `TRUE` no placeholder
#'   column is written, because a column called `x` would shadow the layer's
#'   own mapping and every value would scale to the same radius.
#' @param ... Ignored; kept so older calls that passed `band_width` still work.
#'   `aggregate` sits after `...` on purpose, so it can only be given by name:
#'   a positional argument there would silently land on it.
#' @param aggregate Whether this geom's stat folds a node's rows into one mark
#'   (`"boxplot"`, `"violin"`). Such a mark keeps the node's whole arc rather
#'   than one bin of it. `NULL` asks `track_geom_spec()`; pass it explicitly
#'   when calling this function with a geometry of your own.
#' @param layouts A `circos_layouts` list (the `by = ` result) instead of a
#'   single layout: every row is then placed by the node table of its own
#'   group, so two rings may put the same id at different angles.
#' @param group Name of the column in `data` that says which group a row
#'   belongs to. Required with `layouts`.
#' @return `data` with the angle columns added.
#' @examples
#' lay <- circos_layout(cor(mtcars))
#' head(track_angles(
#'     data.frame(id = colnames(mtcars)), "id", lay, 0.9, "col"
#' ))
#' @export
track_angles <- function(data, node, layout, width = 0.9, geom = "col",
                         has_value = FALSE, ..., aggregate = NULL,
                         layouts = NULL, group = NULL) {
    node_col <- column_or_string(rlang::enquo(node), "node")
    if (is.null(aggregate)) aggregate <- track_aggregates(geom)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    if (!node_col %in% names(data)) {
        ggcircos_abort("data", "The data has no node column {.field {node_col}}.")
    }
    ids <- as.character(data[[node_col]])

    # per-row node key for the bin split: (group, id) under the B-plan, id
    # alone otherwise. The split groups rows of the SAME node in the SAME ring,
    # which is exactly what ymin/ymax must tile.
    if (!is.null(layouts)) {
        gkey <- if (is.null(group)) NULL else as.character(data[[group]])
        if (is.null(gkey) || !group %in% names(data)) {
            ggcircos_abort(
                "data",
                c(
                    "The track data has no {.field {group}} column, so its rows cannot be \
                     placed on a multi-ring plot.",
                    i = "Add the same column the {.arg by = } layout uses, so each row \
                         knows which ring it belongs to."
                )
            )
        }
        theta <- rep(NA_real_, nrow(data))
        arc <- rep(NA_real_, nrow(data))
        for (k in names(layouts)) {
            rows <- which(gkey == k)
            if (!length(rows)) next
            nt_k <- layouts[[k]]$nodes
            idx <- match(ids[rows], nt_k$id)
            if (anyNA(idx)) {
                bad <- unique(ids[rows][is.na(idx)])
                ggcircos_abort(
                    "node",
                    "Track data in group {.val {k}} refers to unknown node{?s} {.val {head(bad, 5)}}.",
                    i = "Known nodes there: {.val {head(nt_k$id, 10)}}."
                )
            }
            a <- (nt_k$theta1[idx] - nt_k$theta0[idx]) %% (2 * pi)
            a[is.na(a)] <- nt_k$width[idx][is.na(a)]
            theta[rows] <- nt_k$theta[idx]
            arc[rows] <- a
        }
        if (anyNA(theta)) {
            ggcircos_abort(
                "data",
                "Some track rows name a group no ring was built for; every row needs a \
                 group the layout knows."
            )
        }
        key <- paste(gkey, ids, sep = "\r")
        return(track_finish_angles(data, key, theta, arc, width, aggregate, has_value))
    }

    nt <- if (inherits(layout, "circos_layout")) layout$nodes else layout
    if (is.null(nt)) {
        ggcircos_abort("layout", "{.arg layout} must be a {.cls circos_layout} or a node table.")
    }
    idx <- match(ids, nt$id)
    if (anyNA(idx)) {
        bad <- unique(ids[is.na(idx)])
        ggcircos_abort(
            "node",
            "Track data refers to unknown node{?s} {.val {head(bad, 5)}}.",
            i = "Known nodes: {.val {head(nt$id, 10)}}."
        )
    }
    arc <- (nt$theta1[idx] - nt$theta0[idx]) %% (2 * pi)
    arc[is.na(arc)] <- nt$width[idx][is.na(arc)]
    theta <- nt$theta[idx]
    track_finish_angles(data, key = idx, theta = theta, arc = arc,
                        width = width, aggregate = aggregate, has_value = has_value)
}

#' Place interval rows by their own [start, end] (spec sec.4.11)
#'
#' A genomic feature is a range, not a point. The range occupies an angular
#' span inside its node's arc; the radius is still the value's job, so this
#' only computes `ymin`/`ymax` and marks the frame so
#' `track_finish_angles()` leaves the span alone (spec sec.4.11-5b).
#'
#' Each node is normalised on its own: `pos = (v - origin) / (L_i - origin)`,
#' where `L_i` is the node's `domain` value (its chromosome length) or, without
#' one, the largest `xmax` seen for that node. Nodes are different circles of
#' the same ring, so a 100 Mb chromosome and a 5 Mb contig both fill their own
#' arc -- that is the point of a per-node domain, not an accident.
#' @keywords internal
#' @noRd
track_place_intervals <- function(data, nodes, ids, domain = NULL, origin = 0,
                                  width = 0.9, lo = "xmin", hi = "xmax") {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    # `lo`/`hi` name the *data* columns behind `aes(xmin = , xmax = )`:
    # `aes(xmin = start)` leaves a column called `start`.
    if (!all(c(lo, hi) %in% names(data))) {
        ggcircos_abort(
            "data",
            c(
                "An interval track needs {.field xmin} and {.field xmax}.",
                i = "Map them on the layer: {.code aes(xmin = start, xmax = end)}."
            )
        )
    }
    if (!is.numeric(data[[lo]]) || !is.numeric(data[[hi]])) {
        ggcircos_abort("data", "{.field xmin} and {.field xmax} must be numeric.")
    }
    if (anyNA(data[[lo]]) || anyNA(data[[hi]])) {
        ggcircos_abort("data", "Interval bounds have missing values.")
    }
    if (any(data[[lo]] > data[[hi]])) {
        ggcircos_abort(
            "data",
            c(
                "{.field xmin} must not be greater than {.field xmax}.",
                i = "Swap the two columns, or check the rows where start > end."
            )
        )
    }
    idx <- match(ids, nodes$id)
    if (anyNA(idx)) {
        bad <- unique(ids[is.na(idx)])
        ggcircos_abort(
            "node",
            "Interval data refers to unknown node{?s} {.val {head(bad, 5)}}.",
            i = "Known nodes: {.val {head(nodes$id, 10)}}."
        )
    }
    arc <- (nodes$theta1[idx] - nodes$theta0[idx]) %% (2 * pi)
    arc[is.na(arc)] <- nodes$width[idx][is.na(arc)]
    theta0 <- nodes$theta0[idx]

    # the node's coordinate ceiling: the caller's `domain` column, or the
    # largest upper bound the node's own data reports
    if (is.null(domain)) {
        upper <- stats::ave(data[[hi]], ids, FUN = max)
        upper <- as.numeric(upper)
    } else {
        if (!domain %in% names(nodes)) {
            ggcircos_abort(
                "data",
                c(
                    "{.arg domain} refers to {.field {domain}}, which the node table does not have.",
                    i = "Available: {.val {names(nodes)}}.",
                    i = "{.arg domain} names a column of {.arg nodes}: a length belongs to \
                         the node, not to the row."
                )
            )
        }
        upper <- as.numeric(nodes[[domain]][idx])
        if (anyNA(upper)) {
            ggcircos_abort(
                "data",
                "{.arg domain} column {.field {domain}} has missing values; every node needs a length."
            )
        }
    }
    span <- upper - origin
    # a node with no extent cannot place anything: say so rather than divide
    # by zero and hand NaN angles to the coordinate
    if (any(!is.finite(span)) || any(span <= 0)) {
        bad <- unique(ids[!is.finite(span) | span <= 0])
        ggcircos_abort(
            "data",
            c(
                "Node{?s} {.val {head(bad, 5)}} {?has/have} no coordinate extent to place \
                 intervals in.",
                i = "Give {.arg domain} a positive length, or check {.field xmax}."
            )
        )
    }
    pos_a <- (data[[lo]] - origin) / span
    pos_b <- (data[[hi]] - origin) / span
    # `width` shrinks the usable arc so neighbouring nodes' intervals cannot
    # touch -- the same convention the evenly-divided rows follow
    usable <- arc * width
    data$ymin <- theta0 + pos_a * usable
    data$ymax <- theta0 + pos_b * usable
    data$y <- theta0 + (pos_a + pos_b) / 2 * usable
    data$.circos_theta <- data$y
    data$.circos_arc <- usable
    data$.circos_half <- (data$ymax - data$ymin) / 2
    # `GeomRect` still asks for `x`, and `track_defaults()` maps it
    # unconditionally. An interval row positions itself by `xmin`/`xmax`, so
    # give it the row midpoint as its `x` -- the band's radial placement then
    # finds a value and the rect is drawn from xmin/xmax anyway.
    if (!"x" %in% names(data)) data$x <- data[[lo]]
    data
}

#' Place track rows onto their own link's slots (spec §4.10)
#'
#' `slot =` names a column of the track data holding the link row number
#' (`edge_id`) each row belongs to. The row then occupies the arc that its
#' link's slot occupies, so a per-link track sits exactly on the link's foot --
#' the thing circlize's ch.16 methylation rectangles do by accumulating
#' `x1 - value1` by hand.
#' @keywords internal
#' @noRd
track_align_slots <- function(data, slot, links, group_key = NULL, node = NULL) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    node_col <- node
    if (!slot %in% names(data)) {
        ggcircos_abort(
            "data",
            c("The track data has no {.field {slot}} column.",
              i = "It names the link each row belongs to, e.g. {.code slot = edge_id}.")
        )
    }
    eid <- as.integer(data[[slot]])
    if (anyNA(eid) || any(eid < 1L)) {
        ggcircos_abort(
            "data",
            "The {.field {slot}} column must hold positive link row numbers ({.field edge_id})."
        )
    }
    # a B-plan link table carries one row per (group, link); the (group,
    # edge_id) pair picks the right panel's slot
    if (!is.null(group_key) && !group_key %in% names(data)) {
        ggcircos_abort(
            "data",
            c(
                "The track data has no {.field {group_key}} column, so its rows cannot be \
                 matched to a ring.",
                i = "Add the same column the {.arg by = } layout uses."
            )
        )
    }
    # Which column of the stacked link table holds the caller's numbering.
    #
    # On a `by =` plot every panel numbers its own links from 1, so the stacked
    # `edge_id` repeats (1, 1, ...) and matching on it picks the wrong panel.
    # `allocate_slots()` renames a caller-supplied `edge_id` to a
    # `user_edge_id`-style name (B48; `make.unique()` may have appended a
    # suffix when the caller already uses that name, B56) precisely so that
    # numbering survives -- that is the column to match on when the layout has
    # renumbered (B49). `make.unique()` appends in order, so the *last*
    # `user_edge_id*` column in name order is the protected one.
    user_cols <- grep("^user_edge_id", names(links), value = TRUE)
    id_col <- if (!is.null(group_key) && length(user_cols)) {
        sort(user_cols)[length(user_cols)]
    } else {
        "edge_id"
    }
    # an unnumbered caller on a `by =` plot has no safe key: the panels
    # renumber their links, so their `edge_id` is 1 in every ring and the
    # caller's 1..N matches nothing. Say plainly what to add rather than let
    # the generic "not in the layout" fire two branches down (the message
    # pointed at the user's data, which was right).
    if (!is.null(group_key) && !length(user_cols)) {
        ggcircos_abort(
            "data",
            c(
                "On a {.arg by} plot, {.field {slot}} needs your own link numbering: \
                 every panel renumbers its links, so there is no unambiguous id to match.",
                i = "Add an {.field edge_id} column to the links (e.g. \
                     {.code links$edge_id <- seq_len(nrow(links))}) and use it here."
            )
        )
    }
    lk_key <- if (!is.null(group_key)) {
        paste(links[[group_key]], links[[id_col]], sep = "\r")
    } else {
        as.character(links[[id_col]])
    }
    my_key <- if (!is.null(group_key)) {
        paste(data[[group_key]], eid, sep = "\r")
    } else {
        as.character(eid)
    }
    hit <- match(my_key, lk_key)
    if (anyNA(hit)) {
        ggcircos_abort(
            "data",
            c(
                "Some track rows name a link that is not in the layout.",
                i = "{.field {slot}} must be the {.field edge_id} of a link the plot carries."
            )
        )
    }
    # Which end of the link the row sits on. A link reserves a slot on *each*
    # of its two nodes, and they are different arcs; taking the from end for
    # every row drew the target-side rectangles on the wrong side of the ring
    # (the reproduction figure made this obvious). The row's own node id says
    # which end it is: match it against the two endpoints. A self-loop has the
    # same node on both ends, so the from slot is used, which is its foot.
    row_node <- if (!is.null(node_col) && node_col %in% names(data))
        as.character(data[[node_col]]) else NA_character_
    on_to <- !is.na(row_node) & row_node == as.character(links$to[hit]) &
        as.character(links$from[hit]) != as.character(links$to[hit])
    theta0 <- ifelse(on_to, links$theta_end0[hit], links$theta0[hit])
    theta1 <- ifelse(on_to, links$theta_end1[hit], links$theta1[hit])
    arc <- (theta1 - theta0) %% (2 * pi)
    theta <- wrap_2pi(theta0 + arc / 2)
    r_link <- ifelse(on_to, links$xend[hit], links$x[hit])

    out <- data
    # the row's arc IS the link's slot: theta is the centre, ymin/ymax the two
    # slot edges. Mapping them like the rect geoms expect means no geometry
    # change at all.
    out$y <- theta
    out$.circos_theta <- theta
    out$.circos_arc <- arc
    out$ymin <- theta0
    out$ymax <- theta1
    out$.circos_half <- arc / 2
    # the slot sits at the link endpoint's radius: the position leaves x alone
    # for a band_fill-like row (it fills the slot radially)
    out$.circos_slot_r <- r_link
    if (!"x" %in% names(out) && !isTRUE(out$.circos_has_value)) out$x <- 0
    out
}

#' Add y / ymin / ymax once theta and arc are known per row
#'
#' Shared by the single-layout and the B-plan paths: both end up with a `theta`
#' and an `arc` per row and a `key` saying which node-row-group a row belongs
#' to (the id in one ring, `(group, id)` across rings). `ymin`/`ymax` come from
#' `theta -/+ arc/2`, which equals `theta0` for a single node and stays correct
#' when the two rings place the same id on different arcs.
#' @keywords internal
#' @noRd
track_finish_angles <- function(data, key, theta, arc, width, aggregate, has_value) {
    if (!"x" %in% names(data) && !isTRUE(data$.circos_has_value) && !isTRUE(has_value)) {
        data$x <- 0  # placeholder; the position owns it
    }
    data$y <- theta
    data$.circos_theta <- theta
    data$.circos_arc <- arc
    # Rows that share a node split its arc evenly, in data order: a tile is a
    # heatmap, an area/line a coverage profile, a column track a brush of bars.
    # Aggregating geoms are exempt (their stat folds the rows into one mark that
    # keeps the whole arc); which they are is declared in `track_geom_spec()`.
    if (aggregate) {
        half <- arc * width / 2
        data$ymin <- theta - half
        data$ymax <- theta + half
        # `.circos_half` is a mapped column, not derived from ymin/ymax: a node
        # crossing the seam has ymin < 0, which the angle scale clamps before
        # the stat runs (B41).
        data$.circos_half <- half
    } else {
        k <- as.numeric(stats::ave(seq_along(key), key, FUN = seq_along))
        m <- as.numeric(stats::ave(seq_along(key), key, FUN = length))
        bin_w <- arc * width / m
        data$ymin <- (theta - arc / 2) + (arc - bin_w * m) / 2 + (k - 1) * bin_w
        data$ymax <- data$ymin + bin_w
    }
    data
}

#' Whether a track geometry's stat aggregates a node's rows
#'
#' Wraps `track_geom_spec()` so the two places that care (`track_angles()` and
#' the `node` mapping in `ggplot_add`) can never disagree, and an unknown name
#' is not an error here: `geom_track()` reports that properly.
#' @keywords internal
#' @noRd
track_aggregates <- function(geom) {
    if (!is.character(geom) || length(geom) != 1L) return(FALSE)
    spec <- tryCatch(track_geom_spec(geom), error = function(e) NULL)
    isTRUE(spec$aggregate)
}

#' @keywords internal
#' @noRd
track_defaults <- function(rect, aggregate = FALSE) {
    out <- list(
        x = rlang::quo(.data$x),
        y = rlang::quo(.data$.circos_theta)
    )
    if (rect) {
        out$ymin <- rlang::quo(.data$ymin)
        out$ymax <- rlang::quo(.data$ymax)
    }
    if (aggregate) {
        # the node's half arc, for the stat; see track_angles()
        out$.circos_half <- rlang::quo(.data$.circos_half)
    }
    out
}

#' Position that maps track values into a radial band
#'
#' Applies the normalisation described in [geom_track()]. ggplot2 runs a
#' position *after* the stat and the geom setup, so the layer must already
#' carry `y` -- [track_angles()] adds it.
#'
#' @param band Track name or `c(r0, r1)` radius interval. A *name* can only
#'   be resolved inside a `ggcircos()` plot (the layout declares the bands);
#'   standalone use needs the numeric form.
#' @param node Name of the node id column; only used to group stacks.
#' @param anchored Stretch from the band's inner edge (default `FALSE`).
#' @param band_fill Fill the whole band radially (`xmin = r0`, `xmax = r1`)
#'   instead of scaling a value into it. Set by [geom_track()] for `"tile"`.
#' @param stack,scales,limits,baseline,width,offset See [geom_track()].
#' @param geom_name Name of the track geometry.
#'
#' @details The position holds plain vectors only (`band`, the normalisation
#' settings, the node column name) -- never a layout or a node table, so one
#' position object can be shared by several layers without them seeing each
#' other's data (spec sec.8.2).
#'
#' @return A ggplot2 position.
#' @examples
#' # the escape hatch: reuse the normalisation on any geom
#' set.seed(1)
#' d <- track_angles(
#'     data.frame(id = "mpg", value = runif(10)),
#'     "id", circos_layout(cor(mtcars)), 0.9, "point"
#' )
#' ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
#'     ggplot2::geom_point(
#'         data = d,
#'         ggplot2::aes(x = value, y = y),
#'         position = position_track(band = c(1.06, 1.26), anchored = TRUE),
#'         inherit.aes = FALSE
#'     )
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @export
position_track <- function(band,
                           node = NULL,
                           geom_name = NULL,
                           anchored = FALSE,
                           band_fill = FALSE,
                           stack = FALSE,
                           scales = c("free", "fixed"),
                           limits = NULL,
                           baseline = NULL,
                           width = 0.9,
                           offset = 0) {
    scales <- match.arg(scales)
    node_quo <- rlang::enquo(node)
    if (!rlang::quo_is_null(node_quo)) {
        node <- column_or_string(node_quo, "node")
    }
    if (is.character(band) && length(band) == 1L) {
        band_name <- band
        band <- NULL
    } else {
        band <- as.numeric(band)
        if (length(band) != 2L || anyNA(band)) {
            ggcircos_abort(
                "band",
                "{.arg band} must be a declared track name or a numeric {.code c(r0, r1)}."
            )
        }
        if (band[1] >= band[2]) {
            ggcircos_abort("band", "{.arg band} must be increasing, got {.val {band}}.")
        }
        band_name <- NULL
    }
    ggproto(NULL, PositionTrack,
        aesthetics = function() character(0),
        band_name = band_name,
        band = band,
        stack = stack,
        scales = scales,
        limits = limits,
        baseline = baseline,
        width = width,
        offset = offset,
        anchored = anchored,
        band_fill = band_fill,
        node = node
    )
}

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @export
PositionTrack <- ggproto("PositionTrack", Position,
    compute_panel = function(self, data, params, scales) {
        if (nrow(data) == 0L) return(data)
        # ggplot2 hands `setup_params()` the layer data, so `params` is not a
        # parameter bag: read the settings from the position itself. A track
        # *name* is resolved to a numeric band by `ggplot_add` (the only
        # place the layout is reachable); a position that still carries a
        # name here was built outside a ggcircos plot.
        if (is.null(self$band)) {
            ggcircos_abort(
                "band",
                "{.fn position_track} needs a numeric {.code band = c(r0, r1)} when used outside {.fn geom_track}.",
                i = "A track name is resolved from the plot's layout; pass {.code c(r0, r1)} directly here."
            )
        }
        r0 <- self$band[1] + (self$offset %||% 0)
        r1 <- self$band[2]
        # Which node a row belongs to. The node *column* is the exact answer and
        # wins whenever it survived to here. It is there under the caller's own
        # name for plain layers, and under the track's reserved aesthetic name
        # `node` for aggregating stats (which emit ids, not rows). Otherwise the
        # row is looked up by its angle, from the snapshot `ggplot_add()` took
        # for the layers that never map `node` (spec sec.8.2: vectors only).
        #
        # Looking up by angle alone is wrong once a stat has rewritten `y`: a
        # violin outline sits at theta +/- density, never at the node centre, so
        # `match()` returned NA for every row and every violin kept raw values
        # as radii -- values in the 0.2-0.8 range drawn outside the band (B41).
        node_col <- if (!is.null(self$node) && self$node %in% names(data)) {
            self$node
        } else if ("node" %in% names(data)) {
            "node"
        } else {
            NULL
        }
        node_values <- if (!is.null(node_col)) {
            as.character(data[[node_col]])
        } else if (!is.null(self$node_angles) && "y" %in% names(data)) {
            self$node_values[match(data$y, self$node_angles)]
        } else if (length(self$node_values) == nrow(data)) {
            self$node_values
        } else {
            rep("all", nrow(data))
        }
        # `geom_track(value = )` stashes the numbers on the position itself:
        # ggplot2 hands the position only the mapped aesthetics, and a data
        # column named `x` (or GeomRect's own xmin/xmax derivation) can have
        # replaced `x` with a constant by then (review R4).
        if (!is.null(self$values) && length(self$values) == nrow(data)) {
            data$x <- self$values
        }
        track_map_values(
            data, r0, r1, self,
            anchored = self$anchored, group_values = node_values
        )
    }
)

#' Resolve a track band
#' @keywords internal
#' @noRd
track_band <- function(name, fallback, node_table) {
    if (!is.null(name)) {
        tracks <- attr(node_table, "tracks")
        if (is.null(tracks)) {
            ggcircos_abort(
                "band",
                "Track {.val {name}} cannot be resolved: this layer has no layout.",
                i = "Declare it with {.code ggcircos(tracks = ...)} or pass {.code band = c(r0, r1)}."
            )
        }
        hit <- match(name, tracks$track)
        if (is.na(hit)) {
            ggcircos_abort(
                "band",
                "Unknown track {.val {name}}. Declared tracks: {.field {tracks$track}}.",
                i = "Declare it in {.code ggcircos(tracks = ...)} or pass {.code band = c(r0, r1)}."
            )
        }
        return(c(tracks$r0[hit], tracks$r1[hit]))
    }
    if (is.null(fallback) || !is.numeric(fallback) || length(fallback) != 2L) {
        ggcircos_abort("band", "{.arg band} must be a track name or {.code c(r0, r1)}.")
    }
    fallback
}

#' Apply the track normalisation to whichever value columns are present
#' @keywords internal
#' @noRd
track_map_values <- function(data, r0, r1, self,
                             anchored = FALSE, group_values = NULL) {
    if (isTRUE(self$band_fill)) {
        # a tile track fills the band radially: every row spans [r0, r1] and
        # its angular extent is the bin split from track_angles(). The x
        # column is a placeholder, not a value to scale.
        data$xmin <- r0
        data$xmax <- r1
        data$x <- (r0 + r1) / 2
        return(data)
    }
    if (!"x" %in% names(data)) {
        mid <- (r0 + r1) / 2
        data$x <- mid
        data$xmin <- mid
        data$xmax <- mid
        return(data)
    }
    if (isTRUE(self$stack)) {
        if (any(data$x < 0, na.rm = TRUE)) {
            ggcircos_abort(
                "data",
                "{.code stack = TRUE} needs non-negative values, found {min(data$x, na.rm = TRUE)}."
            )
        }
        return(track_stack(data, group_values, r0, r1))
    }
    span_x <- setdiff(
        intersect(c("x", "xmin", "xmax", "xmiddle"), names(data)),
        "x"
    )
    if (identical(self$scales, "fixed")) {
        lim <- self$limits
        if (is.null(lim) || length(lim) != 2L || anyNA(lim)) {
            ggcircos_abort("data", "{.arg limits} must be a numeric length-2 vector.")
        }
        base <- lim[1]; top <- lim[2]
        for (nm in c("x", span_x)) {
            data[[nm]] <- rescale_from(data[[nm]], base, top, r0, r1)
        }
    } else {
        # "free" is *per node*: each node's values are normalised against
        # that node's own range, so a node with 1 and a node with 100 both
        # fill their band. Normalising against the whole column instead made
        # "free" mean "the range comes from the data", which is what "fixed"
        # with a data-derived limit would mean, and every node but the
        # largest collapsed into a sliver (review R3).
        base <- self$baseline
        key <- as.character(group_values %||% rep(1L, nrow(data)))
        cols <- c("x", span_x)
        for (k in unique(key)) {
            i <- which(key == k)
            vals <- unlist(data[i, cols, drop = FALSE], use.names = FALSE)
            vals <- vals[is.finite(vals)]
            if (!length(vals)) next
            b <- if (!is.null(base)) {
                base
            } else if (all(vals >= 0)) {
                0
            } else {
                min(vals)
            }
            top <- max(vals)
            for (nm in cols) {
                data[[nm]][i] <- rescale_from(data[[nm]][i], b, top, r0, r1)
            }
        }
    }
    if (anchored) {
        data$xmin <- r0
        data$xmax <- data$x
        data$xmiddle <- data$x
    }
    data
}

#' @keywords internal
#' @noRd
track_stack <- function(data, group_values, r0, r1) {
    key <- as.character(group_values)
    if ("group" %in% names(data)) key <- paste0(key, "|", data$group)
    v <- data$x
    lo <- numeric(nrow(data)); hi <- numeric(nrow(data))
    for (k in unique(key)) {
        i <- which(key == k)
        tot <- sum(v[i], na.rm = TRUE)
        if (!is.finite(tot) || tot <= 0) {
            lo[i] <- r0; hi[i] <- r0
            next
        }
        cs <- cumsum(v[i]) / tot
        lo[i] <- r0 + (r1 - r0) * c(0, cs[-length(cs)])
        hi[i] <- r0 + (r1 - r0) * cs
    }
    data$x <- hi
    data$xmin <- lo
    data$xmax <- hi
    data$xmiddle <- hi
    data
}

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomRect
#' @export
GeomTrackBar <- ggproto("GeomTrackBar", GeomRect,
    default_aes = ggplot2::aes(
        fill = "grey35", colour = NA, linewidth = 0.1,
        linetype = 1, alpha = NA
    ),
    required_aes = c("x", "y", "ymin", "ymax"),
    # `xmin`/`xmax` are normally written by the position, but an interval row
    # brings its own arc bounds (spec sec.4.11), so they must not be reported
    # as unknown aesthetics.
    optional_aes = c("xmin", "xmax"),
    # xmin/xmax arrive from the position, which ggplot2 runs after setup
    setup_data = function(data, params) data
)

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomRect
#' @export
GeomTrackArea <- ggproto("GeomTrackArea", GeomRect,
    default_aes = ggplot2::aes(
        fill = "grey35", colour = NA, linewidth = 0.1,
        linetype = 1, alpha = NA
    ),
    required_aes = c("x", "y", "ymin", "ymax"),
    setup_data = function(data, params) data
)

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomCrossbar
#' @export
GeomTrackCrossbar <- ggproto("GeomTrackCrossbar", GeomCrossbar,
    required_aes = c("x", "y", "ymin", "ymax")
)

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomPoint
#' @export
GeomTrackPoint <- ggproto("GeomTrackPoint", GeomPoint,
    required_aes = "y",
    optional_aes = c("x", "size", "shape", "colour", "alpha", "stroke")
)

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomTile
#' @export
GeomTrackTile <- ggproto("GeomTrackTile", GeomTile,
    required_aes = c("y", "ymin", "ymax"),
    optional_aes = c("x", "xmin", "xmax", "fill", "colour", "linewidth",
                     "linetype", "alpha"),
    # GeomTile$setup_data derives xmin/xmax/ymin/ymax from the width/height
    # *aesthetics*, whose defaults are one whole radian: that would swallow
    # the per-node bin split track_angles() computed, which is exactly the
    # B7 bug. The band's radial extent and the bins' angular extent both
    # arrive as columns, so pass the data through untouched.
    setup_data = function(data, params) data
)

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomText
#' @export
GeomTrackText <- ggproto("GeomTrackText", GeomText,
    required_aes = c("y", "label"),
    optional_aes = c("x", "size", "angle", "hjust", "vjust", "colour", "alpha", "family", "fontface", "lineheight")
)

#' Boxplot quantiles per node, as a track stat
#'
#' `stat_boxplot()` needs a discrete y axis to group by, which cannot work
#' when `y` is an angle in radians. This stat groups by the node column and
#' hands plain `xmin`/`xmax` geometry to the position.
#'
#' @keywords internal
#' @noRd
StatTrackBox <- ggproto("StatTrackBox", Stat,
    required_aes = "x",
    optional_aes = c("node", "group"),
    default_aes = ggplot2::aes(y = after_stat(y)),
    setup_params = function(data, params) {
        params$coef <- params$coef %||% 1.5
        params$na.rm <- params$na.rm %||% FALSE
        params
    },
    extra_params = c("na.rm", "coef"),
    compute_group = function(data, scales, coef = 1.5, na.rm = FALSE) {
        vals <- data$x[stats::complete.cases(data$x)]
        if (!length(vals)) {
            return(data.frame(
                xmin = NA_real_, x = NA_real_, xmax = NA_real_,
                y = data$y[1], ymin = data$ymin[1], ymax = data$ymax[1],
                stringsAsFactors = FALSE
            ))
        }
        iqr <- stats::IQR(vals)
        lo <- min(vals); hi <- max(vals)
        data.frame(
            xmin = max(vals[vals >= lo - coef * iqr]),
            x = stats::median(vals),
            xmax = min(vals[vals <= hi + coef * iqr]),
            y = data$y[1],
            ymin = data$ymin[1],
            ymax = data$ymax[1],
            stringsAsFactors = FALSE
        )
    },
    compute_panel = function(self, data, scales, coef = 1.5, na.rm = FALSE) {
        key <- if ("node" %in% names(data)) {
            as.character(data$node)
        } else if ("group" %in% names(data)) {
            as.character(data$group)
        } else {
            ggcircos_abort("data", "Boxplot tracks need a {.arg node} column to group by.")
        }
        idx <- split(seq_len(nrow(data)), key)
        out <- do.call(rbind, lapply(idx, function(i) {
            self$compute_group(data[i, , drop = FALSE], scales,
                coef = coef, na.rm = na.rm
            )
        }))
        out$node <- names(idx)
        out
    }
)