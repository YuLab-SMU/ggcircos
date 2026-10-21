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
#'   band's inner edge), `"area"`, `"boxplot"`, `"point"`, `"tile"` or
#'   `"text"`. The last three sit at the band's centre when no `x` is mapped;
#'   with an `x` they land at the scaled value, the way a bar's outer edge does.
#' @param node Unquoted name of the column holding node ids. Required.
#' @param value Unquoted name of the column holding the values to plot, e.g.
#'   `value = gc`. Optional (`aes(x = gc)` also works), but it is the robust
#'   spelling: `value` is handed to the position directly, so it keeps working
#'   when the data has a column literally called `x` and it makes `stack`
#'   able to reject negative input instead of silently drawing a flat track.
#' @param band Either a declared track name (see the `tracks` argument of
#'   [ggcircos()]) or an explicit `c(r0, r1)` radius interval.
#' @param stack Stack values within each node instead of scaling them
#'   independently. Values must then be non-negative.
#' @param scales `"free"` normalises each node against its own range;
#'   `"fixed"` uses `limits` for every node.
#' @param limits Value range of the track; required when `scales = "fixed"`.
#' @param baseline Lower bound when `scales = "free"`. By default `0` if all
#'   values are non-negative, else the smallest value.
#' @param width Fraction of a node's arc covered by the layer.
#' @param offset Distance from the band's inner edge. Only meaningful when
#'   `band` is an explicit interval.
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
#' stretch from `r0` to the mapped value.
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
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = FALSE) {
    # A position built by `position_track()` already carries its band, so
    # repeating it on the layer is redundant. Only a layer with neither has
    # nothing to draw into.
    if (missing(band) && (is.null(position) || is.null(position$band))) {
        ggcircos_abort(
            "band",
            "{.fn geom_track} needs a {.arg band}: a declared track name or an explicit {.code c(r0, r1)}."
        )
    }
    if (missing(band)) band <- NULL
    spec <- track_geom_spec(geom)
    scales <- match.arg(scales)
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
             baseline = position$baseline)
    )
}

#' Geometry whitelist and how each entry is assembled
#'
#' `anchored` geoms stretch from the band's inner edge to the mapped value;
#' the others sit at the band's centre when no value is mapped. `rect` marks
#' the entries that need `ymin`/`ymax`. `band` marks the entries that fill
#' the whole radial band (`tile`): their `xmin`/`xmax` are the band edges,
#' not a mapped value, and their angular extent comes from the bin split in
#' [track_angles()].
#' @keywords internal
#' @noRd
track_geom_spec <- function(geom) {
    specs <- list(
        col = list(stat = "identity", geom = GeomTrackBar, anchored = TRUE, rect = TRUE, band = FALSE),
        bar = list(stat = "identity", geom = GeomTrackBar, anchored = TRUE, rect = TRUE, band = FALSE),
        area = list(stat = "identity", geom = GeomTrackArea, anchored = TRUE, rect = TRUE, band = FALSE),
        boxplot = list(stat = StatTrackBox, geom = GeomTrackCrossbar, anchored = FALSE, rect = TRUE, band = FALSE),
        point = list(stat = "identity", geom = GeomTrackPoint, anchored = FALSE, rect = FALSE, band = FALSE),
        tile = list(stat = "identity", geom = GeomTrackTile, anchored = FALSE, rect = TRUE, band = TRUE),
        text = list(stat = "identity", geom = GeomTrackText, anchored = FALSE, rect = FALSE, band = FALSE)
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
#' @return `data` with the angle columns added.
#' @examples
#' lay <- circos_layout(cor(mtcars))
#' head(track_angles(
#'     data.frame(id = colnames(mtcars)), "id", lay, 0.9, "col"
#' ))
#' @export
track_angles <- function(data, node, layout, width = 0.9, geom = "col", has_value = FALSE, ...) {
    node_col <- column_or_string(rlang::enquo(node), "node")
    nt <- if (inherits(layout, "circos_layout")) layout$nodes else layout
    if (is.null(nt)) {
        ggcircos_abort("layout", "{.arg layout} must be a {.cls circos_layout} or a node table.")
    }
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    if (!node_col %in% names(data)) {
        ggcircos_abort("data", "The data has no node column {.field {node_col}}.")
    }
    ids <- as.character(data[[node_col]])
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
    data$.circos_theta <- theta
    data$.circos_arc <- arc
    # A placeholder only when the caller did not supply the value column.
    # A data column named `x` overrides the layer's mapping and turns every
    # value into 0 -- the bug that hid the negative-input check (review R4).
    # `geom_track(value = )` marks the layer with `.circos_has_value`, which
    # survives to here because `data` is the layer's own frame.
    if (!"x" %in% names(data) && !isTRUE(data$.circos_has_value) && !isTRUE(has_value)) {
        data$x <- 0  # placeholder; the position owns it
    }
    data$y <- theta
    # Rows that share a node split its arc evenly, in data order. This is a
    # *sequence along the arc* for every geometry that draws one mark per row:
    # a tile track is a heatmap, an area/line track is a coverage profile, and
    # a column track with several measurements per node is a brush of bars.
    # Only `boxplot` is exempt: it aggregates the rows down to one mark per
    # node, so that mark spans the whole arc.
    #
    # With one row per node the bin is the whole `width` share of the arc, so
    # the split is bit-identical to centring on `theta` and nothing changes for
    # the common single-value track.
    #
    # The angular extent goes through ymin/ymax, not the `width`/`height`
    # aesthetics: those are aesthetics, and a data column with an aesthetic's
    # name is overwritten by the layer's default_aes.
    if (identical(geom, "boxplot")) {
        half <- arc * width / 2
        data$ymin <- theta - half
        data$ymax <- theta + half
    } else {
        k <- as.numeric(stats::ave(seq_along(idx), idx, FUN = seq_along))
        m <- as.numeric(stats::ave(seq_along(idx), idx, FUN = length))
        bin_w <- arc * width / m
        data$ymin <- nt$theta0[idx] + (arc - bin_w * m) / 2 + (k - 1) * bin_w
        data$ymax <- data$ymin + bin_w
    }
    data
}

#' @keywords internal
#' @noRd
track_defaults <- function(rect) {
    out <- list(
        x = rlang::quo(.data$x),
        y = rlang::quo(.data$.circos_theta)
    )
    if (rect) {
        out$ymin <- rlang::quo(.data$ymin)
        out$ymax <- rlang::quo(.data$ymax)
    }
    out
}

#' Position that maps track values into a radial band
#'
#' Applies the normalisation described in [geom_track()]. ggplot2 runs a
#' position *after* the stat and the geom setup, so the layer must already
#' carry `y` — [track_angles()] adds it.
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
#' settings, the node column name) — never a layout or a node table, so one
#' position object can be shared by several layers without them seeing each
#' other's data (spec §8.2).
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
        node_values <- if (!is.null(self$node_angles) && "y" %in% names(data)) {
            self$node_values[match(data$y, self$node_angles)]
        } else if (length(self$node_values) == nrow(data)) {
            self$node_values
        } else if (!is.null(self$node) && self$node %in% names(data)) {
            as.character(data[[self$node]])
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
    optional_aes = c("x", "fill", "colour", "linewidth", "linetype", "alpha"),
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