#' Read-only layout plumbing for ggcircos layers
#'
#' Every layer that needs the layout gets it here. These methods may only
#' read `plot` and write into their *own* layer: nothing accumulates, no
#' layer depends on the order in which layers were added, and `plot$data`,
#' `plot$mapping`, `plot$scales` and `plot$theme` are never touched.
#'
#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @noRd
require_layout <- function(plot, what = "layer") {
    lay <- circos_layout_of(plot)
    if (is.null(lay)) {
        ggcircos_abort(
            "layout",
            "This {what} needs a ggcircos layout, but the plot has none.",
            i = "Start the plot with {.fn ggcircos}."
        )
    }
    lay
}

#' @keywords internal
#' @noRd
layout_node_table <- function(layout) {
    nd <- layout$nodes
    attr(nd, "tracks") <- layout$tracks
    nd
}

#' Params that describe geometry, not aesthetics, and must not reach ggplot2
#'
#' `rotate` and `nudge_x` are deliberately *not* in this list: they belong to
#' the label geoms, which need them in `draw_panel()` to ask the coordinate
#' for the screen angle. Everything else here is resolved in `ggplot_add()`
#' and would only confuse a geom that receives it.
#' @keywords internal
#' @noRd
geometry_params <- c(
    "from", "to", "track_geom", "anchored", "node", "band", "band_name",
    "limits", "breaks", "labels", "tick.length",
    "width", "offset", "start", "extent"
)

#' @keywords internal
#' @noRd
drop_geometry_params <- function(object) {
    if (is.null(object$geom_params)) return(object)
    keep <- !names(object$geom_params) %in% geometry_params
    object$geom_params <- object$geom_params[keep]
    object
}

#' Node and sector layers: fill in the data and the default mapping
#'
#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @noRd
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggcircos_node <- function(object, plot, object_name, ...) {
    lay <- require_layout(plot, object_name)
    params <- layer_settings(object)
    offset <- params$offset %||% 0.01

    no_data <- is.null(object$data) || is_waive(object$data)
    if (inherits(object$geom, "GeomSectorLabel")) {
        if (no_data) {
            object$data <- lay$sectors
            object$mapping <- merge_mapping(object$mapping, sector_defaults(offset))
            object$inherit.aes <- FALSE
        }
        # the rotation is a coordinate question, so `draw_panel` has to ask
        # it; the setting therefore travels in `geom_params`, not in the data
        angle <- params$angle %||% "horizontal"
        if (is.numeric(angle)) {
            object$geom_params$angle <- angle
        } else {
            object$geom_params$rotate <- identical(angle, "radial")
        }
    } else if (no_data) {
        object$data <- lay$nodes
        object$inherit.aes <- FALSE
        object$mapping <- merge_mapping(
            object$mapping, node_defaults(object$geom, params, offset)
        )
        if (inherits(object$geom, "GeomNodeText")) {
            # as above: `draw_panel` asks the coordinate for the screen angle
            object$geom_params$rotate <- isTRUE(params$rotate)
            if (!identical(params$nudge_x %||% 0, 0)) {
                object$geom_params$nudge_x <- params$nudge_x
            }
        }
    }
    re_add(drop_geometry_params(object), plot, object_name)
}

#' @keywords internal
#' @noRd
node_defaults <- function(geom, params, offset) {
    if (inherits(geom, "GeomNodeArc")) {
        width <- params$width %||% 0.04
        return(list(
            x = rlang::quo(.data$r + !!offset + !!width / 2),
            y = rlang::quo(.data$theta),
            xmin = rlang::quo(.data$r + !!offset),
            xmax = rlang::quo(.data$r + !!offset + !!width),
            ymin = rlang::quo(.data$theta0),
            ymax = rlang::quo(.data$theta1)
        ))
    }
    list(
        x = rlang::quo(.data$r + !!offset),
        y = rlang::quo(.data$theta),
        label = rlang::quo(.data$label)
    )
}

#' @keywords internal
#' @noRd
sector_defaults <- function(offset) {
    list(
        x = rlang::quo(.data$r + !!offset),
        y = rlang::quo(.data$theta),
        label = rlang::quo(.data$sector)
    )
}

#' Add default aesthetics without overwriting what the user mapped
#' @keywords internal
#' @noRd
merge_mapping <- function(mapping, defaults) {
    if (is.null(mapping)) {
        mapping <- ggplot2::aes()
    }
    for (nm in names(defaults)) {
        if (!nm %in% names(mapping)) {
            mapping[nm] <- list(defaults[[nm]])
        }
    }
    mapping
}

#' Chord layers: resolve `from`/`to` against the layout's slots
#'
#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @noRd
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggcircos_chord <- function(object, plot, object_name, ...) {
    lay <- require_layout(plot, object_name)
    params <- layer_settings(object)
    from <- chord_endpoint(object, params, "from")
    to <- chord_endpoint(object, params, "to")

    if (!is.null(from) || !is.null(to)) {
        if (is.null(from) || is.null(to)) {
            ggcircos_abort(
                "data",
                "{.fn geom_chord} needs both {.arg from} and {.arg to} to resolve link slots."
            )
        }
        if (is.null(object$data) || is_waive(object$data)) {
            ggcircos_abort(
                "data",
                "{.arg from}/{.arg to} need a {.arg data} table to read the columns from."
            )
        }
        dat <- as.data.frame(object$data, stringsAsFactors = FALSE)
        slots <- layout_slots(
            lay,
            as.character(eval_endpoint(from, dat)),
            as.character(eval_endpoint(to, dat))
        )
        for (nm in names(slots)) dat[[nm]] <- slots[[nm]]
        dat$y <- slots$theta
        dat$yend <- slots$theta_end
        object$data <- dat
        object$inherit.aes <- FALSE
        # from/to have served their purpose; keeping them would ask ggplot2
        # for scales it cannot build
        object$mapping <- object$mapping[setdiff(names(object$mapping), c("from", "to"))]
    }
    object$mapping <- merge_mapping(object$mapping, chord_coordinates())
    re_add(drop_geometry_params(object), plot, object_name)
}

#' The coordinates a chord layer always needs, mapped from their own columns
#'
#' Mapping them (rather than listing them in `default_aes`) is what lets the
#' layout's slot values survive `use_defaults()`.
#' @keywords internal
#' @noRd
chord_coordinates <- function() {
    cols <- c(
        "x", "y", "xend", "yend",
        "theta0", "theta1", "theta_end0", "theta_end1"
    )
    stats::setNames(lapply(cols, function(nm) rlang::quo(.data[[!!nm]])), cols)
}

#' @keywords internal
#' @noRd
chord_endpoint <- function(object, params, name) {
    quo <- params[[name]]
    if (rlang::is_quosure(quo) && !rlang::quo_is_null(quo)) {
        return(column_name(quo, name))
    }
    mapping <- object$mapping
    if (is.null(mapping) || !name %in% names(mapping)) return(NULL)
    expr <- rlang::quo_get_expr(mapping[[name]])
    if (rlang::is_symbol(expr)) rlang::as_name(expr) else NULL
}

#' @keywords internal
#' @noRd
eval_endpoint <- function(what, data) {
    if (is.character(what)) {
        if (!what %in% names(data)) {
            ggcircos_abort("data", "The layer data has no column {.field {what}}.")
        }
        return(data[[what]])
    }
    if (rlang::is_quosure(what)) return(rlang::eval_tidy(what, data = data))
    what
}

#' Find the slots a layout reserved for a set of endpoint pairs
#'
#' Pairs may be given in either orientation: asking for `("b", "a")` returns
#' the slots of the layout's `a -> b` row with the two ends swapped.
#'
#' @param lay A `circos_layout`.
#' @param from,to Character vectors of node ids of the same length.
#' @return A data frame with the slot and coordinate columns.
#' @keywords internal
#' @noRd
layout_slots <- function(lay, from, to) {
    if (length(from) != length(to)) {
        ggcircos_abort("data", "{.arg from} and {.arg to} must have the same length.")
    }
    lk <- lay$links
    fwd <- paste(lk$from, lk$to, sep = "\r")
    rev <- paste(lk$to, lk$from, sep = "\r")
    key <- paste(from, to, sep = "\r")
    i_f <- match(key, fwd)
    i_r <- match(key, rev)
    swap <- is.na(i_f) & !is.na(i_r)
    hit <- ifelse(is.na(i_f), i_r, i_f)
    if (anyNA(hit)) {
        bad <- unique(paste(from[is.na(hit)], to[is.na(hit)], sep = "-"))
        ggcircos_abort(
            "data",
            "Link{?s} {.val {head(bad, 3)}} {?is/are} not in the layout.",
            i = "Pass all links to {.fn ggcircos}: a chord layer can only subset them."
        )
    }
    pick <- function(col_f, col_r) {
        out <- lk[[col_f]][hit]
        out[swap] <- lk[[col_r]][hit[swap]]
        out
    }
    data.frame(
        theta0 = pick("theta0", "theta_end0"),
        theta1 = pick("theta1", "theta_end1"),
        theta = pick("theta", "theta_end"),
        theta_end0 = pick("theta_end0", "theta0"),
        theta_end1 = pick("theta_end1", "theta1"),
        theta_end = pick("theta_end", "theta"),
        x = lay$nodes$r[match(from, lay$nodes$id)],
        xend = lay$nodes$r[match(to, lay$nodes$id)],
        stringsAsFactors = FALSE
    )
}

#' Track layers: hand the node table to the position
#'
#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @noRd
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggcircos_track <- function(object, plot, object_name, ...) {
    lay <- require_layout(plot, object_name)
    params <- layer_settings(object)
    band <- track_band(params$band_name, params$band, layout_node_table(lay))
    object$data <- track_angles(
        data = object$data,
        node = params$node,
        layout = lay,
        width = params$width %||% 0.9,
        geom = params$geom_name,
        band_width = band[2] - band[1]
    )
    spec <- track_geom_spec(params$geom_name)
    object$mapping <- merge_mapping(
        object$mapping, track_defaults(spec$rect)
    )
    # the position keeps only the resolved numeric band (a plain vector); the
    # layout itself never travels into it (spec §8.2). A position that came
    # in with a numeric band already carries it, so it is not touched — a
    # shared position object must not be rewritten by one layer (spec §6.4).
    if (is.null(object$position$band)) {
        object$position$band <- band
    }
    re_add(drop_geometry_params(object), plot, object_name)
}

#' Axis layers: build the geometry once, here
#'
#' @importFrom ggplot2 ggplot_add
#' @keywords internal
#' @noRd
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggcircos_axis <- function(object, plot, object_name, ...) {
    lay <- require_layout(plot, object_name)
    params <- layer_settings(object)
    band <- track_band(params$band_name, params$band, layout_node_table(lay))
    object$data <- track_axis_data(
        band = band,
        limits = params$limits,
        breaks = params$breaks,
        labels = params$labels,
        node_table = lay$nodes,
        tick_length = params$tick.length %||% 0.02
    )
    object$inherit.aes <- FALSE
    object$mapping <- ggplot2::aes(
        x = .data$x, y = .data$y, label = .data$label
    )
    re_add(drop_geometry_params(object), plot, object_name)
}