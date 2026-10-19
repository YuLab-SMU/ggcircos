#' Radial axes for circular tracks
#'
#' Draws the two edges of a track band as rings, a radial tick at every node
#' boundary for each break, and the break labels outside the band. The scale
#' is always explicit: `limits` is required, so the axis can never disagree
#' with how the track layer scaled its data.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,na.rm,...
#'   Standard [ggplot2::layer()] arguments.
#' @param band Track name or `c(r0, r1)` radius interval.
#' @param limits Value range of the track; required.
#' @param breaks Break positions. `NULL` uses `pretty(limits, 4)`.
#' @param labels Break labels; `waiver()` formats them with the default
#'   formatter.
#' @param text.size Size of the break labels.
#' @param line.size Width of the rings and ticks.
#' @param tick.length Extra radius reserved for the labels.
#'
#' @return A ggplot2 layer.
#' @examples
#' set.seed(1)
#' vals <- data.frame(
#'     id = rep(colnames(mtcars), each = 4),
#'     value = runif(ncol(mtcars) * 4),
#'     stringsAsFactors = FALSE
#' )
#' ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
#'     geom_track(data = vals, aes(x = value), node = id, band = "values") +
#'     geom_track_axis(band = "values", limits = c(0, 1))
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @export
geom_track_axis <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            band,
                            limits,
                            breaks = NULL,
                            labels = ggplot2::waiver(),
                            text.size = 3,
                            line.size = 0.2,
                            tick.length = 0.02,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = FALSE) {
    if (missing(band)) {
        ggcircos_abort("band", "{.fn geom_track_axis} needs a {.arg band}.")
    }
    if (missing(limits) || length(limits) != 2L || anyNA(limits)) {
        ggcircos_abort(
            "data",
            "{.fn geom_track_axis} needs {.arg limits} -- the axis never guesses the track's range.",
            i = "Use the same limits you passed to {.fn geom_track}, e.g. {.code limits = c(0, 1)}."
        )
    }
    if (is.null(breaks)) breaks <- pretty_within(limits, 4)
    band_name <- if (is.character(band) && length(band) == 1L) band else NULL
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomTrackAxis,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                text.size = text.size, line.size = line.size,
                na.rm = na.rm, ...
            )
        ),
        "ggcircos_axis",
        list(band = band, band_name = band_name, limits = limits,
             breaks = breaks, labels = labels, tick.length = tick.length)
    )
}

#' Geometry of a track axis, built once when the layer is added
#'
#' Produces the rings, the radial ticks (one per node boundary) and the
#' labels, all as rows tagged with `kind`.
#' @keywords internal
#' @noRd
track_axis_data <- function(band, limits, breaks, labels, node_table, tick_length) {
    br <- rescale_from(breaks, limits[1], limits[2], band[1], band[2])
    theta0 <- if (is.null(node_table) || !nrow(node_table)) {
        0
    } else {
        node_table$theta0
    }
    empty <- function(n) {
        data.frame(
            x = numeric(n), y = numeric(n), label = character(n),
            group = character(n), kind = character(n),
            stringsAsFactors = FALSE
        )
    }
    ring <- function(r, g) {
        d <- empty(181)
        d$x <- r
        d$y <- seq(0, 2 * pi, length.out = 181)
        d$group <- g
        d$kind <- "ring"
        d
    }
    ticks <- if (length(br) && length(theta0)) {
        do.call(rbind, lapply(seq_along(br), function(i) {
            d <- empty(2 * length(theta0))
            d$x <- rep(c(band[1], band[2]), each = length(theta0))
            d$y <- rep(theta0, 2)
            d$group <- rep(paste0("b", i), each = length(theta0))
            d$kind <- "tick"
            d
        }))
    } else {
        empty(0)
    }
    lab <- if (is_waive(labels) || is.null(labels)) {
        format(scales::squish(breaks))
    } else {
        as.character(labels)
    }
    text <- empty(length(br))
    text$x <- band[2] + tick_length
    text$y <- if (length(theta0)) theta0[1] else 0
    text$label <- rep_len(lab, length(br))
    text$group <- "lab"
    text$kind <- "label"

    out <- rbind(ring(band[1], "r0"), ring(band[2], "r1"), ticks, text)
    out$label[is.na(out$label)] <- ""
    rownames(out) <- NULL
    out
}

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 GeomText
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid gTree gpar pathGrob
#' @export
GeomTrackAxis <- ggproto("GeomTrackAxis", Geom,
    default_aes = ggplot2::aes(
        x = NA_real_, y = NA_real_, label = "",
        colour = "grey30", linewidth = 0.2, linetype = 1, alpha = NA,
        size = 3.88, hjust = 0.5, vjust = 0.5, angle = 0,
        family = "", fontface = 1, lineheight = 1.2
    ),
    required_aes = c("x", "y"),
    optional_aes = c("label", "kind", "group"),
    draw_key = draw_key_blank,
    draw_panel = function(self, data, panel_params, coord,
                          line.size = 0.2, text.size = 3, na.rm = FALSE) {
        if (nrow(data) == 0L) return(grid::nullGrob())
        kind <- data$kind %||% rep("ring", nrow(data))
        lines <- data[kind != "label", , drop = FALSE]
        texts <- data[kind == "label", , drop = FALSE]

        line_grob <- if (nrow(lines) > 0) {
            pts <- coord$transform(lines, panel_params)
            pathGrob(
                pts$x, pts$y, id = pts$group,
                default.units = "native",
                gp = gpar(
                    col = lines$colour[1], lwd = line.size * ggcircos_pt,
                    lty = lines$linetype[1], lineend = "butt"
                )
            )
        } else {
            grid::nullGrob()
        }
        text_grob <- if (nrow(texts) > 0) {
            ang <- circos_label_angle(coord, panel_params, texts$y, TRUE)
            texts$angle <- ang$angle
            texts$hjust <- ang$hjust
            texts$size <- text.size
            ggplot2::GeomText$draw_panel(texts, panel_params, coord)
        } else {
            grid::nullGrob()
        }
        ggname("geom_track_axis", gTree(children = grid::gList(line_grob, text_grob)))
    }
)