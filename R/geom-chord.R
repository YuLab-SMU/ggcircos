#' Chords between nodes
#'
#' A chord leaves each of its two endpoint arcs at the slot the layout
#' reserved for it, so links read as a banded ring rather than a star burst
#' from node centres. Two shapes are available: `type = "ribbon"` fills the
#' area between the two curves of a link, `type = "line"` draws a single
#' curve from one slot to the other.
#'
#' The default `data` is the layout's link table, in which the coordinates are
#' already computed. To style a *subset* of the links, pass data plus the
#' two endpoint columns — either as arguments (`from = , to = `) or through
#' the mapping (`aes(from = , to = )`).
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,na.rm,...
#'   Standard [ggplot2::layer()] arguments.
#' @param from,to Unquoted column names of the link endpoints. Required when
#'   `data` is supplied; otherwise the layout's link table is used as is.
#' @param type `"ribbon"` (filled area) or `"line"` (single curve).
#' @param curvature `"auto"` derives the bulge from how far apart the two
#'   slots are — opposite slots reach the centre. A number overrides it:
#'   `1` always bulges to the centre.
#' @param ncp Number of points sampled along each curve.
#' @param alpha,linewidth Line width / ribbon opacity defaults.
#' @param arrow Optional arrow specification for `type = "line"`.
#'
#' @return A ggplot2 layer.
#' @section Geometry:
#' For a slot pair `[a0, a1]` and `[b0, b1]`, let `d` be the shortest signed
#' angular distance between the two facing slot edges. The control point
#' radius is `k = 1 - 4|d| / (3pi)`, which is `1` for adjacent slots (the
#' curve stays on the ring) and `-1/3` for opposite slots (the curve passes
#' through the centre). The control points share the endpoints' angles, so
#' every chord leaves the ring radially.
#'
#' @examples
#' m <- cor(mtcars)
#' ggcircos(m, order = "hclust") +
#'     geom_chord(aes(fill = r)) +
#'     ggplot2::scale_fill_gradient2(midpoint = 0)
#' @importFrom ggplot2 layer
#' @export
geom_chord <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       from = NULL,
                       to = NULL,
                       type = c("ribbon", "line"),
                       curvature = "auto",
                       ncp = 40,
                       alpha = NULL,
                       linewidth = NULL,
                       arrow = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    type <- match.arg(type)
    if (identical(curvature, "auto")) curvature <- NA_real_
    if (is.na(curvature)) alpha <- alpha %||% if (type == "ribbon") 0.5 else 0.9
    linewidth <- linewidth %||% if (type == "ribbon") 0.1 else 0.3
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomChord,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                type = type,
                curvature = curvature,
                ncp = ncp,
                alpha = alpha,
                linewidth = linewidth,
                arrow = arrow,
                na.rm = na.rm,
                ...
            )
        ),
        "ggcircos_chord",
        list(from = rlang::enquo(from), to = rlang::enquo(to))
    )
}

#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 draw_key_path
#' @importFrom grid pathGrob gpar polylineGrob nullGrob gList gTree
#' @importFrom scales alpha
#' @export
GeomChord <- ggproto("GeomChord", Geom,
    required_aes = c("x", "y", "xend", "yend"),
    non_missing_aes = c("linewidth", "linetype", "alpha"),
    # the slot columns are mapped, not defaulted: a NA in default_aes would
    # overwrite the values the layout computed
    optional_aes = c(
        "group", "subset", "from", "to",
        "theta0", "theta1", "theta_end0", "theta_end1"
    ),
    default_aes = ggplot2::aes(
        colour = "black", fill = "grey50", linewidth = 0.3,
        linetype = 1, alpha = 0.5
    ),
    draw_key = draw_key_path,
    setup_data = function(data, params) {
        if (!is.null(data$subset)) {
            data <- data[which(data$subset), , drop = FALSE]
        }
        data
    },
    draw_panel = function(self, data, panel_params, coord,
                          type = "ribbon",
                          curvature = NA_real_,
                          ncp = 40,
                          arrow = NULL,
                          na.rm = FALSE) {
        if (nrow(data) == 0L) return(grid::nullGrob())
        linear <- isTRUE(coord$is_linear())
        data <- data[stats::complete.cases(data[, c("x", "y", "xend", "yend")]), , drop = FALSE]
        if (nrow(data) == 0L) return(grid::nullGrob())

        # slots are needed for ribbons; fall back to point endpoints for lines
        if (type == "ribbon") {
            need <- c("theta0", "theta1", "theta_end0", "theta_end1")
            if (!all(need %in% names(data))) {
                ggcircos_abort(
                    "internal",
                    "{.fn geom_chord} needs the slot columns {.field {need}}; are the
                     coordinates coming from {.fn ggcircos}?"
                )
            }
        }
        pieces <- if (linear) {
            chord_paths_cartesian(data, type, curvature, ncp)
        } else {
            chord_paths_polar(data, type, curvature, ncp)
        }
        pieces <- coord$transform(pieces, panel_params)

        # grid gives one gp per path, so rows are batched by style: a discrete
        # fill collapses to a handful of grobs. A continuous fill would be one
        # grob per link, so past a threshold the colours are quantised onto an
        # rgb grid instead (B11) — each chunk then draws with its first row's
        # exact colour, which is within half a step of every member.
        sty <- data[pieces$.id, , drop = FALSE]
        key <- paste(
            sty$fill, sty$colour, sty$linewidth, sty$linetype, sty$alpha,
            sep = "\r"
        )
        if (length(unique(key)) > 24L) {
            key <- paste(style_key(sty$fill), style_key(sty$colour),
                         sty$linewidth, sty$linetype, sty$alpha, sep = "\r")
        }
        chunks <- split(seq_len(nrow(pieces)), key)
        grobs <- lapply(chunks, function(i) {
            d <- sty[i[1L], , drop = FALSE]
            # aesthetics arrive as data columns: a fixed value in geom_chord()
            # becomes a constant aes_param, not a draw_panel argument
            gp <- gpar(
                col = scales::alpha(d$colour, d$alpha),
                fill = if (type == "ribbon") scales::alpha(d$fill, d$alpha) else NA,
                lwd = d$linewidth * ggcircos_pt,
                lty = d$linetype,
                lineend = "butt",
                linejoin = "round"
            )
            ids <- match(pieces$.id[i], unique(pieces$.id[i]))
            if (type == "ribbon") {
                pathGrob(
                    pieces$x[i], pieces$y[i], id = ids,
                    default.units = "native", rule = "evenodd", gp = gp
                )
            } else {
                polylineGrob(
                    pieces$x[i], pieces$y[i], id = ids,
                    default.units = "native",
                    gp = gpar(
                        col = gp$col, lwd = gp$lwd, lty = gp$lty,
                        lineend = gp$lineend, linejoin = gp$linejoin
                    ),
                    arrow = arrow
                )
            }
        })
        ggname("geom_chord", gTree(children = do.call(gList, grobs)))
    }
)

#' Points per inch, matching ggplot2's own line-width unit.
#' @keywords internal
#' @noRd
ggcircos_pt <- 72.27 / 25.4

#' Quantise a colour vector onto a 16-level rgb grid for style batching
#'
#' NA stays NA (col2rgb would silently turn it white).
#' @keywords internal
#' @noRd
style_key <- function(colour) {
    out <- rep(NA_character_, length(colour))
    ok <- !is.na(colour) & nzchar(colour)
    if (any(ok)) {
        rgb <- grDevices::col2rgb(colour[ok])
        q <- apply(rgb %/% 16L, 2L, paste, collapse = "_")
        out[ok] <- q
    }
    out
}

#' Vertex paths for the polar layout, in (radius, angle) space.
#' @keywords internal
#' @noRd
chord_paths_polar <- function(data, type, curvature, ncp) {
    m <- max(2L, as.integer(ncp))
    out <- list()
    if (type == "ribbon") {
        for (i in seq_len(nrow(data))) {
            a0 <- data$theta0[i]; a1 <- data$theta1[i]
            b0 <- data$theta_end0[i]; b1 <- data$theta_end1[i]
            d1 <- ang_diff(a1, b0)
            d2 <- ang_diff(b1, a0)
            k1 <- bulge_radius(d1, curvature)
            k2 <- bulge_radius(d2, curvature)
            ring <- 1
            out[[length(out) + 1L]] <- data.frame(
                x = c(
                    rep(ring, arc_samples(m)), bezier_r(ring, k1, ring, m),
                    rep(ring, arc_samples(m)), bezier_r(ring, k2, ring, m)
                ),
                y = c(
                    seq(a0, a1, length.out = arc_samples(m)),
                    bezier_t(a1, a1 + d1, m),
                    seq(b0, b1, length.out = arc_samples(m)),
                    bezier_t(b1, b1 + d2, m)
                ),
                .id = i
            )
        }
    } else {
        for (i in seq_len(nrow(data))) {
            t0 <- data$y[i]; t1 <- data$yend[i]
            r0 <- data$x[i]; r1 <- data$xend[i]
            d <- ang_diff(t0, t1)
            k <- bulge_radius(d, curvature)
            out[[length(out) + 1L]] <- data.frame(
                x = c(bezier_r(r0, k, r1, m)),
                y = c(bezier_t(t0, t0 + d, m)),
                .id = i
            )
        }
    }
    do.call(rbind, out)
}

#' Vertex paths for the linear (arc) layout, in (x, y) space.
#' @keywords internal
#' @noRd
chord_paths_cartesian <- function(data, type, curvature, ncp) {
    m <- max(2L, as.integer(ncp))
    out <- list()
    if (type == "ribbon") {
        for (i in seq_len(nrow(data))) {
            a0 <- data$theta0[i]; a1 <- data$theta1[i]
            b0 <- data$theta_end0[i]; b1 <- data$theta_end1[i]
            d1 <- ang_diff(a1, b0)
            d2 <- ang_diff(b1, a0)
            k1 <- bulge_cart(d1, curvature)
            k2 <- bulge_cart(d2, curvature)
            # the foot is the node's own extent on the spine (x = r = 0)
            out[[length(out) + 1L]] <- data.frame(
                x = c(
                    rep(0, arc_samples(m)),
                    bezier_r(0, k1, 0, m),
                    rep(0, arc_samples(m)),
                    bezier_r(0, k2, 0, m)
                ),
                y = c(
                    seq(a0, a1, length.out = arc_samples(m)),
                    bezier_t(a1, a1 + d1, m),
                    seq(b0, b1, length.out = arc_samples(m)),
                    bezier_t(b1, b1 + d2, m)
                ),
                .id = i
            )
        }
    } else {
        for (i in seq_len(nrow(data))) {
            t0 <- data$y[i]; t1 <- data$yend[i]
            d <- ang_diff(t0, t1)
            k <- bulge_cart(d, curvature)
            out[[length(out) + 1L]] <- data.frame(
                x = bezier_r(0, k, 0, m),
                y = bezier_t(t0, t0 + d, m),
                .id = i
            )
        }
    }
    do.call(rbind, out)
}

#' Control point radius for the polar layout.
#'
#' `k = 1 - 4|d| / (3pi)`: `1` keeps the curve on the ring, `-1/3` sends it
#' through the centre. With an explicit `curvature` the user wins.
#' @keywords internal
#' @noRd
bulge_radius <- function(d, curvature) {
    if (is.na(curvature)) {
        1 - 4 * abs(d) / (3 * pi)
    } else {
        -(curvature / 3) * sign(d)
    }
}

#' Control point for the linear layout: an absolute distance left of the spine.
#' @keywords internal
#' @noRd
bulge_cart <- function(d, curvature) {
    if (is.na(curvature)) {
        -(4 / (3 * pi)) * abs(d)
    } else {
        -(curvature / 3) * sign(d)
    }
}

#' Radius along a cubic bezier whose control radii are both `k`.
#' @keywords internal
#' @noRd
bezier_r <- function(r0, k, r1, m) {
    t <- seq(0, 1, length.out = m)
    (1 - t)^3 * r0 + 3 * (1 - t)^2 * t * k + 3 * (1 - t) * t^2 * k + t^3 * r1
}

#' Angle along a cubic bezier whose control angles equal the endpoints.
#' @keywords internal
#' @noRd
bezier_t <- function(t0, t1, m) {
    t <- seq(0, 1, length.out = m)
    b0 <- (1 - t)^3 + 3 * (1 - t)^2 * t
    b1 <- 3 * (1 - t) * t^2 + t^3
    b0 * t0 + b1 * t1
}
#' ggcircos geoms, stats and positions
#'
#' The building blocks behind [geom_chord()], [geom_node_arc()],
#' [geom_track()] and [geom_track_axis()]. Rarely needed directly: they are
#' documented so that the layers' defaults and geometry can be looked up.
#'
#' @name ggcircos-ggproto
#' @keywords internal
NULL
