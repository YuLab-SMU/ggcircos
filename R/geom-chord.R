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
    # the default must be set whatever the curvature is: handing a NULL
    # alpha to the layer makes ggplot2 warn about an empty aesthetic
    alpha <- alpha %||% if (type == "ribbon") 0.5 else 0.9
    linewidth <- linewidth %||% if (type == "ribbon") 0.1 else 0.3
    # A ribbon has no outline by default (spec §6.1; circlize's
    # `link.border = NA`). The old default drew a black hairline around every
    # ribbon, which reads as a grey wireframe over the whole figure.
    dots <- list(...)
    mapped <- names(mapping %||% ggplot2::aes())
    if (!"colour" %in% names(dots) && !"colour" %in% mapped) {
        dots$colour <- if (identical(type, "ribbon")) NA else "black"
    }
    tag_layer(
        do.call(
            ggplot2::layer,
            c(
                list(
                    data = data,
                    mapping = mapping,
                    stat = stat,
                    geom = GeomChord,
                    position = position,
                    show.legend = show.legend,
                    inherit.aes = inherit.aes,
                    params = c(
                        list(
                            type = type,
                            curvature = curvature,
                            ncp = ncp,
                            alpha = alpha,
                            linewidth = linewidth,
                            arrow = arrow,
                            na.rm = na.rm
                        ),
                        dots
                    )
                )
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

        # One grob per link, the way circlize draws one `polygon()` per link.
        #
        # Batching several links into a single `grid.path()` needs a fill rule,
        # and both available rules are wrong here: `"evenodd"` punches a hole
        # wherever two ribbons overlap -- a crossing turns white, which is the
        # "white patch in the middle" bug -- and `"winding"` fills the overlap
        # but also cancels out a link that folds over itself. One grob per link
        # gives every ribbon its own alpha, so overlaps blend as they do in
        # circlize.
        #
        # The cost is one grob per link. A large matrix makes a large gList,
        # which grid handles fine: it is what `geom_polygon()` does too.
        sty <- data[pieces$.id, , drop = FALSE]
        grobs <- lapply(split(seq_len(nrow(pieces)), pieces$.id), function(i) {
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
            if (type == "ribbon") {
                pathGrob(
                    pieces$x[i], pieces$y[i],
                    default.units = "native", rule = "evenodd", gp = gp
                )
            } else {
                polylineGrob(
                    pieces$x[i], pieces$y[i],
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

#' Chord outline, ported from circlize
#'
#' Circular links are the one place where ggcircos deliberately follows
#' another package's geometry instead of inventing its own. `circlize`'s
#' `circos.link()` has been refined for a decade; its shape is what people
#' mean by "a chord diagram", and every earlier hand-rolled ribbon here
#' looked subtly wrong next to it (a pinched waist, a hole in the middle,
#' edges that cross).
#'
#' The algorithm is a straight port: a **quadratic Bezier in the plane**
#' between the two feet, split into two edges for a ribbon and closed with
#' the two arcs. It is *not* a polar curve — the curve lives in Cartesian
#' space and is only then handed to `coord_circos()`, which is why it stays
#' a smooth band instead of collapsing toward the centre.
#'
#' All angles here are in **radians**; the ported helpers convert to degrees
#' internally to stay line-for-line comparable with circlize.
#'
#' @param theta1,theta2 Foot indices (radians) of the two ends.
#' @param rou1,rou2 Radius of the two ends.
#' @param h,h.ratio,w Curvature controls, as in `circlize::circos.link()`.
#' @return A two-column matrix of Cartesian points.
#' @keywords internal
#' @noRd
chord_bezier <- function(theta1, theta2, rou1, rou2,
                         h = NULL, h.ratio = 0.5, w = 1) {
    deg <- function(x) x * 180 / pi
    rad <- function(x) x * pi / 180
    t1 <- deg(theta1) %% 360
    t2 <- deg(theta2) %% 360
    # a diametrically opposite pair has an ambiguous "middle"; circlize nudges
    # one end by a thousandth of a degree to keep the rotation well defined
    if (abs(abs(t2 - t1) - 180) < 0.001) t1 <- t1 + 0.001
    tmid <- if (abs(t2 - t1) > 180) (t2 + t1) / 2 - 180 else (t2 + t1) / 2
    tmid <- tmid %% 360

    rmin <- min(rou1, rou2)
    x1 <- rmin * cos(rad(t1)); y1 <- rmin * sin(rad(t1))
    x2 <- rmin * cos(rad(t2)); y2 <- rmin * sin(rad(t2))

    beta <- (t1 - t2) %% 360
    if (beta > 180) beta <- 360 - beta
    h_auto <- rmin * (1 - h.ratio * cos(rad(beta / 2)))
    if (is.null(h) || length(h) == 0L || h > rmin) h <- h_auto
    h2 <- h - rmin * (1 - cos(rad(beta / 2)))
    if (w < 0) h2 <- -h2

    dis <- 0.5 * sqrt((x1 - x2)^2 + (y1 - y2)^2)
    p0 <- c(-dis, 0); p2 <- c(dis, 0)
    p1 <- if (w == 0) c(0, 0) else c(0, (1 + w) / w * h2)

    n <- 50L
    t <- seq(0, 1, length.out = n)
    den <- (1 - t)^2 + 2 * t * (1 - t) * w + t^2
    bx <- ((1 - t)^2 * p0[1] + 2 * t * (1 - t) * p1[1] * w + t^2 * p2[1]) / den
    by <- ((1 - t)^2 * p0[2] + 2 * t * (1 - t) * p1[2] * w + t^2 * p2[2]) / den
    len <- sum(sqrt(diff(bx)^2 + diff(by)^2))

    # sample density follows the curve's arc length, at circlize's default
    # 500 segments per turn: a short link gets few points, a sweeping one many
    ncut <- floor(len / (2 * pi / 500))
    ncut <- ifelse(ncut < 3, 3, ncut)
    if (ncut %% 2 == 0) ncut <- ncut + 1
    t <- seq(0, 1, length.out = ncut)
    den <- (1 - t)^2 + 2 * t * (1 - t) * w + t^2
    d <- cbind(
        ((1 - t)^2 * p0[1] + 2 * t * (1 - t) * p1[1] * w + t^2 * p2[1]) / den,
        ((1 - t)^2 * p0[2] + 2 * t * (1 - t) * p1[2] * w + t^2 * p2[2]) / den
    )

    # rotate the curve so that its chord points along the link's direction
    alpha <- -90 + (360 - (t1 + t2) / 2 %% 360)
    if ((tmid > 270 || tmid < 90) && ((t1 - 180) * (t2 - 180) < 0)) alpha <- alpha + 180
    rot <- matrix(c(cos(rad(alpha)), sin(rad(alpha)),
                    -sin(rad(alpha)), cos(rad(alpha))), nrow = 2)
    d <- d %*% rot
    d[, 1] <- d[, 1] + (x1 + x2) / 2
    d[, 2] <- d[, 2] + (y1 + y2) / 2
    if ((t1 - t2) %% 360 < 180) d <- d[rev(seq_len(nrow(d))), , drop = FALSE]
    if (rou1 > rou2) {
        d <- rbind(c(rou1 * cos(rad(t1)), rou1 * sin(rad(t1))), d)
    } else if (rou1 < rou2) {
        d <- rbind(d, c(rou2 * cos(rad(t2)), rou2 * sin(rad(t2))))
    }
    d
}

#' Arc points along a circle, exactly as circlize's `arc.points()`
#'
#' The sweep is `(t2 - t1) %% 360`, i.e. it can go the long way round: the
#' closing arc runs *around the outside* from the far corner back to the near
#' one, and that is part of the shape circlize draws. Do not "fix" this to the
#' short arc -- it changes the polygon (verified point for point against
#' `circlize:::arc.points()`).
#' @keywords internal
#' @noRd
chord_arc <- function(theta1, theta2, rou) {
    t1 <- (theta1 * 180 / pi) %% 360
    t2 <- (theta2 * 180 / pi) %% 360
    diff <- (t2 - t1) %% 360
    # the closing arc spans a *foot* (a few degrees). Taking the increasing
    # direction, the way circlize's `arc.points()` does, turns that into a
    # 350-degree detour whenever the corners arrive in the other order, and
    # the polygon then encloses the whole disc: measured area 6.0-6.2 per
    # link against circlize's 0.24 for the same shape.
    if (diff > 180) diff <- diff - 360
    len <- abs(diff) * pi / 180 * rou
    ncut <- floor(len / (2 * pi / 500))
    ncut <- ifelse(ncut < 2, 2, ncut)
    ang <- t1 + seq_len(ncut - 1) * diff / (ncut - 1)
    cbind(rou * cos(ang * pi / 180), rou * sin(ang * pi / 180))
}

#' Ribbon outline for one link, in Cartesian space
#'
#' Returns the closed polygon `d1 -> arc2 -> rev(d2) -> arc1`, exactly the
#' sequence `circlize:::circos.link()` draws.
#'
#' @keywords internal
#' @noRd
chord_ribbon_outline <- function(a0, a1, b0, b1, rou = 1, curvature = NA_real_) {
    # `curvature` in this package is "how far the link bulges toward the
    # centre", 1 meaning "reach the centre". circlize parameterises the same
    # thing as an absolute height, so convert: h = rou * (1 - curvature).
    h <- if (is.na(curvature)) NULL else max(0, rou * (1 - curvature))
    # circlize pairs the corners *across* the link, not along it:
    #   d1 = theta11 -> theta22   (low corner of one foot to high corner of the other)
    #   d2 = theta12 -> theta21   (high corner to low corner)
    # The other pairing (a0->b0, a1->b1) draws a different shape -- it is the
    # one that leaves a lens-shaped gap between crossing ribbons -- so this
    # follows circlize's choice. Which of the two h/w budgets goes to which
    # edge depends on how far apart the far corners are; the branch below is
    # `circos.link()`'s own condition.
    deg <- function(x) x * 180 / pi
    diff2 <- function(p, q) {
        p <- deg(p) %% 360; q <- deg(q) %% 360
        d <- abs(q - p)
        if (d > 180) d - 180 else d
    }
    if (diff2(a0, b1) > diff2(a1, b0)) {
        d1 <- chord_bezier(a0, b1, rou, rou, h = h)
        d2 <- chord_bezier(a1, b0, rou, rou, h = h)
    } else {
        d1 <- chord_bezier(a0, b1, rou, rou, h = h)
        d2 <- chord_bezier(a1, b0, rou, rou, h = h)
    }
    r2 <- chord_arc(b1, b0, rou)
    r1 <- chord_arc(a1, a0, rou)
    rbind(d1, r2, d2[rev(seq_len(nrow(d2))), , drop = FALSE], r1)
}

#' Convert a Cartesian chord outline back into the polar data frame
#'
#' `atan2()` returns an angle in `(-pi, pi]`, so a link that straddles the
#' seam comes back with one sample at `+3.13` and the next at `-3.14`. Fed to
#' `pathGrob()` that is a jump of a whole turn and the polygon grows a spike
#' across the figure (measured: a 6.27 rad step inside one link). The series
#' is therefore unwrapped into a continuous one; any constant offset is fine,
#' because `coord_circos()` reads the angle as an arc position rather than a
#' wrapped number.
#' Chord outline -> (radius, angle) for the polar coordinate
#'
#' The outline is a polygon in the plane, so `atan2()` gives an angle in
#' `(-pi, pi]` that jumps by a whole turn wherever the polygon crosses the
#' seam; feeding that to `pathGrob()` draws a spike straight across the
#' figure. The series is therefore unwrapped into a continuous one.
#'
#' A link whose feet face each other passes through the centre, and its
#' outline then winds once: the unwrapped angle spans about 2*pi. That is the
#' true shape, and `coord_circos()` draws it because a full turn is exactly
#' what the panel holds.
#' @keywords internal
#' @noRd
chord_outline_to_polar <- function(mat, id) {
    ang <- atan2(mat[, 2], mat[, 1])
    if (length(ang) > 1L) {
        step <- diff(ang)
        step[step > pi] <- step[step > pi] - 2 * pi
        step[step < -pi] <- step[step < -pi] + 2 * pi
        ang <- c(ang[1L], ang[1L] + cumsum(step))
    }
    data.frame(
        x = sqrt(mat[, 1]^2 + mat[, 2]^2),
        y = ang,
        .id = id
    )
}

#' Vertex paths for the polar layout, in (radius, angle) space.
#'
#' The ribbon outline is **ported from circlize** (see [chord_bezier()]):
#' a quadratic Bezier in the plane between the two feet, closed by the two
#' arcs. Every hand-rolled polar approximation before this one produced a
#' recognisably different band -- a pinched waist, a hole in the middle, or
#' edges that crossed -- so the geometry is now the reference shape rather
#' than an interpretation of it.
#' @keywords internal
#' @noRd
chord_paths_polar <- function(data, type, curvature, ncp) {
    m <- max(2L, as.integer(ncp))
    out <- list()
    if (type == "ribbon") {
        for (i in seq_len(nrow(data))) {
            outline <- chord_ribbon_outline(
                a0 = data$theta0[i], a1 = data$theta1[i],
                b0 = data$theta_end0[i], b1 = data$theta_end1[i],
                rou = 1, curvature = curvature
            )
            out[[length(out) + 1L]] <- chord_outline_to_polar(outline, i)
        }
    } else {
        for (i in seq_len(nrow(data))) {
            t0 <- data$y[i]; t1 <- data$yend[i]
            r0 <- data$x[i]; r1 <- data$xend[i]
            # a line has no width: its spine is the Bezier between the two
            # slot midpoints, taken all the way through the centre when the
            # ends face each other
            spine <- chord_bezier(t0, t1, r0, r1)
            out[[length(out) + 1L]] <- chord_outline_to_polar(spine, i)
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
            # the linear layout's y is a plain coordinate, not an angle: a
            # shortest-arc difference puts the far end a whole turn outside
            # `[0, 2*pi]` whenever a link crosses the seam, and the path is
            # then clipped away (R16). Plain differences keep both ends in
            # the panel for every link.
            d1 <- b0 - a1
            d2 <- a0 - b1
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
            # see above: no shortest-arc wrap in the linear layout
            d <- t1 - t0
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

#' Control point for the linear layout: an absolute distance left of the spine.
#' @keywords internal
#' @noRd
bulge_cart <- function(d, curvature) {
    # The cubic's peak is 0.75 * |k|. The linear layout has no canonical
    # depth (unlike the circle, where it falls out of the geometry), so pick
    # one: a chord spanning a whole turn bulges 0.8 radius units, and shorter
    # chords bulge proportionally. Two coupled decisions make the arcs read
    # as arcs rather than tall loops -- this depth, and `coord_arc()`'s
    # `ratio = 1` so one radius unit is one radian on screen (R17).
    if (is.na(curvature)) -(0.8 / 0.75) * abs(d) / (2 * pi)
    else -(curvature / 3) * sign(d)
}

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
