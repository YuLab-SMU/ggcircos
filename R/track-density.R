# Per-node densities in a circular band.
#
# Two geometries share this file because they share one kernel: `"violin"`
# draws the density as a closed outline (the value runs along the radius, the
# density decides how much of the node's arc it takes at that value), and
# `"density"` draws it as an open ridgeline curve rising from the node's centre
# line. Neither shape is what `geom_violin()` draws, and neither can be done
# downstream of the stat:
#
#   * `stat_ydensity` puts the value on `y` and the density on `x`, a quarter
#     turn off, and asks for a discrete x to group by -- the same y-axis clash
#     `StatTrackBox` exists to avoid;
#   * the angular offset has to be written into `y` *by the stat*, because
#     `position_track()` only ever sees aesthetics, and a stat's extra columns
#     are not aesthetics (spec §6.4, B41).
#
# So the stats do the KDE and build the angles; the position keeps owning the
# radius, exactly as it does for `"col"`.

# ---------------------------------------------------------------------------
# the shared kernel

#' Normalised per-node density samples
#'
#' The one piece `StatTrackViolin` and `StatTrackDensity` have in common: group
#' by node, run `stats::density()`, optionally trim to the observed range, and
#' scale the height to `[0, 1]` so two nodes -- and the two geometries -- are
#' on the same scale.
#'
#' Returns `NULL` for a node that cannot carry a KDE (fewer than three finite
#' values, or no spread at all). A caller decides whether that is worth a
#' message; returning a degenerate flat line instead would silently draw a
#' distribution that is not there.
#' @keywords internal
#' @noRd
node_density <- function(values, n = 64L, adjust = 1, trim = TRUE) {
    values <- values[is.finite(values)]
    if (length(values) < 3L || stats::sd(values) == 0) return(NULL)
    dens <- stats::density(values, n = as.integer(n), adjust = adjust)
    at <- dens$x
    w <- dens$y
    if (isTRUE(trim)) {
        keep <- at >= min(values) & at <= max(values)
        at <- at[keep]
        w <- w[keep]
    }
    if (length(at) < 2L) return(NULL)
    list(value = at, weight = w / max(w))
}

#' Group a track frame by node, the way the density stats need it
#'
#' The data's `y` is an angle, so it cannot group anything; `node` is the
#' column that can. Falls back to `group` for a hand-built layer.
#' @keywords internal
#' @noRd
track_node_groups <- function(data) {
    if ("node" %in% names(data)) {
        as.character(data$node)
    } else if (!is.null(data$group)) {
        as.character(data$group)
    } else {
        rep("1", nrow(data))
    }
}

#' The node's half arc, in radians
#'
#' `.circos_half` is written by [track_angles()] and mapped by `ggplot_add()`;
#' it cannot be recovered from `ymin`/`ymax` here because the angle scale
#' (`limits = c(0, 2*pi)`, `oob = squish`) clamps a straddling node's negative
#' `ymin` up to 0 *before* the stat runs (B41). The fallback keeps a
#' hand-built layer working.
#' @keywords internal
#' @noRd
track_half_arc <- function(data) {
    if (".circos_half" %in% names(data)) {
        data$.circos_half
    } else if (all(c("ymin", "ymax") %in% names(data))) {
        (data$ymax - data$ymin) / 2
    } else {
        rep(NA_real_, nrow(data))
    }
}

#' Emit the rows one stat piece owns, with the columns downstream expects
#' @keywords internal
#' @noRd
density_rows <- function(x, y, node, group, panel) {
    data.frame(
        x = x,
        y = y,
        node = rep(node, length(x)),
        group = rep(group, length(x)),
        PANEL = rep(panel, length(x)),
        stringsAsFactors = FALSE
    )
}

#' Complain once when no node could produce a density
#' @keywords internal
#' @noRd
abort_no_density <- function(geom) {
    ggcircos_abort(
        "data",
        c(
            "{.fn geom_track} {.val {geom}} has nothing to draw: no node has \\
             at least three finite values with some spread.",
            i = "A KDE needs {.code >= 3} distinct values per node to have a shape."
        )
    )
}

#' @keywords internal
#' @noRd
density_params <- function(params) {
    # `params[["n"]]`, never `params$n`: ggplot2 seeds the parameter list with
    # `na.rm`, and `$` partial matching turns `params$n` into `params$na.rm`
    # -- FALSE -- so the bandwidth argument arrived as a logical (B41).
    params[["n"]] <- params[["n"]] %||% 64L
    params[["adjust"]] <- params[["adjust"]] %||% 1
    params[["trim"]] <- params[["trim"]] %||% TRUE
    params[["na.rm"]] <- params[["na.rm"]] %||% FALSE
    params
}

# ---------------------------------------------------------------------------
# violin: the closed outline

#' Per-node density outline, as a track stat
#'
#' A violin in a band is turned sideways: the value runs along the radius and
#' the density decides how much of the node's arc the outline takes at that
#' value. `stat_ydensity()` (what `geom_violin()` uses) puts the density on
#' `x` and needs a discrete y axis to group by, so this stat builds the outline
#' itself: `stats::density()` per node, normalised so the tallest point of the
#' node's own distribution fills `half` of its arc (`half` comes from
#' `.circos_half`, which [track_angles()] writes).
#'
#' One node becomes one closed polygon. Points are emitted in drawing order --
#' up one side of the outline, back along the other -- so the geom only has to
#' fill them.
#'
#' Nodes that cannot carry a KDE (fewer than three finite values, or no spread
#' at all) contribute no rows instead of a degenerate line; the remaining nodes
#' still draw.
#' @keywords internal
#' @noRd
StatTrackViolin <- ggproto("StatTrackViolin", Stat,
    required_aes = "x",
    optional_aes = c("node", "ymin", "ymax", ".circos_half"),
    setup_params = function(data, params) density_params(params),
    extra_params = c("na.rm", "n", "adjust", "trim"),
    compute_panel = function(self, data, scales, n = 64L, adjust = 1,
                             trim = TRUE, na.rm = FALSE) {
        if (nrow(data) == 0L) return(data.frame())
        key <- track_node_groups(data)
        half <- track_half_arc(data)
        idx <- split(seq_len(nrow(data)), key)
        panel <- if ("PANEL" %in% names(data)) data$PANEL[1] else 1L
        out <- vector("list", length(idx))
        drawn <- 0L
        for (k in names(idx)) {
            i <- idx[[k]]
            d <- node_density(data$x[i], n = n, adjust = adjust, trim = trim)
            if (is.null(d)) next
            centre <- data$y[i][1]
            h <- half[i][1]
            if (!is.finite(centre) || !is.finite(h) || h <= 0) next
            # up one side, back down the other: a closed outline in (value, angle)
            pieces <- seam_pieces(
                c(d$value, rev(d$value)),
                c(centre + d$weight * h, rev(centre - d$weight * h))
            )
            for (p in pieces) {
                drawn <- drawn + 1L
                out[[drawn]] <- density_rows(p$x, p$y, k, drawn, panel)
            }
        }
        if (!drawn) abort_no_density("violin")
        out <- do.call(rbind, out[seq_len(drawn)])
        rownames(out) <- NULL
        out
    }
)

#' The violin outline, as a fillable polygon
#'
#' `GeomPolygon` needs nothing beyond `x`/`y` and a numeric `group` per
#' polygon; the stat has already put the angles in drawing order.
#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
GeomTrackViolin <- ggproto("GeomTrackViolin", ggplot2::GeomPolygon,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
        colour = NA, fill = "grey35", linewidth = 0.1,
        linetype = 1, alpha = NA
    )
)

# ---------------------------------------------------------------------------
# density: the open ridgeline

#' Per-node density curve, as a track stat
#'
#' The companion to `StatTrackViolin`: the same kernel, drawn as an open
#' curve instead of a closed outline. Each node's density rises from its
#' centre line (`theta`) towards `theta + w * half`, so several distributions
#' can share a band without covering each other -- the filled violin cannot do
#' that, which is why this is a separate geometry rather than a `fill = FALSE`
#' switch.
#'
#' Points run in value order and are never closed, so the geom is a path. Nodes
#' with too few values or no spread contribute no rows.
#' @keywords internal
#' @noRd
StatTrackDensity <- ggproto("StatTrackDensity", Stat,
    required_aes = "x",
    optional_aes = c("node", "ymin", "ymax", ".circos_half"),
    setup_params = function(data, params) density_params(params),
    extra_params = c("na.rm", "n", "adjust", "trim"),
    compute_panel = function(self, data, scales, n = 64L, adjust = 1,
                             trim = TRUE, na.rm = FALSE) {
        if (nrow(data) == 0L) return(data.frame())
        key <- track_node_groups(data)
        half <- track_half_arc(data)
        idx <- split(seq_len(nrow(data)), key)
        panel <- if ("PANEL" %in% names(data)) data$PANEL[1] else 1L
        out <- vector("list", length(idx))
        drawn <- 0L
        for (k in names(idx)) {
            i <- idx[[k]]
            d <- node_density(data$x[i], n = n, adjust = adjust, trim = trim)
            if (is.null(d)) next
            centre <- data$y[i][1]
            h <- half[i][1]
            if (!is.finite(centre) || !is.finite(h) || h <= 0) next
            for (p in seam_folds(d$value, centre + d$weight * h)) {
                drawn <- drawn + 1L
                out[[drawn]] <- density_rows(p$x, p$y, k, drawn, panel)
            }
        }
        if (!drawn) abort_no_density("density")
        out <- do.call(rbind, out[seq_len(drawn)])
        rownames(out) <- NULL
        out
    }
)

#' The ridgeline, as a stroke
#'
#' `GeomPath` plus nothing: the stat has already placed every point, so the
#' geom only has to draw the line. Kept as its own class (rather than a bare
#' `"path"`) so `geom_track(geom = "density")` can carry density defaults and
#' so a future option has somewhere to live.
#' @rdname ggcircos-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @export
GeomTrackDensity <- ggproto("GeomTrackDensity", ggplot2::GeomPath,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
        colour = "grey35", linewidth = 0.5,
        linetype = 1, alpha = NA
    )
)

# ---------------------------------------------------------------------------
# seam handling

#' Cut a closed outline into the pieces a full-turn panel can show
#'
#' The angle scale of a `ggcircos()` plot spans `[0, 2*pi]` and squishes
#' anything outside it. That is harmless for a bar, which only gets narrower at
#' the seam, but a violin is a *closed* outline: squishing it turns everything
#' past the seam into a flat edge lying on the seam, and the fill collapses.
#'
#' So the outline is cut at the seam and the part that ran past `2*pi`
#' reappears just past `0`, which is the same place on a circle. Each piece is
#' a clipped copy of the polygon, in drawing order, ready to be filled on its
#' own.
#'
#' The cut is a Sutherland-Hodgman clip against one angular window per full
#' turn the outline touches; a violin spans less than a turn, so in practice
#' that is one window, or two for a node that straddles the seam.
#' @keywords internal
#' @noRd
seam_pieces <- function(x, y, turn = 2 * pi) {
    if (!length(x) || !length(y)) return(list())
    if (all(y >= 0 & y <= turn)) {
        return(list(list(x = x, y = y)))
    }
    # which turn each vertex falls in; an exact `turn` belongs to the first
    # one, or the seam vertex would be drawn twice, once per side
    win <- floor(y / turn - 1e-12)
    out <- list()
    for (k in unique(win)) {
        lo <- k * turn
        p <- clip_against(x, y, lo, above = TRUE)
        if (is.null(p)) next
        p <- clip_against(p$x, p$y, lo + turn, above = FALSE)
        if (is.null(p)) next
        out[[length(out) + 1L]] <- list(x = p$x, y = p$y - lo)
    }
    out
}

#' Break an open curve at the seam instead of clipping it
#'
#' A polygon clip is the wrong tool for a stroke: Sutherland-Hodgman returns a
#' *closed* ring whose edges lie on the boundary, and for a filled shape those
#' edges are invisible -- but drawn as a line they would put a straight segment
#' along the seam that the data never asked for.
#'
#' So an open curve is only *split*: walk the vertices in order, and whenever
#' two neighbours fall in different turns insert a fold exactly at the seam,
#' end the current run there and start the next one at the matching spot. The
#' two runs meet at the same point on the circle, so the curve reads as
#' continuous.
#' @keywords internal
#' @noRd
seam_folds <- function(x, y, turn = 2 * pi) {
    n <- length(y)
    if (!n) return(list())
    if (all(y >= 0 & y <= turn)) {
        return(list(list(x = x, y = y)))
    }
    # No unwrapping here, deliberately. A density curve is sampled densely
    # (a few dozen points over at most one node's arc), so neighbouring points
    # are never a whole turn apart -- measured max step 0.056 rad against a
    # half turn of 3.14. Reading the series by "shortest step" would therefore
    # only ever reinterpret the curve's own *descending* stretches as wraps,
    # which cut one crossing into three pieces with one of them spanning the
    # whole turn (B42). A point's turn index is simply where it lies.
    turn_of <- floor(y / turn - 1e-12)
    pieces <- list()
    cx <- numeric(0)
    cy <- numeric(0)
    flush <- function() {
        if (length(cx) >= 2L) {
            pieces[[length(pieces) + 1L]] <<- list(x = cx, y = cy)
        }
        cx <<- numeric(0)
        cy <<- numeric(0)
    }
    for (i in seq_len(n)) {
        if (i > 1L && turn_of[i] != turn_of[i - 1L]) {
            # end the run where the curve meets the seam, and start the next one
            # at the same place measured in its own turn
            edge <- if (turn_of[i] > turn_of[i - 1L]) {
                (turn_of[i - 1L] + 1L) * turn
            } else {
                turn_of[i - 1L] * turn
            }
            t <- (edge - y[i - 1L]) / (y[i] - y[i - 1L])
            xb <- x[i - 1L] + t * (x[i] - x[i - 1L])
            cx <- c(cx, xb)
            cy <- c(cy, edge - turn_of[i - 1L] * turn)
            flush()
            cx <- c(xb, x[i])
            cy <- c(edge - turn_of[i] * turn, y[i] - turn_of[i] * turn)
        } else {
            cx <- c(cx, x[i])
            cy <- c(cy, y[i] - turn_of[i] * turn)
        }
    }
    flush()
    pieces
}

#' Sutherland-Hodgman clip of a polygon against one horizontal half-plane
#'
#' Returns `NULL` when nothing is left, so the caller can drop the piece.
#' @keywords internal
#' @noRd
clip_against <- function(x, y, bound, above = TRUE) {
    n <- length(x)
    if (n < 3L) return(NULL)
    keep <- if (above) y >= bound else y <= bound
    if (all(keep)) return(list(x = x, y = y))
    if (!any(keep)) return(NULL)
    ox <- numeric(n + sum(!keep) + 1L)
    oy <- numeric(length(ox))
    k <- 0L
    for (i in seq_len(n)) {
        j <- if (i == 1L) n else i - 1L
        if (keep[i]) {
            k <- k + 1L
            ox[k] <- x[i]; oy[k] <- y[i]
        }
        if (keep[i] != keep[j]) {
            # the edge crosses the boundary exactly once
            t <- (bound - y[j]) / (y[i] - y[j])
            k <- k + 1L
            ox[k] <- x[j] + t * (x[i] - x[j])
            oy[k] <- bound
        }
    }
    if (k < 3L) return(NULL)
    list(x = ox[seq_len(k)], y = oy[seq_len(k)])
}
