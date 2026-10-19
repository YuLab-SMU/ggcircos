#' Circular and arc-like coordinates
#'
#' `coord_circos()` draws the panel in polar coordinates: `x` is the radius,
#' `y` is the angle in radians. `coord_arc()` is the linear counterpart —
#' nodes sit on a line and links bulge to one side. `ggcircos()` installs
#' the right one for you; you rarely need to call these directly.
#'
#' The circular coordinate is built on [ggplot2::coord_radial()]: the radius
#' axis is suppressed (a radius has no physical meaning here), the angle is
#' taken from `y`, and `coord_circos()` maps its own arguments onto the
#' fields `CoordRadial` expects. Labels are rotated with the screen angle
#' the coordinate itself reports, so they stay attached to the circle for
#' any `clockwise` setting.
#'
#' @param theta Which aesthetic holds the angle. Only `"y"` is supported.
#' @param clockwise Direction of increasing `theta` on screen: `TRUE` puts
#'   `theta = 0` at 12 o'clock and runs clockwise, `FALSE` runs
#'   counter-clockwise.
#' @param start Angle (radians) of `theta = 0` on the ring, measured
#'   clockwise from 12 o'clock. `ggcircos()` passes the layout's `start`
#'   here, so the first node lands where the layout says it does.
#' @param clip Should the panel be clipped to its range? `"off"` by default
#'   so that bulging links are not cut off.
#' @param r.axis Draw the radial axis. `FALSE` by default: a radius has no
#'   physical meaning here.
#' @param xlim,ylim Panel ranges for `coord_arc()`.
#' @param direction Rejected on purpose: the `coord_polar()` spelling for
#'   `clockwise`. Present in the signature so that old code gets a classed
#'   error instead of "unused argument".
#' @param expand,limits,... Passed on to [ggplot2::coord_radial()] or
#'   [ggplot2::coord_cartesian()].
#'
#' @return A ggplot2 coordinate.
#' @family coordinates
#' @examples
#' # normally installed for you by ggcircos()
#' p <- ggplot2::ggplot(data.frame(x = 1, y = 0), ggplot2::aes(x, y)) +
#'     ggplot2::geom_point() +
#'     coord_circos()
#' p
#' @importFrom ggplot2 ggproto
#' @export
coord_circos <- function(theta = "y",
                         clockwise = TRUE,
                         start = -pi / 2,
                         clip = "off",
                         r.axis = FALSE,
                         expand = FALSE,
                         limits = NULL,
                         direction = NULL,
                         ...) {
    if (!identical(theta, "y")) {
        ggcircos_abort(
            "layout",
            "{.fn coord_circos} only supports {.code theta = \"y\"}, not {.val {theta}}."
        )
    }
    if (!is.null(direction)) {
        ggcircos_abort(
            "layout",
            c(
                "{.arg direction} is not an argument of {.fn coord_circos}.",
                i = "Use {.arg clockwise = TRUE}/{.arg FALSE}."
            )
        )
    }
    # CoordRadial draws theta = 0 at 12 o'clock when the arc starts at
    # -pi/2 and runs forward; a reversed circle is the same span read
    # backwards, which CoordRadial expresses as `reverse = "theta"`.
    arc <- if (isTRUE(clockwise)) {
        c(start, start + 2 * pi)
    } else {
        c(start + 2 * pi, start)
    }
    ggproto(NULL, CoordCircos,
        theta = theta,
        r = "x",
        arc = arc,
        reverse = if (isTRUE(clockwise)) "none" else "theta",
        r_axis = isTRUE(r.axis),
        expand = expand,
        limits = limits,
        clip = clip,
        # `inner.radius` must be a length-2 vector for CoordRadial's own
        # scale handling; the donut default leaves the middle free for
        # chords that reach the centre
        inner_radius = c(0, 0.4),
        # a reversed arc needs the radius scale read outwards in the same
        # order; CoordRadial warns about r.axis placement otherwise
        r_axis_inside = TRUE,
        # We compute label rotation ourselves (spec §5.4, `screen_angle()`),
        # and `rotate_angle = TRUE` would have ggplot2's `transform` apply its
        # own `flip_data_text_angle()` on top — which doubled the rotation and
        # turned `rotate = TRUE` into "horizontal" and vice versa.
        rotate_angle = FALSE
    )
}

#' @rdname coord_circos
#' @export
coord_arc <- function(xlim = c(-1, 1),
                      ylim = NULL,
                      clip = "off",
                      expand = FALSE,
                      ...) {
    ggproto(NULL, CoordArc,
        xlim = xlim,
        ylim = ylim,
        expand = expand,
        clip = clip
    )
}

#' @rdname coord_circos
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 CoordRadial ggproto
#' @export
CoordCircos <- ggproto("CoordCircos", CoordRadial,
    # a radius has no physical meaning here, so the radial axis is suppressed
    # unless asked for; ggplot2 still needs a well-formed (empty) axis back
    render_axis_v = function(self, panel_params, theme) {
        if (isTRUE(self$r_axis)) {
            return(ggplot2::ggproto_parent(CoordRadial, self)$render_axis_v(panel_params, theme))
        }
        empty_axis()
    },
    render_axis_h = function(self, panel_params, theme) {
        if (isTRUE(self$r_axis)) {
            return(ggplot2::ggproto_parent(CoordRadial, self)$render_axis_h(panel_params, theme))
        }
        empty_axis()
    },

    # The scale that y is drawn against spans [0, 2*pi] by construction in
    # ggcircos(); if a user replaces it, angles stop meaning angles.
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
        pp <- ggplot2::ggproto_parent(CoordRadial, self)$setup_panel_params(scale_x, scale_y, params)
        tr <- pp$theta.range
        if (!is.null(tr) && (max(tr, na.rm = TRUE) < 2 * pi || min(tr, na.rm = TRUE) > 0)) {
            cli::cli_warn(c(
                "The angle scale no longer spans {.code [0, 2*pi]}: labels and \\
                 slots will sit in the wrong place.",
                i = "Leave the y scale of a ggcircos plot alone; the layout \\
                    already fixed it to {.code c(0, 2*pi)}."
            ),
            class = "ggcircos_warning_scale")
        }
        pp
    },

    # Screen angle in degrees for a vector of layout angles, in the same
    # frame the coordinate draws them: 12 o'clock is 0, positive clockwise.
    #
    # This is the only sanctioned source of label rotation (spec §5.4).
    # Deriving it back from transformed coordinates (atan2) silently breaks
    # when the angle scale is replaced, when `expand` is non-zero, or when
    # the panel is not square; the panel parameters are what the transform
    # itself uses, so they cannot disagree with it.
    screen_angle = function(self, panel_params, theta) {
        arcframe <- panel_params$arc %||% self$arc %||% c(-pi / 2, 3 * pi / 2)
        tr <- panel_params$theta.range
        if (is.null(tr) || !is.finite(diff(tr))) {
            return(rep(0, length(theta)))
        }
        u <- (theta - tr[1]) / diff(tr)
        if (identical(self$reverse, "theta")) u <- 1 - u
        rad <- arcframe[1] + u * diff(arcframe)
        (rad * 180 / pi) %% 360
    },

    is_linear = function() FALSE
)

#' @keywords internal
#' @noRd
empty_axis <- function() {
    null <- grid::nullGrob()
    list(left = null, right = null, top = null, bottom = null)
}

#' @rdname coord_circos
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 CoordCartesian ggproto
#' @export
CoordArc <- ggproto("CoordArc", CoordCartesian,
    is_linear = function() TRUE,
    screen_angle = function(self, panel_params, theta) {
        rep(0, length(theta))
    }
)

# the screen angle of a label, asked of the coordinate itself
#
# Geoms never compute their own rotation: they call this and get back the
# degrees the coordinate will draw `theta` at. A coordinate without the
# method (a user's own) gets the old formula-free fallback of "no rotation".
# @keywords internal
# @noRd
circos_screen_angle <- function(coord, panel_params, theta) {
    if (!is.null(coord$screen_angle)) {
        return(coord$screen_angle(panel_params, theta))
    }
    rep(0, length(theta))
}
