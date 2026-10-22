#' Circular and arc-like coordinates
#'
#' `coord_circos()` draws the panel in polar coordinates: `x` is the radius,
#' `y` is the angle in radians. `coord_arc()` is the linear counterpart --
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
#'
#' @details `coord_arc()` locks the radius unit to the angle unit
#'   (`ratio = 1`), because the linear layout has no circle to borrow a scale
#'   from: without it a chord spanning half a turn is drawn as a tall loop
#'   instead of an arc. The angle axis covers a whole turn, so this layout
#'   wants a **tall** device (roughly 1:2 or narrower); in a square device
#'   the panel becomes a narrow column.
#' @param direction Rejected on purpose: the `coord_polar()` spelling for
#'   `clockwise`. Present in the signature so that old code gets a classed
#'   error instead of "unused argument".
#' @param expand,limits,... Passed on to [ggplot2::coord_radial()] or
#'   [ggplot2::coord_cartesian()]. A plain numeric `limits` is read as the
#'   *angle* range: `coord_circos(limits = c(0, 2 * pi))` says "the angle
#'   axis is the whole circle", which is what a hand-built plot needs
#'   (see `vignette("from-circlize")` and the layout-as-data section of
#'   `vignette("tracks")`).
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
                         expand = c(FALSE, TRUE, FALSE, FALSE),
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
    # a plain numeric `limits` means the angle range: the radius axis has no
    # physical meaning here, and `coord_circos(limits = c(0, 2*pi))` is how
    # a hand-built plot says "the angle axis is the whole circle"
    if (is.numeric(limits)) {
        limits <- list(theta = limits)
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
        # We compute label rotation ourselves (spec sec.5.4, `screen_angle()`),
        # and `rotate_angle = TRUE` would have ggplot2's `transform` apply its
        # own `flip_data_text_angle()` on top -- which doubled the rotation and
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
        clip = clip,
        # one unit of radius must be one unit of angle, or a chord is drawn
        # as a vertically stretched loop instead of an arc: the y axis spans
        # a whole turn while x spans about one radius (R17)
        ratio = 1
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
    # ggcircos(); if a user replaces it, angles stop meaning angles. A
    # little slack is allowed: a hand-built plot whose y scale was trained
    # on the layout's own angles lands a fraction of a radian inside the
    # circle, and that is fine.
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
        pp <- ggplot2::ggproto_parent(CoordRadial, self)$setup_panel_params(scale_x, scale_y, params)
        tr <- pp$theta.range
        tol <- 0.02 * 2 * pi
        if (!is.null(tr) && (max(tr, na.rm = TRUE) < 2 * pi - tol ||
                             min(tr, na.rm = TRUE) > tol)) {
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
    # This is the only sanctioned source of label rotation (spec sec.5.4).
    # Deriving it back from transformed coordinates (atan2) silently breaks
    # when the angle scale is replaced, when `expand` is non-zero, or when
    # the panel is not square; the panel parameters are what the transform
    # itself uses, so they cannot disagree with it.
    # The *radial* angle of a theta: the direction from the centre outward,
    # in degrees, 12 o'clock = 0, positive clockwise. That is the angle a
    # label outside the ring reads along, and it is the quantity
    # `transform()` itself puts a point at, so the two cannot disagree.
    #
    # A point at radius r sits at (r*sin(a) + .5, r*cos(a) + .5) with `a`
    # this radial angle, which is why the answer is the arc position and not
    # a tangent: a label is drawn radially, never along the circumference.
    screen_angle = function(self, panel_params, theta) {
        arcframe <- panel_params$arc %||% self$arc %||% c(-pi / 2, 3 * pi / 2)
        tr <- panel_params$theta.range
        if (is.null(tr) || !is.finite(diff(tr))) {
            return(rep(0, length(theta)))
        }
        u <- (theta - tr[1]) / diff(tr)
        # `arc` already carries the direction: a counter-clockwise circle is
        # arc = c(start + 2*pi, start), i.e. a *decreasing* span. Flipping `u`
        # again for `reverse == "theta"` cancelled that out and made a
        # reversed circle report the same angles as a forward one -- rotated
        # labels came out mirrored (review R2).
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
CoordArc <- ggproto("CoordArc", ggplot2::CoordFixed,
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
