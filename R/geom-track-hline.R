#' A threshold line inside a track
#'
#' `geom_hline()` draws at `yintercept` on the *angle* axis, because a track's
#' `y` is an angle (spec sec.4.2). For a threshold in the band's own units use
#' this instead: `value` goes through the same normalisation the track's values
#' went through, so the line and the data always share one scale.
#'
#' The line is drawn per node -- one arc inside each node's own span -- so it
#' reads as "this node's threshold" rather than a ring passing through the gaps.
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes Standard
#'   [ggplot2::layer()] arguments. `data` is optional; the nodes come from the
#'   plot's layout.
#' @param band The track to draw in: a declared band name, or `c(r0, r1)`.
#'   Defaults to the band of a track layer already on the plot.
#' @param value Threshold in the track's **value** units, not radii. A vector
#'   draws several lines.
#' @param linetype,linewidth,colour Appearance of the line.
#' @param ... Passed to [ggplot2::layer()], for further line settings.
#' @return A ggplot2 layer.
#' @seealso [geom_track()], [geom_track_axis()]
#' @examples
#' links <- data.frame(from = c("a", "b"), to = c("b", "c"))
#' ggcircos(links, tracks = c(v = 0.2)) +
#'   geom_track(data = data.frame(id = c("a", "b", "c"), v = c(0.2, 0.5, 0.8)),
#'              aes(x = v), node = id, band = "v") +
#'   geom_track_hline(band = "v", value = 0.5)
#' @export
geom_track_hline <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             band = NULL,
                             value,
                             linetype = "dashed",
                             linewidth = 0.4,
                             colour = "grey30",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = FALSE) {
    if (missing(value)) {
        ggcircos_abort(
            "data",
            "{.fn geom_track_hline} needs a {.arg value}: a threshold in the \\
             track's own units."
        )
    }
    if (!is.numeric(value) || !length(value) || anyNA(value)) {
        ggcircos_abort("data", "{.arg value} must be one or more finite numbers.")
    }
    band_name <- if (is.character(band) && length(band) == 1L) band else NULL
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomTrackHline,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                linetype = linetype,
                linewidth = linewidth,
                colour = colour,
                ...
            )
        ),
        "ggcircos_hline",
        # `value` is not an aesthetic and the geom never sees it: `ggplot_add`
        # turns it into radii and writes the result as data
        list(band = band, band_name = band_name, value = as.numeric(value))
    )
}

#' The threshold line itself: plain segments, placed by `ggplot_add`
#'
#' Nothing is computed here -- every endpoint arrives as data -- so this is
#' `GeomSegment` under a name that says what it is for.
#' @keywords internal
#' @noRd
GeomTrackHline <- ggplot2::GeomSegment
