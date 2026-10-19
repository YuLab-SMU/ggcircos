#' Blank theme for circular layouts
#'
#' The default theme of a circular plot: no grid, no axes, sensible margins
#' and a legend on the right. Layer on top of it with `+ theme()` as usual.
#'
#' @param bgcolor Panel background colour. The panel is *painted*, not left
#'   transparent, so that `bgcolor = "white"` really is white — `theme_void()`
#'   blanks the elements, it does not fill them.
#' @param grid Keep the panel grid? With a circular coordinate these are the
#'   polar elements `panel.grid.*.theta` / `panel.grid.*.r`, not cartesian
#'   `.x` / `.y`.
#' @param axis Keep the axes? With a circular coordinate these are
#'   `axis.text.theta`, `axis.line.theta`, `axis.ticks.theta`.
#' @param legend.position Legend placement, passed to [ggplot2::theme()].
#' @param margin Plot margin, in points.
#'
#' @return A ggplot2 theme.
#' @examples
#' ggcircos(cor(mtcars), order = "hclust") +
#'     geom_chord() +
#'     theme_circos(bgcolor = "grey95")
#' @importFrom ggplot2 theme element_blank element_rect margin
#' @export
theme_circos <- function(bgcolor = "white",
                         grid = FALSE,
                         axis = FALSE,
                         legend.position = "right",
                         margin = 10) {
    th <- theme(
        plot.background = element_rect(fill = bgcolor, colour = NA),
        plot.margin = margin(margin, margin, margin, margin)
    )
    # ggplot2 has no `panel.grid.*.theta` / `.r` element in its hierarchy —
    # the radial coordinate builds those lines itself in `render_bg()`, and
    # they are null because the scales have no breaks. So there is nothing
    # to blank: only the cartesian names are, so that a coordinate swap
    # cannot surface a cartesian grid.
    th <- th + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )
    if (isTRUE(grid)) {
        th <- th + theme(
            axis.text.theta = ggplot2::element_text(),
            axis.ticks.theta = ggplot2::element_line(),
            axis.line.theta = ggplot2::element_line()
        )
    }
    # There is no `axis.title.theta` in ggplot2, so it is not blanked here.
    th <- th + theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.theta = element_blank(),
        axis.ticks.theta = element_blank(),
        axis.line.theta = element_blank()
    )
    if (isTRUE(axis)) {
        th <- th + theme(
            axis.title = ggplot2::element_text(),
            axis.text = ggplot2::element_text(),
            axis.ticks = ggplot2::element_line(),
            axis.line = ggplot2::element_line()
        )
    }
    # painted here, not through theme_void(), which blanks the element
    # instead of filling it
    th + theme(
        panel.background = element_rect(fill = bgcolor, colour = NA),
        legend.position = legend.position
    )
}
