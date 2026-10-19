#' Node layers
#'
#' Layers for the node ring itself: the arc, a label, a point. They read the
#' node table of the plot's layout and only ever add their own data — the
#' layout stays read-only.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,na.rm,...
#'   Standard [ggplot2::layer()] arguments.
#' @param width Width of the node arc (ribbon type) or of its gap
#'   (`geom_node_text`), in radius units.
#' @param offset Distance between the node ring and the layer, in radius
#'   units.
#' @param linewidth Border width of the node arc.
#' @param rotate Rotate the label to follow the circle. The rotation is
#'   derived from where the coordinate actually puts the label, so it stays
#'   correct for any `start` or `clockwise`.
#' @param nudge_x Radial nudge applied to the label.
#'
#' @return A ggplot2 layer.
#' @section Label placement:
#' `rotate = FALSE` keeps labels horizontal and flips `hjust` on the left half
#' of the circle. `rotate = TRUE` rotates them, flipping both the angle and
#' `hjust` on the left half. `geom_node_text()` does not take an `angle`
#' aesthetic — use `rotate`, so the two can never disagree.
#'
#' @examples
#' ggcircos(cor(mtcars), order = "hclust") +
#'     geom_node_arc() +
#'     geom_node_text()
#' @name geom_node
NULL

#' @rdname geom_node
#' @importFrom ggplot2 layer
#' @export
geom_node_arc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          width = 0.04,
                          offset = 0.01,
                          linewidth = 0.2,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomNodeArc,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                linewidth = linewidth, na.rm = na.rm, ...
            )
        ),
        "ggcircos_node",
        list(width = width, offset = offset)
    )
}

#' @rdname geom_node
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomRect
#' @export
GeomNodeArc <- ggproto("GeomNodeArc", GeomRect,
    default_aes = ggplot2::aes(
        fill = "grey30", colour = NA, linewidth = 0.2,
        linetype = 1, alpha = NA
    ),
    required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax")
    # `fill` defaults to a constant so that a chord layer can own the fill
    # scale; map aes(fill = sector) when the arcs should be coloured.
)

#' @rdname geom_node
#' @importFrom ggplot2 layer
#' @export
geom_node_text <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           rotate = FALSE,
                           offset = 0.06,
                           nudge_x = 0,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = FALSE) {
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomNodeText,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        ),
        "ggcircos_node",
        list(rotate = rotate, offset = offset, nudge_x = nudge_x)
    )
}

#' @rdname geom_node
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomText
#' @export
GeomNodeText <- ggproto("GeomNodeText", GeomText,
    default_aes = ggplot2::aes(
        colour = "black", size = 3.88, angle = 0, hjust = 0.5,
        vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
    ),
    required_aes = c("x", "y", "label"),
    optional_aes = c("size", "angle", "hjust", "vjust", "colour", "alpha", "family", "fontface", "lineheight"),
    draw_panel = function(self, data, panel_params, coord, rotate = FALSE) {
        if (nrow(data) == 0L) return(grid::nullGrob())
        angle <- circos_label_angle(coord, panel_params, data$y, rotate)
        data$angle <- angle$angle
        data$hjust <- angle$hjust
        ggplot2::ggproto_parent(GeomText, self)$draw_panel(data, panel_params, coord)
    }
)

#' @rdname geom_node
#' @importFrom ggplot2 layer
#' @export
geom_node_point <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            offset = 0.02,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomPoint,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        ),
        "ggcircos_node",
        list(offset = offset)
    )
}

#' Label angle and justification from the coordinate
#'
#' The screen angle is asked of the coordinate (spec §5.4), never derived
#' back from transformed coordinates: the coordinate uses the same panel
#' parameters as `transform()`, so the two cannot drift apart.
#' @keywords internal
#' @noRd
circos_label_angle <- function(coord, panel_params, theta, rotate) {
    screen <- circos_screen_angle(coord, panel_params, theta)
    left <- cos(screen * pi / 180) < 0
    if (rotate) {
        angle <- ifelse(left, screen + 180, screen)
        list(angle = angle, hjust = ifelse(left, 1, 0))
    } else {
        list(angle = rep(0, length(screen)), hjust = ifelse(left, 1, 0))
    }
}

#' Sector labels
#'
#' One label per sector, placed at the sector's mid-angle just outside the
#' node ring.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,na.rm,...
#'   Standard [ggplot2::layer()] arguments.
#' @param offset Distance outside the node ring.
#' @param angle `"horizontal"` keeps the label upright and flips `hjust` on
#'   the left half; `"radial"` rotates it with the circle; a number is used
#'   as given.
#'
#' @return A ggplot2 layer.
#' @examples
#' nodes <- data.frame(
#'     id = c("a", "b", "c", "d"),
#'     genome = c("ref", "ref", "qry", "qry"),
#'     stringsAsFactors = FALSE
#' )
#' links <- data.frame(
#'     from = c("a", "c"), to = c("d", "b"),
#'     stringsAsFactors = FALSE
#' )
#' ggcircos(links, nodes = nodes, sectors = "genome") +
#'     geom_chord() +
#'     geom_sector_label()
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 GeomText
#' @export
geom_sector_label <- function(mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              ...,
                              offset = 0.06,
                              angle = "horizontal",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = FALSE) {
    tag_layer(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomSectorLabel,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        ),
        "ggcircos_node",
        list(offset = offset, angle = angle)
    )
}

#' @rdname geom_sector_label
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomText
#' @export
GeomSectorLabel <- ggproto("GeomSectorLabel", GeomText,
    default_aes = ggplot2::aes(
        colour = "black", size = 3.88, angle = 0, hjust = 0.5,
        vjust = 0.5, alpha = NA, family = "", fontface = "bold", lineheight = 1.2
    ),
    required_aes = c("x", "y", "label"),
    optional_aes = c("size", "angle", "hjust", "vjust", "colour", "alpha", "family", "fontface", "lineheight"),
    draw_panel = function(self, data, panel_params, coord, angle = "horizontal") {
        if (nrow(data) == 0L) return(grid::nullGrob())
        if (identical(angle, "horizontal")) {
            ang <- circos_label_angle(coord, panel_params, data$y, FALSE)
            data$angle <- ang$angle
            data$hjust <- ang$hjust
        } else if (identical(angle, "radial")) {
            ang <- circos_label_angle(coord, panel_params, data$y, TRUE)
            data$angle <- ang$angle
            data$hjust <- ang$hjust
        } else {
            data$angle <- angle
        }
        ggplot2::ggproto_parent(GeomText, self)$draw_panel(data, panel_params, coord)
    }
)