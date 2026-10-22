#' Circular layouts, chord diagrams and tracks for ggplot2
#'
#' The layout of a circular plot -- node arcs, sectors and the angular slot
#' reserved for every link -- is computed once by [circos_layout()] and kept
#' as data. Layers then behave like any other ggplot2 layer: they read the
#' layout and add their own data, and nothing accumulates on the plot.
#'
#' @keywords internal
#' @importFrom ggplot2 ggproto aes ggplot aes layer
#' @importFrom ggplot2 Geom Stat Coord Position
#' @importFrom ggplot2 GeomRect GeomTile GeomPoint GeomText GeomCrossbar
#' @importFrom ggplot2 CoordRadial CoordCartesian
#' @importFrom ggplot2 draw_key_path draw_key_blank
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme element_blank element_rect margin
#' @importFrom grid gTree gpar pathGrob polylineGrob nullGrob gList
#' @importFrom scales alpha squish
#' @importFrom stats complete.cases median IQR as.dist hclust
#' @importFrom utils head tail
#' @importFrom rlang enquo as_name sym quo_get_expr quo_is_null eval_tidy
#' @importFrom cli cli_warn
"_PACKAGE"

#' @importFrom rlang .data
#' @export
rlang::.data

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes
