#' @title Line curves in coordinate polar y
#' @description
#' `geom_curve_link` draw a curve line between points (x,y) and (xend, yend) in
#' coordinate polar (y). 
#' the curvature parameter will be calculated in internal (default). You also 
#' can set it by manually, we don't recommend to set it by manually.
#' @inheritParams ggplot2::layer
#' @inheritParams grid::curveGrob
#' @param arrow specification for arrow heads, as created by arrow().
#' @param arrow.fill fill colour to use for the arrow head (if closed). `NULL`
#'        means use `colour` aesthetic.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param na.rm logical, If `FALSE`, the default, missing values are removed with
#' a warning. If `TRUE`, missing values are silently removed.
#' @importFrom ggplot2 layer
#' @author Shuangbin Xu
#' @export
geom_curve_link <- function(mapping = NULL, 
                            data = NULL,
                            stat = "identity", 
                            position = "identity",
                            ...,
                            arrow = NULL,
                            arrow.fill = NULL,
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurveLink,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomCurveLink
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom rlang abort
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_path
#' @importFrom dplyr rename
#' @importFrom grid curveGrob gpar gTree
#' @importFrom scales alpha
#' @author Shuangbin Xu
#' @export
GeomCurveLink <- ggproto("GeomCurveLink", Geom,
    required_aes = c("x", "y", "xend", "yend"),
    non_missing_aes = c("linetype", "size", "shape"),
    optional_aes = c("subset"),
    default_aes = aes(colour = "black", size = 0.3, linetype = 1, alpha = 0.4, curvature=NA, curveangle=90, square=FALSE),
    draw_key = draw_key_path,
    setup_data = function(data, params){
        if (!is.null(data$subset)){
            data <- data[which(data$subset),,drop=FALSE]
        }
        data
    },
    draw_panel = function(data, panel_params, coord, shape = 0.5, arrow = NULL,
                          hratio = .8, ncp = 1, arrow.fill = NULL, lineend = "butt", 
                          direction = 'left', na.rm = FALSE) {
      direction <- match.arg(direction, c("left", "right"))
      if (!coord$is_linear()){
          tmpgroup <- data$group
          starts <- subset(data, select = c(-xend, -yend))
          starts$group <- 1
          ends <- rename(subset(data, select = c(-x, -y)), c("x" = "xend", "y" = "yend"))
          ends$group <- 2
          pieces <- rbind(starts, ends)
          
          trans <- coord$transform(pieces, panel_params)
          starts <- trans[trans$group==1, ,drop=FALSE]
          ends <- trans[trans$group==2, ,drop=FALSE]
          if (all(is.na(trans$curvature))){
              curvature <- unlist(mapply(build_curvature, 
                                         starttheta=starts$theta, 
                                         endtheta=ends$theta, 
                                         MoreArgs=list(hratio = hratio, ncp = ncp),
                                         SIMPLIFY=FALSE))
          }else{
              curvature <- trans$curvature
          }
          ends <- rename(subset(ends, select=c(x, y)), c("xend"="x", "yend"="y"))
          trans <- cbind(starts, ends)
          trans$group <- tmpgroup
          trans$curvature <- curvature
      }else{
          trans <- coord$transform(data, panel_params)
          logicflag <- trans$y < trans$yend
          if (all(is.na(trans$curvature))){
              if (direction == 'left'){
                  trans$curvature <- ifelse(logicflag, -1, 1) * abs(hratio)
              }else if (direction == 'right'){
                  trans$curvature <- ifelse(logicflag, 1, -1) * abs(hratio)
              }
          }else{
              if (direction == 'left'){
                  trans$curvature <- ifelse(logicflag, -1, 1) * abs(trans$curvature)
              }else if (direction == 'right'){
                  trans$curvature <- ifelse(logicflag, 1, -1) * abs(trans$curvature)
              }
          }
          
      }
      arrow.fill <- arrow.fill %|||% trans$colour
      grobs <- lapply(seq_len(nrow(trans)), function(i){
                          curveGrob(
                            trans$x[i], trans$y[i], trans$xend[i], trans$yend[i],
                            default.units = "native",
                            curvature = trans$curvature[i], angle = trans$curveangle[i], ncp = ncp,
                            square = trans$square[i], squareShape = 1, inflect = FALSE, open = TRUE,
                            gp = gpar(col = alpha(trans$colour[i], trans$alpha[i]),
                                      fill = alpha(arrow.fill[i], trans$alpha[i]),
                                      lwd = trans$size[i] * ggplot2::.pt,
                                      lty = trans$linetype[i],
                                      lineend = lineend),
                            arrow = arrow)})
      class(grobs) <- "gList"
      ggname("geom_curve_link", gTree(children=grobs))
    }
)

