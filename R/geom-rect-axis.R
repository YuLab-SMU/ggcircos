geom_rect_axis <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           radius = 10,
                           width = NULL,
                           breakdist = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRectAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      radius = radius,
      width = width,
      breakdist = breakdist,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 resolution Geom draw_key_polygon GeomPolygon
#' @importFrom grid rectGrob grobTree gpar
#' @export
GeomRectAxis <- ggproto("GeomRectAxis", ggplot2::Geom,
  default_aes = aes(colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA, radius=10),
  required_aes = c("y"),
  setup_data = function(data, params) {
      width <- params$width
      if (is.null(width)) width <- 1/3
      data$x <- params$radius
      #data <- data[order(data$y),,drop=FALSE]
      if (is.null(params$breakdist)){
          params$breakdist <- 0.1 * sum(data$y)
      }
      print(params$breakdist)
      data <- pos_stackrect(df=data, breakdist=params$breakdist)
      print(data)
      data <- transform(data, xmin = x - width / 2, xmax = x + width / 2)
      print(data)
  },
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", width=NULL, breakdist=NULL) {
    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )

      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- new_data_frame(row[aesthetics])[rep(1,5), ]

        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggname("bar", do.call("grobTree", polys))
    } else {
      coords <- coord$transform(data, panel_params)
      print (coords)
      ggname("geom_rect", rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = gpar(
          col = coords$colour,
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          linejoin = linejoin,
          # `lineend` is a workaround for Windows and intentionally kept unexposed
          # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
          lineend = if (identical(linejoin, "round")) "round" else "square"
        )
      ))
    }
  },
  draw_key = ggplot2::draw_key_polygon
)


# Convert rectangle to polygon
# Useful for non-Cartesian coordinate systems where it's easy to work purely in
# terms of locations, rather than locations and dimensions. Note that, though
# `polygonGrob()` expects an open form, closed form is needed for correct
# munching (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-458406857).
#
# @keyword internal
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
}


pos_stackrect <- function(df, breakdist=NULL, fill = FALSE) {
  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  heights <- c(0, cumsum(y))
  if (fill) {
      heights <- heights / abs(heights[length(heights)])
  }
  # We need to preserve ymin/ymax order. If ymax is lower than ymin in input, it should remain that way
  if (!is.null(df$ymin) && !is.null(df$ymax)) {
      max_is_lower <- df$ymax < df$ymin
  } else {
      max_is_lower <- rep(FALSE, nrow(df))
  }
  ymin <- pmin(heights[-n], heights[-1])
  ymax <- pmax(heights[-n], heights[-1])
  #df$y <- (1 - vjust) * ymin + vjust * ymax
  df$ymin <- ifelse(max_is_lower, ymax, ymin)
  df$ymax <- ifelse(max_is_lower, ymin, ymax)
  if(!is.null(breakdist)){
      df$ymin <- seq_along(df$ymin) * breakdist + df$ymin
      print(df$ymin)
      df$ymax <- seq_along(df$ymax) * breakdist + df$ymax
  }
  df
}
