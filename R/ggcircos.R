#' visualizing relationship data and heterogenous associated data based on grammar of graphics
#' @param x data
#' @param mapping aesthetic mapping 
#' @param layout character, 'linear' or 'circular'
#' @param radius numeric the radius of the circular layout
#' @param ncp numeric
#' @param hratio numeric
#' @param ... additional parameter
#' @importFrom ggplot2 aes_ ggplot coord_polar scale_y_continuous
#' @importFrom utils modifyList
#' @export
ggcircos <- function(x, mapping = NULL, layout = "circular", radius = 1, ncp = 4, hratio= 0.5, ...){
    layout <- match.arg(layout, c("linear", "circular"))

    if (is.null(mapping)){
        mapping <- aes_(x = ~x, y= ~from, xend = ~xend, yend = ~to)
    }else{
        mapping <- modifyList(aes_(x = ~x, y= ~from, xend = ~xend, yend = ~to), mapping)
    }
    p <- ggplot(
           data = tidy_link(x, radius = radius, ...)
         ) +
         geom_curve_link(mapping = mapping, ncp = ncp, hratio = hratio, ...) +
         theme_blank()
    if (layout %in% c("circular")){
        p <- p +
             coord_polar(theta = "y", start = -pi/2, -1, clip = "off") +
             scale_y_continuous(limits = c(0, NA))
    }
    assign("layout", layout, envir = p$plot_env)
    return(p)
}
