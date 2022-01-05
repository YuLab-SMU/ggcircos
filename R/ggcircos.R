#' visualizing relationship data and heterogenous associated data based on grammar of graphics
#' @param x data
#' @param mapping aesthetic mapping 
#' @param layout character, 'linear' or 'circular'
#' @param radius numeric the radius of the circular layout
#' @param ncp numeric
#' @param hratio numeric
#' @param alpha numeric
#' @param ... additional parameter
#' @importFrom ggplot2 aes_ ggplot coord_polar scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom utils modifyList
#' @export
#' @examples
#' data(mtcars)
#' tbl <- mtcars %>% cor() %>% as.tbl_lk()  
#' p1 <- ggcircos(tbl)
#' p2 <- ggcircos(tbl, layout = 'linear')
#' p3 <- ggcircos(tbl, layout = 'linear', direction = 'right')
ggcircos <- function(x, mapping = NULL, layout = "circular", radius = 1, ncp = 5, hratio= .8, alpha=.4, ...){
    layout <- match.arg(layout, c("linear", "circular"))

    if (is.null(mapping)){
        mapping <- aes_(x = ~x, y= ~from, xend = ~xend, yend = ~to)
    }else{
        mapping <- modifyList(aes_(x = ~x, y= ~from, xend = ~xend, yend = ~to), mapping)
    }
    p <- ggplot(
           data = tidy_link(x, radius = radius, ...)
         ) +
         geom_curve_link(mapping = mapping, ncp = ncp, hratio = hratio, alpha = alpha, ...) +
         theme_blank()
    if (layout %in% c("circular")){
        p <- p +
             coord_polar(theta = "y", start = -pi/2, -1, clip = "off") +
             scale_y_continuous(expand = c(0, 0.6, 0, 0.2)) + 
             scale_x_continuous(expand = c(0, 0.6, 0, 0.01))
    }else if (layout == 'linear'){
        p <- p + 
             scale_x_continuous(
               expand = c(0, 0.1, 0, .1), 
               #limits = c(0, NA)
             )
    }
    assign("layout", layout, envir = p$plot_env)
    return(p)
}
