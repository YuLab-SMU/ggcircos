#' @importFrom ggplot2 theme element_blank element_rect
theme_blank <- function(bgcolor = "white"){
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = bgcolor, colour = bgcolor),
      NULL)
}
