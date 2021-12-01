#' the node label layer of circos
#' @param data input data
#' @param mapping mapping aesthetics
#' @param hjust horizontal adjustment to nudge labels
#' @param offset Horizontal adjustment to nudge labels by.
#' @param ... additional parameters.
#' @export
geom_link_lab <- function(
    data = NULL,
    mapping = NULL,
    hjust = 0, 
    offset = 0, 
    #as_ylab = FALSE, 
    ...){
    params <- list(...)
    structure(list(
                   mapping = mapping,
                   hjust = hjust,
                   offset = offset,
                   #as_ylab = as_ylab,
                   params = params
                   ),
               class = "link_lab"
    )

}
