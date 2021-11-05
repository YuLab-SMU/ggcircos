#' tidy link
#' @param x object 
#' @param radius numeric
#' @param ... additional parameters
#' @export
tidy_link <- function(x, radius = 1, ...){
    UseMethod("tidy_link")
}

#' @method tidy_link default
#' @export
tidy_link.default <- function(x, radius = 1, ...){
    x
}

#' @importFrom rlang .data
#' @method tidy_link tbl_link
#' @export
tidy_link.tbl_link <- function(x, radius = 1, ...){
    edge.tbl <- attr(x, "edges")
    attr(x, "edges") <- NULL
    x %<>%
        drop_class(class = c("tbl_link", "tbl_node")) %>%
        dplyr::mutate(x = .data$radius, y = .data$node) %>%
        calculate_angle()
    edge.tbl %<>%
        dplyr::mutate(x = .data$radius, xend = .data$radius) %>%
        dplyr::filter(.data$from != .data$to) %>%
        drop_class(class="tbl_edge") %>%
        add_attr(y=x, class="nodes")
    edge.tbl
}
