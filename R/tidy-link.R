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
#' @method tidy_link tbl_lk
#' @export
tidy_link.tbl_lk <- function(x, radius = 1, ...){
    edge.tbl <- attr(x, "edges")
    attr(x, "edges") <- NULL
    x %<>%
        drop_class(class = c("tbl_lk", "tbl_node")) %>%
        dplyr::mutate(x = radius, y = .data$node) %>%
        calculate_angle()
    edge.tbl %<>%
        dplyr::mutate(x = radius, xend = radius) %>%
        dplyr::filter(.data$from != .data$to) %>%
        drop_class(class="tbl_edge") %>%
        add_attr(y=x, attrobj="nodes")
    edge.tbl
}


#' tl_extract
#' @param name character name of attributes
#' @param .f function
#' @export
tl_extract <- function(name, .f = NULL){
    function(.data) {
        name <- rlang::enquo(name)
        .data %<>% attr(rlang::as_name(name))
        if (!is.null(.f)){
            .data <- .f(.data)
        }
        return(.data)
    }
}
