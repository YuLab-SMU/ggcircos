#' @importFrom pillar tbl_sum
#' @method tbl_sum tbl_edge
#' @export
tbl_sum.tbl_edge <- function(x, ...){
    header <- NextMethod()
    names(header) <- "An Edge tibble"
    header
}


#' @method tbl_sum tbl_node
#' @export
tbl_sum.tbl_node <- function(x, ...){
    header <- NextMethod()
    names(header) <- "A Node tibble"
    header
}

#' @method print tbl_lk
#' @export
print.tbl_lk <- function(x, ..., n = 6, width = NULL, n_extra = NULL){
    edge.tbl <- attr(x, "edges")
    x <- drop_class(x, "tbl_lk")
    print_subtle("# A tbl_lk abstraction: ", nrow(x), " nodes and ", nrow(edge.tbl), " edges\n", sep="\n")
    print(x, ..., n = n, width = width, n_extra = n_extra)
    print_subtle("#\n")
    print_subtle("# With the available attributes: \n#\n# ",  extract_external_attr_name(x), "\n")
    print_subtle("#\n")
    print(edge.tbl, ..., n = n, width = width, n_extra = n_extra)
}


print_subtle <- function(...){
    cat(pillar::style_subtle(paste0(...)))
}

drop_class <- function(x, class){
    old <- class(x)
    class(x) <- old[!old %in% class]
    return(x)
}

add_class <- function(x, class){
    old <- class(x)
    if (!class %in% old){
        class(x) <- c(class, old)
    }
    return (x)
}

add_attr <- function(x, y, attrobj){
    attr(x, attrobj) <- y
    x
}

calculate_angle <- function(data){
    data$angle <- 360/(diff(range(data$y)) + 1) * (data$y)
    return(data)
}

extract_external_attr_name <- function(x){
    xx <- names(attributes(x))
    paste0(xx[!xx %in% c("class", "row.names", "names")], collapse=", ")
}

#' @importFrom utils globalVariables
globalVariables(".")

#' @importFrom utils getFromNamespace
node_tibble <- getFromNamespace("node_tibble", "tidygraph")

edge_tibble <- getFromNamespace("edge_tibble", "tidygraph")
