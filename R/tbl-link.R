#' convert to tbl_lk class
#' @param x object
#' @param ... additional parameters
#' @export
as.tbl_lk <- function(x, ...){
    UseMethod("as.tbl_lk")
}

#' @importFrom corrr stretch as_cordf
#' @importFrom tibble tibble
#' @method as.tbl_lk matrix
#' @export
as.tbl_lk.matrix <- function(x, ...){
    flag <- identical(sort(rownames(x)), sort(colnames(x)))
    if (!flag){
        rlang::abort("The rownames and colnames of matrix should be identical")
    }

    x <- x[match(rownames(x),colnames(x)),,drop=FALSE]
    dat <- x %>% 
       as_cordf() %>%
       stretch() %>%
       dplyr::rename(from="x", to = "y") %>%
       dplyr::mutate(edge = seq_len(nrow(.)))

    name <- dat %>%
        dplyr::select("from", "to") %>%
        as.matrix() %>%
        as.vector() %>%
        unique()
    df <- tibble(node = seq_len(length(name)), label = name) %>%
          add_class(class="tbl_node")
    
    dat %<>%
       dplyr::mutate(from = do.call(dplyr::recode,
                                    c(.x=rlang::sym("from"),
                                      df %>%
                                        dplyr::pull(.data$node, name=.data$label) %>%
                                        as.list())
                            )
       ) %>%
       dplyr::mutate(to = do.call(dplyr::recode,
                                  c(.x=rlang::sym("to"),
                                    df %>%
                                      dplyr::pull(.data$node, name=.data$label) %>%
                                      as.list())
                            )
       ) %>%
       add_class(class="tbl_edge")

    df %<>% add_attr(y=dat, attrobj="edges") %>% 
       add_class(class = "tbl_lk")

    return (df)
}

#' @method as.tbl_lk data.frame
#' @export
as.tbl_lk.data.frame <- function(x, ...){
    x <- as.matrix(x)
    x <- as.tbl_lk(x)
    return(x)
}



