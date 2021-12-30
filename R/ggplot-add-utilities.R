check_plotdata_ <- function(object, plot){
    if (is.null(object$data)){
        object$mapping <- modifyList(object$mapping, aes_(y=~y))
        object$data <- tl_extract(name = !!rlang::sym("nodes"))(plot$data)
        object$datanull <- TRUE
    }else if (is.function(object$data)){
        dat <- tl_extract(name = !!rlang::sym("nodes"))(plot$data)
        object$data <- object$data(dat)
        if (!is.data.frame(object$data)){
            abort("Data function must return a data.frame")
        }
        object$mapping <- modifyList(object$mapping, aes_(y=~y))
        object$datanull <- TRUE
    }else{
        object$datanull <- FALSE
    }
    return (object)
}

check_subset_aes_ <- function(object){
    if (!is.null(object$mapping$subset)){
        object$data <- dplyr::filter(object$data, !!object$mapping$subset)
        object$mapping <- object$mapping[names(object$mapping)!="subset"]
    }
    return(object)
}

#' @importFrom rlang as_name
build_new_data_ <- function(object, plot){
    if (inherits(object$data, "data.frame") && !object$datanull){
        origindata <- tl_extract(name = !!rlang::sym("node"))(plot$data)
        commonnames <- intersect(colnames(object$data), colnames(origindata))
        commonnames <- commonnames[commonnames!=as_name(object$mapping$y)]
        if (length(commonnames) > 0){
            warning_wrap("The following column names/name: ", paste0(commonnames, collapse=", "),
                         " are/is the same to tree data, the tree data column names are : ",
                         paste0(colnames(origindata), collapse=", "), ".")
        }
        object$data <- dplyr::left_join(
                         origindata, 
                         object$data, 
                         by=c("label"=as_name(object$mapping$y)), 
                         suffix = c("", ".y")
                       )
        object$mapping <- modifyList(object$mapping, aes_(y=~y))
    }
    object$datanull <- NULL
    return(object)
}


set_mapping_ <- function(object){
    if ("x" %in% names(object$mapping)){
        xid <- as_name(object$mapping$x)
        if (xid == "x"){
            object$data[["xtmp"]] <- object$data$x
            xid <- "xtmp"
            object$mapping <- modifyList(object$mapping,aes_string(x=xid))
        }
    }else{
        object$data$xtmp <- 0
        xid <- "xtmp"
        object$mapping <- modifyList(object$mapping,aes_string(x=xid))
    }
    return (list(object, xid))
}


adjust_angle_ <- function(layout, angle){
    if (!layout %in% c("linear")){
        angle <- 90 - angle
    }else{
        angle <- 90
    }
    return(angle)
}

adjust_text_angle_ <- function(layout, angle){
    if (!layout %in% c("linear")){
        angle <- unlist(lapply(angle, function(i)
                               {if (i>90 && i<270){
                                   i <- i - 180}
                               return(i)}))
    }else{
        angle <- 0
    }
    return(angle)
}


#' @importFrom ggplot2 last_plot
get_circos_layout <- function(plot_view=NULL){
    if (is.null(plot_view)){
        plot_view <- last_plot()
    }
    layout <- get("layout", envir = plot_view$plot_env)
    if (!inherits(layout, "character")) {
        layout <- attr(plot_view$data, "layout")
    }
    return(layout)
}

get_offset_ <- function(vnum, ratio){
    offset <- ratio*max(vnum, na.rm=TRUE)
}

#' @importFrom yulab.utils get_fun_from_pkg
select_lab_geom <- function(geom){
    geom <- match.arg(geom, c("text", "label", "shadowtext"))
    geom <- switch(
              geom,
              label = get_fun_from_pkg("ggtree", "geom_label2"),
              text = get_fun_from_pkg("ggtree", "geom_text2"),
              shadowtext = get_fun_from_pkg("shadowtext", "geom_shadowtext")
          )
    return(geom)
}
