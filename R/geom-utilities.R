#' @keywords internal
build_curvature <- function(starttheta, endtheta){
    flag <- endtheta - starttheta
    if (flag > 0){
        if (flag < pi){
            origin_direction <- 1
        }else{
            origin_direction <- -1
        }
    }else{
        if (abs(flag) < pi){
            origin_direction <- - 1
        }else{
            origin_direction <- 1
        }
    }
    flag <- min(c(abs(flag), 2*pi-abs(flag)))
    curvature <- origin_direction * (1 - flag/pi)
    return(curvature)
}

#' @importFrom utils getFromNamespace
ggname <- getFromNamespace("ggname", "ggplot2")

"%||%" <- getFromNamespace("%||%", "ggplot2")
