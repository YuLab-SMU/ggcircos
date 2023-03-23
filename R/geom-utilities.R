#' @keywords internal
build_curvature <- function(starttheta, endtheta, hratio, ncp){
    flag <- endtheta - starttheta
    newflag <- min(c(abs(flag), 2*pi-abs(flag)))
    if (flag > 0){
        if (flag <= pi/2){
            origin_direction <- 1
            if (ncp==1){
                origin_direction <- origin_direction * hratio * 0.68 * pi/newflag
            }
        }else if (flag < pi && flag > pi/2){
            origin_direction <- 1
        }else if (flag > pi && flag <=3*pi/2){
            origin_direction <- -1
        }else{
            origin_direction <- -1
            if (ncp==1){
                origin_direction <- origin_direction * hratio * 0.68 * pi/newflag
            }
        }
    }else{
        if (abs(flag)<=pi/2){
            origin_direction <- -1
            if (ncp == 1){
                origin_direction <- origin_direction * hratio * 0.68 * pi/newflag
            }
        }else if (abs(flag) < pi && abs(flag) > pi/2){
            origin_direction <- -1
        }else if (abs(flag) > pi && abs(flag) <= 3*pi/2){
            origin_direction <- 1
        }else{
            origin_direction <- 1
            if (ncp==1){
                origin_direction <- origin_direction * hratio * 0.68 * pi/newflag
            }
        }
    }
    curvature <- origin_direction * (1 - newflag/pi)
    return(curvature)
}

generate_curvature2 <- function(starttheta, endtheta, hratio, ncp){
    flag <- endtheta - starttheta
    newflag <- min(c(abs(flag), 2 * pi - abs(flag)))
    if (flag > 0) {
        if (flag <= pi) {
            origin_direction <- -1
        }
        else {
            origin_direction <- 1
        }
    }
    else {
        if (abs(flag) <= pi) {
            origin_direction <- 1
        }
        else {
            origin_direction <- -1
        }
    }
    if (newflag > pi/2) {
        curvature <- hratio * origin_direction * pi/newflag
    }
    else {
        curvature <- hratio * origin_direction * (1 - newflag/pi)
    }
    return(curvature)
}


generate_curvature <- function(starttheta, endtheta, hratio, ncp){
    flag <- endtheta - starttheta
    newflag <- min(c(abs(flag), 2 * pi - abs(flag)))
    if (flag > 0) {
        if (flag <= pi) {
            origin_direction <- 1
        }
        else {
            origin_direction <- -1
        }
    }
    else {
        if (abs(flag) <= pi) {
            origin_direction <- -1
        }
        else {
            origin_direction <- 1
        }
    }
    curvature <- hratio * origin_direction * (1 - newflag/pi)
    return(curvature)
}


#' @importFrom utils getFromNamespace
ggname <- getFromNamespace("ggname", "ggplot2")

"%|||%" <- function(x, y){
    if (is.null(x)){
        return (y)
    }else{
        if (length(x)<length(y)){
            return (y)
        }else{
            return (x)
        }
    }
}

#new_data_frame <- getFromNamespace("new_data_frame", "ggplot2")
