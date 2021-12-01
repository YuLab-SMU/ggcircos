# #' @importFrom ggplot2 ggplot_add
# #' @method ggplot_add link_lab
# #' @export
# ggplot_add.link_lab <- function(){
# 
# }

#' @importFrom ggplot2 ggplot_add aes_string
#' @method ggplot_add circos_panel
#' @importFrom ggnewscale new_scale_color
#' @export
ggplot_add.circos_panel <- function(object, plot, object_name){
    object <- check_plotdata_(object=object, plot=plot)
    object <- check_subset_aes_(object=object)
    object <- build_new_data_(object=object, plot=plot)
    object <- compute_aes(object=object, plot=plot)
    res <- set_mapping_(object=object)
    object <- res[[1]]
    xid <- res[[2]]
    layout <- get_circos_layout(plot)
    refdat <- tl_extract(name=!!rlang::sym("node"))(plot$data)
    offset <- get_offset_(refdat$x, object$offset)
    if ("xmaxtmp" %in% colnames(refdat)){
        hexpand2 <- max(abs(refdat$xmaxtmp), na.rm=TRUE) + offset
    }else{
        hexpand2 <- max(abs(refdat$x), na.rm=TRUE) + offset
    }
    orientation <- 1
    dat <- object$data
    refdat <- tl_extract(name=!!rlang::sym("node"))(plot$data)
    if (is.numeric(dat[[xid]]) & !all(dat[[xid]]==0)){
        normres <- get_continuous_norm(refdata=refdat$x,
                                       data=dat,
                                       orientation=orientation,
                                       xid=xid,
                                       position=object$params$position,
                                       geomname=object$geomname,
                                       ratio=object$pwidth,
                                       nbreak=object$axis.params$nbreak)
        dat <- normres[[1]]
        newxexpand <- normres[[2]]
    }else{
        if (!is.numeric(dat[[xid]])){
            if (!is.factor(dat[[xid]])){
                dat[[xid]] <- factor(dat[[xid]], levels=sort(unique(as.vector(dat[[xid]]))))
            }
            dat[[paste0(xid,"_bp")]] <- as.numeric(dat[[xid]])
            dat[[paste0("new_", xid)]] <- orientation *
                                          normxy(refnum=refdat$x, targetnum=dat[[paste0(xid,"_bp")]],
                                                 keepzero=TRUE, ratio=object$pwidth)
            if (orientation > 0){
                dat[[paste0("new_", xid)]] <- dat[[paste0("new_", xid)]] + offset
            }
            dat <- dat[order(-dat$y, dat[[paste0("new_", xid)]]),,drop=FALSE]
            newxexpand <- max(abs(dat[[paste0("new_", xid)]]), na.rm=TRUE)
        }else{
            if (!"hexpand" %in% names(object$params$position)){
                dat[[paste0("new_", xid)]] <- data.frame(refdat, check.names=FALSE)[match(dat$label, refdat$label),"x"]
            }else{
                dat[[paste0("new_", xid)]] <- 0 
            }
            newxexpand <- 0
        }
    }

    if ("xmaxtmp" %in% colnames(refdat)){
        refdat$xmaxtmp <- refdat$xmaxtmp + newxexpand + offset
    }else{
        refdat$xmaxtmp <- refdat$x + newxexpand + offset
    }

    attr(plot$data, "node") <- refdat

    if ("hexpand" %in% names(object$params$position)){
        if (is.na(object$params$position$hexpand)){
            if (orientation < 0){
                hexpand2 <- abs(hexpand2)
            }
            object$params$position$hexpand <- hexpand2
        }
    }
    tmpangle <- dat$angle
    if (object$geomname=="geom_star"){
        dat$angle <- adjust_angle_(layout=layout, angle=tmpangle)
        object$mapping = modifyList(object$mapping, aes_(angle=~angle))
    }
    if (object$geomname=="geom_text"){
        dat$angle <- adjust_text_angle_(layout=layout, angle=tmpangle)
        object$mapping = modifyList(object$mapping, aes_(angle=~angle))
    }
    if (object$geomname %in% c(dodpos, densitypos)){
        object$mapping = modifyList(object$mapping, aes(color=factor(eval(parse(text="y")))))
        plot <- plot + new_scale_color()
    }
    object$mapping = modifyList(object$mapping, aes_string(x=paste0("new_",xid)))
    params <- c(list(data=dat, mapping=object$mapping, inherit.aes=object$inherit.aes), object$params)
    obj <- do.call(object$geom, params)
    ggplot_add(obj, plot, object_name)
}

