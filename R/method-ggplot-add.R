#' @importFrom ggplot2 ggplot_add
#' @importFrom ggfun get_aes_var
#' @importFrom ggtree geom_text2
#' @method ggplot_add link_lab
#' @export
ggplot_add.link_lab <- function(object, plot, object_name){
    layout <- get_circos_layout(plot)
    dat1 <- tl_extract(
              name = !!rlang::sym("node")
            )(plot$data)
    geomlayer <- select_lab_geom(geom=object$geom)
    if (layout == "circular"){
        dat2 <- dat1
        subset1 <- "(angle < 90 | angle > 270)"
        subset2 <- "(angle >= 90 & angle <=270)"
        m1 <- aes_string(subset=subset1, x="x", y="y", angle="angle", label="label")
        m2 <- aes_string(subset=subset2, x="x", y="y", angle="angle+180", label="label")
        if (!is.null(object$mapping)) {
            if (!is.null(object$mapping$subset)) {
                newsubset1 <- paste0(as.expression(get_aes_var(object$mapping, "subset")), '&', subset1)
                newsubset2 <- paste0(as.expression(get_aes_var(object$mapping, "subset")), '&', subset2)
                m1 <- aes_string(angle = "angle", node = "node", subset = newsubset1)
                m2 <- aes_string(angle = "angle+180", node = "node", subset = newsubset2)
            }
            m1 <- modifyList(object$mapping, m1)
            m2 <- modifyList(object$mapping, m2)
        }
        params1 <- params2 <- object$params
        params1$data <- dat1
        params2$data <- dat2
        params1$mapping <- m1
        params2$mapping <- m2
        params1$nudge_x <- object$offset
        params2$nudge_x <- object$offset
        params1$hjust <- object$hjust
        params2$hjust <- 1 - object$hjust
        obj <- list(do.call(geomlayer, params1), do.call(geomlayer, params2))
    }else if (layout == 'linear'){
        params <- object$params
        params$data <- dat1
        m <- aes_string(x = "x", y = "y", label = "label")
        if (!is.null(object$mapping)){
            m <- modifyList(object$mapping, m)
        }
        params$mapping <- m
        params$hjust <- object$hjust
        params$nudge_x <- object$offset 
        obj <- do.call(geomlayer, params)
    }
    ggplot_add(obj, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add aes_string 
#' @importFrom ggplot2 scale_color_manual
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
    if (object$axis.params$axis != "none"){
        obj.axis <- build_axis(dat=dat,
                               xid=xid,
                               text=object$axis.params$text,
                               position=object$params$position,
                               axis.params=object$axis.params,
                               axis.dot.params=object$axis.dot.params,
                               y.range = range(refdat$y))
        obj <- list(obj, obj.axis)
    }
    if (!is.null(object$grid.params)){
        obj.grid <- build_grid(dat=dat,
                               xid=xid,
                               position=object$params$position,
                               grid.params=object$grid.params,
                               grid.dot.params=object$grid.dot.params,
                               y.range = range(refdat$y))
        obj <- list(obj.grid, obj)
    }
    # because original y is continuous, but y of box plot density plot is discrete
    # to combine them, should map y to group or color, but sometimes group box
    # or density plot is also a demand, so group should not be mapped,
    # only left color.
    if (object$geomname %in% c(dodpos, densitypos)){
        obj <- list(obj, scale_color_manual(values=c(rep("black", length(dat$y))), guide="none"), new_scale_color())
    }
    ggplot_add(obj, plot, object_name)
}

