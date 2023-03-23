##' @title plot external layers of ggcircos.
##'
##' @rdname geom_circos
##' @param mapping aes mapping for 'geom'
##' @param data data to plot by 'geom', the column contained circos node labels
##' should be as y in mapping.
##' @param geom geom function to plot the data.
##' @param offset numeric, distance between external layers or between circos and external layers,
##' default is 0.03, meaning the 0.03 times of x range of circos (0.03 * radius of circos).
##' @param pwidth numeric, the width of external geometric layer, default is 0.2,
##' meaning the 0.2 times of radius of circos (0.2 * radius of circos).
##' @param position Position adjustment, either as a string, or the result of a
##' call to a position adjustment function, default is 'auto', see details in the following.
##' @param inherit.aes logical, If 'FALSE', overrides the default aesthetics, rather than
##' combining with them. This is most useful for helper functions that define both data and
##' aesthetics and shouldn't inherit behaviour from the default plot specification,
##' default is FALSE.
##' @param grid.params list, the parameters to control the attributes of grid lines,
##' default is NULL, see the grid.params in the following.
##'
##' grid.params control the attributes of grid line of external layer, it can be referred to the
##' following parameters:
##'     \itemize{
##'         \item \code{vline} logical, whether add the vertical line, default is FALSE.
##'         \item \code{color} color of line, default is grey.
##'         \item \code{size} the width of line, default is 0.2.
##'         \item \code{alpha} the colour transparency of line, default is 1.
##'         \item \code{lineend} Line end style (round, butt, square), default is "butt".
##'         \item \code{linejoin} Line end style (round, butt, square), default is "round".
##'         \item \code{linetype} Type of line, default is 1.
##'     }
##' @param axis.params list, the parameters to control the attributes of pseudo axis,
##' see the axis.params in the following.
##'
##' axis.params control the attributes of axis, it can be referred to the following parameters:
##'     \itemize{
##'         \item \code{axis} character, add the axis, if it is set to "none", meaning don't display axis (default),
##'         "x" display the x axis, "y" display the y axis, "xy" display the two axis.
##'         \item \code{text} vector, the text of axis x, default is NULL, it is only valid when
##'         the text of axis is single and x is discrete.
##'         \item \code{vjust} numeric, A numeric specifying vertical justification, default is 0.5.
##'         \item \code{hjust} numeric, A numeric specifying horizontal justification, default is 0.5.
##'         \item \code{text.angle} numeric, the angle of axis text, default is 0.
##'         \item \code{text.size} numeric, the size of axis text, default is 0.8.
##'         \item \code{title} character, the title of panel or x-axis label, default is NULL, it is only
##'          valid when "x" axis exists.
##'         \item \code{title.size} numeric, the size of title text, default is 3.
##'         \item \code{title.height} numeric, the height of title text position more than circos, default is 0.1,
##'          it is relative to height of circos.
##'         \item \code{title.angle} numeric, the angle of title text, default is 0.
##'         \item \code{title.color} character, the color of title text, default is "black".
##'         \item \code{nbreak} numeric, meaning the number of axis to break,
##'          integer giving the _desired_ number of intervals. Non-integer values are rounded down.
##'          It is only valid when x is continuous, default is 4.
##'         \item \code{line.size} numeric, the size of axis line, default is 0.2.
##'         \item \code{line.color} character, the color of axis line color, default is "grey".
##'         \item \code{line.alpha} numeric, the colour transparency of axis line, default is 1.
##' }
##' @param ... additional parameters for 'geom' 
##' @details
##'
##' The 'data' parameter is data.frame or tibble type, it is the same with the data for corresponding
##' geometric layers, but it must contain one column of taxa labels of circos, it will be mapped to 'y' axis
##' in 'mapping'. When 'data' is not provided, the associated data in circos data will be extracted automatically,
##' and the 'y' axis don't need to be mapped.
##'
##' The 'mapping' parameter is setting of aesthetic mappings created by 'aes()' or 'aes_()' of 'ggplot2',
##' the 'y' should be assigned to the variable names of column of taxa labels in data.frame of 'data', only if
##' the 'data' is not provided, see the above.
##'
##' The 'geom' parameter is the geometric function defined in 'ggplot2' or other 'ggplot2-extension',
##' e.g.
##'  \tabular{lll}{
##'    ggplot2 \tab geom_bar,geom_col,geom_boxplot,geom_violin,geom_tile \tab circular, rectangular \cr
##'    ggmsa   \tab geom_msa \tab rectangular \cr
##'    ggstar  \tab geom_star \tab circular, rectangular \cr
##'    ggimage \tab geom_image,geom_phylopic \tab circular, rectangular \cr
##'    ggpmisc \tab geom_plot,geom_table \tab circular, rectangular \cr
##'    ggridges \tab geom_density_ridges \tab circular, rectangular \cr
##'    ggtext  \tab geom_richtext \tab circular, rectangular \cr
##'    ...
##' }
##' if the 'geom' is 'geom_bar', 'geom_col', 'geom_boxplot', 'geom_violin', the 'orientation' should be
##' specified to 'y'.
##'
##' The default 'position' parameter is 'auto', it will guess and determine (hopefully) a suitable
##' position for the specified geometric layer. That means using 'position_stackx()' for geom_bar(),
##' 'position_dodgex()' for 'geom_violin()' and 'geom_boxplot()', and 'position_identityx()' for
##' others (e.g. geom_point() and geom_tile() etc.). A geometric layer that has a position parameter
##' should be compatible with geom_circos(), as it allows using position functions defined in the
##' ggtreeExtra package to adjust output layer position.
##'
##' and the grid line also can be added using 'grid.params=list(...)'.
##'
##' The axis line and text can be added using 'axis.params=list(axis="x",...)'.
##' @importFrom rlang enquo
##' @export
##' @examples
##' library(ggplot2)
##' data(mtcars)
##' p <- mtcars %>% cor %>% as.tbl_lk() %>% ggcircos()
##' dat <- data.frame(id=colnames(mtcars), values=abs(rnorm(ncol(mtcars))))
##' p <- p + 
##'      geom_circos(
##'        data = dat, 
##'        geom = geom_col,
##'        mapping = aes(y = id, x = values),
##'        orientation = "y",
##'        pwidth = 0.5
##'      )
##' p
geom_circos <- function(
    mapping,
    geom,
    #panel,
    data = NULL,
    position = 'auto',
    pwidth = 0.3,
    offset = 0.02,
    inherit.aes = FALSE,
    grid.params=NULL,
    axis.params=list(
                    axis="none",
                    text.angle=0,
                    text.size=0.8,
                    text=NULL,
                    title = NULL,
                    title.size = 3,
                    title.height = 0.1,
                    title.angle = 0,
                    title.color = "black",
                    nbreak=4,
                    line.size=0.2,
                    line.color="grey",
                    line.alpha=1,
                    ...
                ),    
    ...){
    geomname <- as.character(as.list(match.call())[["geom"]])
    default.grid.params <- list(color="grey",
                                size=0.2,
                                alpha=1,
                                lineend="butt",
                                linejoin="round",
                                vline=FALSE,
                                inherit.aes=FALSE)
    default.axis.params <- list(axis="none",
                                text.angle=0,
                                text.size=0.8,
                                text=NULL,
                                title = NULL,
                                title.size = 3,
                                title.height = 0.1,
                                title.angle = 0,
                                title.color = "black",
                                line.size=0.2,
                                line.color="grey",
                                nbreak=4,
                                line.alpha=1,
                                inherit.aes=FALSE)
    params <- list(...)
    if (check_orientation(x = geomname) && !'orientation' %in% names(params)){
        params <- c(params, orientation = 'y')
    }    
    grid.params <- reset_params(defaultp=default.grid.params,
                                inputp=enquo(grid.params))
    axis.params <- reset_params(defaultp=default.axis.params,
                                inputp=enquo(axis.params))
    axis.params <- confuse_params(axis.params)
    axis.dot.params <- extract_dot_params(
                           defaultp=default.axis.params,
                           inputp=axis.params
                       )
    grid.dot.params <- extract_dot_params(
                           defaultp=default.grid.params,
                           inputp=grid.params
                       )   
    obj <- structure(
             list(
               data = data,
               geom = geom,
               #panel = panel, 
               mapping = mapping,
               params = params,
               offset = offset,
               pwidth = pwidth,
               inherit.aes = inherit.aes,
               position=position,
               geomname=geomname,
               grid.params=grid.params,
               axis.params=axis.params,
               grid.dot.params=grid.dot.params,
               axis.dot.params=axis.dot.params
             ),
             class = 'circos_panel'
           )
    obj <- choose_pos(object=obj)    

}
