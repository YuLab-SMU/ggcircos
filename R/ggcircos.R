# Space reserved outside the outermost band for labels. The sector label
# carries the largest offset (0.16), so 0.22 leaves a readable margin.
label_space <- 0.22

#' Start a circular layout
#'
#' Creates the plot that every other ggcircos layer attaches to. The layout
#' — node arcs, sectors and the angular slot reserved for each link — is
#' computed once, here, and stored as data; it is never modified afterwards.
#' `ggcircos()` itself adds no layer, so you can draw links, node arcs,
#' labels and outer tracks in any order.
#'
#' @param data A square `matrix`, a `data.frame` with `from`/`to` columns,
#'   a `tidygraph`/`igraph` object, or an existing `circos_layout`.
#' @param nodes,sectors,order,gap,sector_gap,slot_fill,start,tracks,track_offset,self_loops
#'   Passed to [circos_layout()], which documents each of them.
#' @param layout `"circular"` for the polar layout, `"arc"` for the linear
#'   one, where links bulge to the left of a vertical spine.
#' @param weight Name of the column holding each link's weight, e.g.
#'   `weight = r` or `weight = "r"` (a matrix supplies `r`). Set it to make
#'   the layout carry the values the way [circlize::chordDiagram()] does:
#'   node arcs proportional to the weight they carry, slots proportional to
#'   their link's weight. Left `NULL` every node keeps an equal arc and the
#'   values only drive `aes(fill = )`.
#' @param radius Base radius of the node ring. `ggcircos()` derives this from
#'   `layout`; you rarely need to set it.
#' @param ... Passed on to [circos_layout()].
#'
#' @return A ggplot with a ggcircos layout attached.
#' @seealso [geom_chord()], [geom_node_arc()], [geom_track()]
#' @examples
#' ggcircos(cor(mtcars), order = "hclust") +
#'     geom_chord(aes(fill = r)) +
#'     geom_node_arc() +
#'     geom_node_text()
#'
#' # two genomes in two sectors
#' nodes <- data.frame(
#'     id = c("a", "b", "c", "d"),
#'     genome = c("ref", "ref", "qry", "qry"),
#'     stringsAsFactors = FALSE
#' )
#' links <- data.frame(
#'     from = c("a", "c"), to = c("d", "b"),
#'     stringsAsFactors = FALSE
#' )
#' ggcircos(links, nodes = nodes, sectors = "genome") +
#'     geom_chord() +
#'     geom_node_arc()
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_y_continuous
#' @importFrom rlang .data
#' @export
ggcircos <- function(data,
                     nodes = NULL,
                     sectors = NULL,
                     order = NULL,
                     layout = c("circular", "arc"),
                     gap = 0.01,
                     sector_gap = 0.05,
                     slot_fill = 0.9,
                     start = 0,
                     tracks = NULL,
                     track_offset = 0.06,
                     self_loops = FALSE,
                     radius = NULL,
                     weight = NULL,
                     ...) {
    layout <- match.arg(layout)
    radius <- radius %||% if (layout == "arc") 0 else 1

    # `weight = r` and `weight = "r"` both name a column of the link table;
    # the layout resolves it once and the geometry follows the values
    weight_col <- if (rlang::quo_is_null(rlang::enquo(weight))) {
        NULL
    } else {
        column_or_string(rlang::enquo(weight), "weight")
    }
    lay <- circos_layout(
        data,
        nodes = nodes, sectors = sectors, order = order, gap = gap,
        sector_gap = sector_gap, slot_fill = slot_fill, start = start,
        tracks = tracks, track_offset = track_offset,
        self_loops = self_loops, radius = radius, weight = weight_col, ...
    )

    rmax <- radius
    if (nrow(lay$tracks)) rmax <- max(rmax, max(lay$tracks$r1))
    # room outside the outermost band for the node and sector labels: without
    # it the labels are pinned to the panel edge (plain chord) or buried under
    # the first track (whose inner edge is r = 1 + track_offset)
    xmax <- rmax + label_space
    # in the linear layout links bulge left of the spine, so the radius axis
    # has to reach into negative values too
    xlim <- if (layout == "arc") c(-1.1, xmax) else c(0, xmax)

    p <- ggplot(
        data = structure(lay$links, circos_layout = lay),
        mapping = aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)
    ) +
        # squish rather than drop: a node straddling theta = 0 makes some
        # track layers reach just outside [0, 2*pi], and losing those rows
        # would silently delete a node's track
        scale_y_continuous(limits = c(0, 2 * pi), expand = c(0, 0),
                           oob = scales::squish) +
        scale_x_continuous(limits = xlim, expand = c(0, 0),
                           oob = scales::squish) +
        theme_circos()

    if (layout == "arc") {
        p + coord_arc(xlim = xlim, ylim = c(0, 2 * pi))
    } else {
        # `start` is where theta = 0 sits on the ring, so the coordinate has
        # to be told about it: the layout put the first node there, and the
        # screen angles (spec §5.4) are measured from the same origin
        p + coord_circos(start = start)
    }
}