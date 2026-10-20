#' Build a circular layout
#'
#' A `circos_layout` turns a set of nodes and links into polar coordinates.
#' Everything a plot needs is computed *once*, here, and returned as data:
#' the arc of every node, the sector each node belongs to, and the angular
#' slot reserved for every link endpoint. Layers never recompute any of it.
#'
#' @param x A square `matrix`, a `data.frame` with `from`/`to` columns, a
#'   `tidygraph`/`igraph` object, or an existing `circos_layout`.
#' @param nodes An optional node table. Needs an `id` column; `label`,
#'   `sector` and any further columns are carried through to the layers.
#' @param sectors Sector assignment: either the name of a column of `nodes`,
#'   or a named character vector mapping node ids to sectors. Nodes are
#'   grouped in the order given (first appearance, or the order of the
#'   vector's names).
#' @param order Node order around the circle: `NULL` keeps the input order,
#'   `"hclust"` clusters nodes by link strength, a character vector gives an
#'   explicit order, and a function is called with the node table and must
#'   return node ids.
#' @param gap Fraction of a full turn left as a gap between neighbouring
#'   nodes.
#' @param sector_gap Extra fraction of a turn left between sectors.
#' @param start Angle (radians) of the first node arc.
#' @param slot_fill Fraction of a node's arc that its link slots occupy
#'   (slots are centred inside the arc).
#' @param self_loops Keep self-links (`from == to`). v0.1 cannot draw them,
#'   so they are dropped with a message.
#' @param tracks Named numeric vector of outer track widths, e.g.
#'   `c(values = 0.2, heat = 0.1)`. Widths are relative to the unit node ring.
#' @param track_offset Gap between the node ring, and between tracks. The
#'   first track band starts at `1 + track_offset`, so keep it at or above
#'   the node ring's outer edge (0.05 with the default `geom_node_arc()`).
#' @param weight Name of the column holding each link's weight, e.g.
#'   `weight = "r"`. When set, the geometry follows the values: a node's arc
#'   is proportional to the weight it carries and a slot is proportional to
#'   its link's weight (negative values count by magnitude). `NULL`, the
#'   default, gives every node an equal arc and every link an equal share of
#'   it -- the value then only shows through `aes(fill = )`.
#' @param radius Base radius of the node ring. `0` puts the ring on the
#'   spine of a linear (`coord_arc()`) layout.
#' @param ... Used by the methods to build the node table from graph objects.
#'
#' @return An object of class `circos_layout`.
#' @seealso [ggcircos()], [layout_nodes()], [layout_links()]
#' @examples
#' m <- cor(mtcars)
#' l <- circos_layout(m, order = "hclust", tracks = c(values = 0.2))
#' head(layout_nodes(l))
#' head(layout_links(l))
#' layout_tracks(l)
#' @export
circos_layout <- function(x,
                          nodes = NULL,
                          sectors = NULL,
                          order = NULL,
                          gap = 0.01,
                          sector_gap = 0.05,
                          start = 0,
                          slot_fill = 0.9,
                          self_loops = FALSE,
                          tracks = NULL,
                          track_offset = 0.06,
                          radius = 1,
                          weight = NULL,
                          ...) {
    UseMethod("circos_layout")
}

#' @export
circos_layout.default <- function(x, ...) {
    ggcircos_abort(
        "data",
        c(
            "Don't know how to build a layout from an object of class {.cls {class(x)}}.",
            i = "Use a square {.cls matrix}, a {.cls data.frame} with {.field from}/{.field to},
                 a {.pkg tidygraph} or {.pkg igraph} object, or an existing {.cls circos_layout}."
        )
    )
}

#' @rdname circos_layout
#' @export
circos_layout.circos_layout <- function(x, ...) x

#' @rdname circos_layout
#' @export
circos_layout.matrix <- function(x, nodes = NULL, ...) {
    if (nrow(x) != ncol(x)) {
        ggcircos_abort(
            "data",
            c(
                "The matrix is {.val {nrow(x)}}x{ncol(x)}}; a weight matrix must be square.",
                i = "For a bipartite graph use a {.cls data.frame} with {.field from}/{.field to}."
            )
        )
    }
    rn <- rownames(x)
    cn <- colnames(x)
    if (is.null(rn) || is.null(cn)) {
        ids <- as.character(seq_len(nrow(x)))
        rn <- cn <- ids
    }
    if (!identical(rn, cn)) {
        ggcircos_abort(
            "data",
            c(
                "Row and column names differ: {.val {setdiff(rn, cn)}} vs {.val {setdiff(cn, rn)}}.",
                i = "ggcircos() needs a square matrix with identical names, or supply a
                     {.cls data.frame} with {.field from}/{.field to} columns."
            )
        )
    }
    cells <- which(upper.tri(x) & !is.na(x), arr.ind = TRUE)
    links <- data.frame(
        from = rn[cells[, 1]],
        to = cn[cells[, 2]],
        r = as.vector(x[cells]),
        stringsAsFactors = FALSE
    )
    circos_layout_build(
        links = links,
        nodes = nodes,
        node_ids = if (nrow(links)) unique(c(links$from, links$to)) else unique(rn),
        ...
    )
}

#' @rdname circos_layout
#' @export
circos_layout.data.frame <- function(x, nodes = NULL, ...) {
    missing <- setdiff(c("from", "to"), names(x))
    if (length(missing) > 0) {
        ggcircos_abort(
            "data",
            c(
                "The data.frame is missing required column{?s} {.field {missing}}.",
                i = "Rename them, or pass {.arg from} / {.arg to} to {.fn geom_chord}."
            )
        )
    }
    links <- x
    links$from <- as.character(links$from)
    links$to <- as.character(links$to)
    circos_layout_build(
        links = links,
        nodes = nodes,
        node_ids = unique(c(links$from, links$to)),
        ...
    )
}

#' @rdname circos_layout
#' @export
circos_layout.tbl_graph <- function(x, nodes = NULL, ...) {
    require_suggests("tidygraph", "tbl_graph")
    verts <- as.data.frame(tidygraph::as_tibble(tidygraph::activate(x, "nodes")))
    edges <- as.data.frame(tidygraph::as_tibble(tidygraph::activate(x, "edges")))
    # tidygraph stores edge endpoints as 1-based *indices* into the node
    # table, where igraph stores names; resolve them to the same names the
    # node table will be keyed by (B19)
    ids <- if ("name" %in% names(verts)) as.character(verts$name) else
        as.character(seq_len(nrow(verts)))
    edges[[1]] <- ids[edges[[1]]]
    edges[[2]] <- ids[edges[[2]]]
    graph_to_input(verts, edges, nodes = nodes, ...)
}

#' @rdname circos_layout
#' @export
circos_layout.igraph <- function(x, nodes = NULL, ...) {
    require_suggests("igraph", "igraph")
    graph_to_input(
        igraph::as_data_frame(x, what = "vertices"),
        igraph::as_data_frame(x, what = "edges"),
        nodes = nodes, ...
    )
}

#' @keywords internal
#' @noRd
graph_to_input <- function(vertices, edges, nodes, ...) {
    if (!"name" %in% names(vertices)) {
        vertices$name <- as.character(seq_len(nrow(vertices)))
    }
    edges <- as.data.frame(edges, stringsAsFactors = FALSE)
    links <- data.frame(
        from = as.character(edges[[1]]),
        to = as.character(edges[[2]]),
        stringsAsFactors = FALSE
    )
    # edge attributes come along as value columns: `weight` is named `r` (the
    # matrix path's name), any other attribute keeps its own name. Dropping
    # them made `aes(fill = w)` fail on an igraph input (B19).
    if ("weight" %in% names(edges)) links$r <- edges$weight
    keep <- setdiff(names(edges), c(names(edges)[1:2], "weight"))
    for (nm in keep) links[[nm]] <- edges[[nm]]
    circos_layout_build(
        links = links,
        nodes = nodes,
        node_ids = as.character(vertices$name),
        node_defaults = vertices,
        ...
    )
}

#' @keywords internal
#' @noRd
require_suggests <- function(pkg, class) {
    if (!isTRUE(requireNamespace(pkg, quietly = TRUE))) {
        ggcircos_abort(
            "dependency",
            "Reading {.cls {class}} input needs the {.pkg {pkg}} package.",
            i = "Install {.pkg {pkg}}, or supply a {.cls data.frame} with
                 {.field from}/{.field to} columns."
        )
    }
    invisible(TRUE)
}

#' Columns the package owns in the link table
#' @keywords internal
#' @noRd
reserved_link_cols <- c(
    "from", "to", "edge_id", "theta", "theta_end",
    "theta0", "theta1", "theta_end0", "theta_end1",
    "x", "y", "xend", "yend"
)

#' @keywords internal
#' @noRd
circos_layout_build <- function(links,
                                nodes = NULL,
                                node_ids,
                                node_defaults = NULL,
                                sectors = NULL,
                                order = NULL,
                                gap = 0.01,
                                sector_gap = 0.05,
                                start = 0,
                                slot_fill = 0.9,
                                self_loops = FALSE,
                                tracks = NULL,
                                track_offset = 0.06,
                                radius = 1,
                                weight = NULL) {
    if (anyNA(gap) || !is.numeric(gap) || length(gap) != 1L || gap <= 0) {
        ggcircos_abort("layout", "{.arg gap} must be a single positive number.")
    }
    if (anyNA(sector_gap) || !is.numeric(sector_gap) || length(sector_gap) != 1L || sector_gap < 0) {
        ggcircos_abort("layout", "{.arg sector_gap} must be a single non-negative number.")
    }
    if (anyNA(start) || !is.numeric(start) || length(start) != 1L) {
        ggcircos_abort("layout", "{.arg start} must be a single number.")
    }
    if (anyNA(slot_fill) || !is.numeric(slot_fill) || length(slot_fill) != 1L ||
        slot_fill <= 0 || slot_fill > 1) {
        ggcircos_abort("layout", "{.arg slot_fill} must be a single number in (0, 1].")
    }

    links <- as.data.frame(links, stringsAsFactors = FALSE)
    # the package owns these column names; a user column of the same name is
    # preserved as user_<name> rather than silently overwritten
    shadow <- setdiff(reserved_link_cols, c("from", "to"))
    shadow <- intersect(names(links), shadow)
    for (nm in shadow) {
        links[[paste0("user_", nm)]] <- links[[nm]]
        links[[nm]] <- NULL
    }
    links$from <- as.character(links$from)
    links$to <- as.character(links$to)
    if (anyNA(links$from) || anyNA(links$to)) {
        ggcircos_abort("data", "Links have missing {.arg from} or {.arg to} values.")
    }

    loop <- links$from == links$to
    if (any(loop)) {
        if (isTRUE(self_loops)) {
            ggcircos_abort(
                "data",
                "{sum(loop)} self-link{?s} found, but {.arg self_loops = TRUE} is not supported in 0.1.0."
            )
        }
        links <- links[!loop, , drop = FALSE]
        cli::cli_inform(c(
            "i" = "Dropped {sum(loop)} self-link{?s} ({.arg from == to}); ggcircos 0.1.0 does not draw them."
        ))
    }

    nd <- circos_nodes_table(nodes, node_defaults, node_ids)
    unknown <- setdiff(unique(c(links$from, links$to)), nd$id)
    if (length(unknown) > 0) {
        ggcircos_abort(
            "node",
            c(
                "Link{?s} refer to unknown node{?s} {.val {head(unknown, 5)}}.",
                i = "Known nodes: {.val {head(nd$id, 10)}}.",
                i = "Use the {.arg nodes} argument to supply the full node table."
            )
        )
    }

    nd$sector <- circos_assign_sectors(nd, sectors)
    nd <- circos_apply_order(nd, links, order)
    # A sector is a *contiguous run* of node arcs: the angle budget reserves
    # exactly S-1 sector seams (spec 4.3-4) and the sector span is taken from
    # the run's two ends (spec 4.6-5). An ordering that interleaves two
    # sectors (`order = "hclust"` with `sectors =`, or an explicit vector)
    # would insert one seam per switch, overflow the turn and overlap arcs --
    # so the sectors are grouped here and the requested order is honoured
    # *inside* each one (R1).
    nd <- circos_group_sectors(nd)

    n <- nrow(nd)
    if (n == 0L) {
        # a plot may legitimately hold no nodes at all (an empty data.frame);
        # the layout then carries no geometry and only the plot shell exists
        ggcircos_abort(
            "data",
            c(
                "No nodes to lay out: the input has no rows.",
                i = "Pass a {.cls matrix}, links with {.arg from}/{.arg to}, or a graph object."
            )
        )
    }
    n_sectors <- length(unique(nd$sector))
    # the gap budget is a *fraction of a turn* spent per seam; with many
    # nodes a literal default swallows the whole circle. Both gaps are
    # clamped to a fifth of the turn overall, so a batch of matrices of any
    # size still yields a readable ring out of the box (spec §4.3-4).
    gap_eff <- min(gap, 0.2 / n)
    sector_gap_eff <- min(sector_gap, 0.2 / max(n_sectors - 1L, 1L))
    g <- gap_eff * 2 * pi
    sg <- sector_gap_eff * 2 * pi
    span <- 2 * pi - n * g - (n_sectors - 1) * sg
    if (span <= 0) {
        ggcircos_abort(
            "layout",
            c(
                "Gap too large: {.arg gap}*{n} plus {.arg sector_gap}*{k} fills a whole turn.",
                i = "Use a smaller {.arg gap} / {.arg sector_gap}."
            ),
            n = n, k = n_sectors - 1L
        )
    }
    # Link weights make the geometry carry the values, the way circlize's
    # chordDiagram does: a node's arc is proportional to the weight it
    # carries and a slot is proportional to its link's weight. With
    # `weight = NULL` every node weighs the same, so the arcs stay equal and
    # the slots stay equal -- the previous behaviour, unchanged.
    links$.circos_w <- resolve_link_weights(links, weight)
    nw <- if (is.null(weight)) {
        rep(1, n)
    } else {
        # a link's weight counts at both of its ends, the way a matrix row
        # sum counts a cell once from each side
        nw <- numeric(n)
        acc <- rowsum(rep(links$.circos_w, 2L),
                      match(c(links$from, links$to), nd$id), reorder = TRUE)
        nw[as.integer(rownames(acc))] <- acc[, 1L]
        nw
    }
    if (sum(nw) <= 0) {
        ggcircos_abort(
            "data",
            c("The weights are all zero, so there is nothing to draw.",
              i = "Check the {.field {weight}} column, or leave {.arg weight} unset.")
        )
    }
    w <- span * nw / sum(nw)
    switch_sector <- c(FALSE, nd$sector[-1] != nd$sector[-n])
    if (n > 1L) {
        after <- c(rep(g, n - 1L), 0) + ifelse(switch_sector, sg, 0)
        offset <- c(0, cumsum((w + after)[-n]))
    } else {
        offset <- 0
    }
    nd$theta0 <- wrap_2pi(start + offset)
    nd$theta1 <- wrap_2pi(nd$theta0 + w)
    nd$theta <- wrap_2pi(nd$theta0 + w / 2)
    nd$width <- w
    nd$order <- seq_len(n)
    nd$r <- radius

    sectors_df <- circos_sectors_table(nd)

    links <- allocate_slots(links, nd, slot_fill)
    nd$n_links <- tabulate(
        match(c(links$from, links$to), nd$id),
        nbins = n
    )

    tracks_df <- circos_track_bands(tracks, track_offset, radius)
    check_layout_invariants(nd, links, g, sg)
    links$slot_span <- NULL

    nd_out <- nd[, c(
        "id", "label", "sector", "order", "n_links",
        "theta0", "theta1", "theta", "width", "r",
        setdiff(names(nd), c(
            "id", "label", "sector", "order", "n_links",
            "theta0", "theta1", "theta", "width", "r"
        ))
    ), drop = FALSE]
    rownames(nd_out) <- NULL
    rownames(links) <- NULL

    structure(
        list(
            nodes = nd_out,
            links = links,
            sectors = sectors_df,
            tracks = tracks_df,
            params = list(
                gap = gap, sector_gap = sector_gap, slot_fill = slot_fill,
                start = start, track_offset = track_offset, radius = radius,
                # the *effective* values after the budget clamp of §4.3-4:
                # what the ring actually uses, not what was asked for
                gap_eff = gap_eff, sector_gap_eff = sector_gap_eff
            )
        ),
        class = c("circos_layout", "list")
    )
}

#' @keywords internal
#' @noRd
circos_nodes_table <- function(nodes, node_defaults, node_ids) {
    # an explicit `nodes` table is authoritative: link endpoints that it does
    # not list are an error the caller needs to see, not a silent new node
    base_ids <- node_ids
    if (!is.null(nodes)) {
        probe <- as.data.frame(nodes, stringsAsFactors = FALSE)
        if (!"id" %in% names(probe) && "name" %in% names(probe)) probe$id <- probe$name
        if (!"id" %in% names(probe) && "label" %in% names(probe)) probe$id <- probe$label
        if (!"id" %in% names(probe)) {
            ggcircos_abort(
                "node",
                "The {.arg nodes} table needs an {.field id} column (or {.field label} / {.field name} to use as id)."
            )
        }
        base_ids <- as.character(probe$id)
    }

    nd <- data.frame(
        id = as.character(base_ids),
        label = as.character(base_ids),
        stringsAsFactors = FALSE
    )
    for (tbl in list(node_defaults, nodes)) {
        if (is.null(tbl)) next
        # an explicit `nodes` table is authoritative; the tables contributed
        # by a graph object only fill in what the user did not spell out
        extend <- is.null(nodes)
        tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
        if (!"id" %in% names(tbl) && "name" %in% names(tbl)) tbl$id <- tbl$name
        if (!"id" %in% names(tbl) && "label" %in% names(tbl)) tbl$id <- tbl$label
        if (!"id" %in% names(tbl)) {
            ggcircos_abort(
                "node",
                "The {.arg nodes} table needs an {.field id} column (or {.field label} / {.field name} to use as id)."
            )
        }
        tbl$id <- as.character(tbl$id)
        if (!"label" %in% names(tbl)) tbl$label <- tbl$id
        tbl$label <- as.character(tbl$label)
        tbl <- tbl[!duplicated(tbl$id), , drop = FALSE]
        extra <- setdiff(tbl$id, nd$id)
        if (length(extra) && extend) {
            tbl <- rbind(
                data.frame(id = extra, label = extra, stringsAsFactors = FALSE),
                tbl
            )
        }
        # don't let a user column shadow the package's own node columns
        dup <- setdiff(intersect(names(nd), names(tbl)), "id")
        for (nm in dup) {
            tbl[[paste0("user_", nm)]] <- tbl[[nm]]
            tbl[[nm]] <- NULL
        }
        if ("user_label" %in% names(tbl)) {
            keep <- is.na(nd$label)
            nd$label[keep] <- tbl[["user_label"]][match(nd$id[keep], tbl$id)]
            tbl[["user_label"]] <- NULL
        }
        nd <- merge(nd, tbl, by = "id", all.x = TRUE, sort = FALSE)
        nd$label[is.na(nd$label)] <- nd$id[is.na(nd$label)]
    }
    if (anyDuplicated(nd$id)) {
        ggcircos_abort("node", "The {.arg nodes} table has duplicated ids.")
    }
    nd
}

#' @keywords internal
#' @noRd
circos_assign_sectors <- function(nd, sectors) {
    if (is.null(sectors)) return(rep("all", nrow(nd)))
    if (is.null(names(sectors)) && length(sectors) == 1L && is.character(sectors)) {
        if (!sectors %in% names(nd)) {
            ggcircos_abort(
                "layout",
                "Column {.field {sectors}} not found in the node table.",
                i = "Available columns: {.field {names(nd)}}."
            )
        }
        out <- as.character(nd[[sectors]])
        # a missing sector is a data error the caller has to see; letting NA
        # through ends up in the invariant check as a bare stopifnot()
        if (anyNA(out)) {
            bad <- unique(nd$id[is.na(out)])
            ggcircos_abort(
                "node",
                "The sector column {.field {sectors}} has no value for node{?s} {.val {bad}}.",
                i = "Fill the column, or supply {.arg sectors} as a named vector."
            )
        }
        return(out)
    }
    if (is.null(names(sectors)) || any(names(sectors) == "")) {
        ggcircos_abort(
            "layout",
            "{.arg sectors} must be a column name or a *named* character vector mapping node id to sector."
        )
    }
    miss <- setdiff(nd$id, names(sectors))
    if (length(miss) > 0) {
        ggcircos_abort("node", "The {.arg sectors} vector does not cover node{?s} {.val {miss}}.")
    }
    out <- unname(sectors[nd$id])
    out[is.na(out)] <- "all"
    as.character(factor(out, levels = unique(c(names(sectors), "all"))))
}

#' @keywords internal
#' @noRd
#' Keep each sector's nodes together, preserving the order inside a sector
#'
#' The angle budget reserves one seam per *sector*, not per adjacent pair, so
#' a layout whose nodes interleave two sectors cannot be drawn: it would put
#' a sector gap at every switch and push the arcs past a full turn. Grouping
#' is stable, so whatever order the caller asked for still holds within each
#' sector.
#' @keywords internal
#' @noRd
circos_group_sectors <- function(nd) {
    if (!"sector" %in% names(nd) || nrow(nd) < 2L) return(nd)
    key <- match(nd$sector, unique(nd$sector))
    if (length(unique(key)) < 2L) return(nd)
    # already grouped: every sector's nodes are one run
    runs <- sum(c(FALSE, nd$sector[-1] != nd$sector[-nrow(nd)])) + 1L
    if (runs == length(unique(key))) return(nd)
    nd[order(key, seq_len(nrow(nd))), , drop = FALSE]
}

#' @keywords internal
#' @noRd
circos_apply_order <- function(nd, links, order) {
    if (is.null(order)) return(nd)
    if (is.function(order)) {
        got <- order(nd)
        order <- if (is.character(got)) got else as.character(got)
    } else if (identical(order, "hclust")) {
        order <- circos_hclust_order(nd, links)
    }
    order <- as.character(order)
    if (length(order) != nrow(nd) || anyDuplicated(order) ||
        !setequal(order, nd$id)) {
        ggcircos_abort(
            "layout",
            c(
                "The {.arg order} vector must list every node exactly once.",
                i = "Got {length(order)} names for {nrow(nd)} nodes; missing: {.val {head(setdiff(nd$id, order), 5)}}."
            )
        )
    }
    nd[match(order, nd$id), , drop = FALSE]
}

#' @keywords internal
#' @noRd
circos_hclust_order <- function(nd, links) {
    ids <- nd$id
    if (nrow(links) == 0L) return(ids)
    w <- matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
    wv <- if ("r" %in% names(links)) links$r else rep(1, nrow(links))
    w[cbind(match(links$from, ids), match(links$to, ids))] <- wv
    w[cbind(match(links$to, ids), match(links$from, ids))] <- wv
    diag(w) <- 0
    d <- 1 - w
    d[d < 0] <- 0
    ids[stats::hclust(stats::as.dist(d))$order]
}

#' @keywords internal
#' @noRd
circos_sectors_table <- function(nd) {
    key <- unique(nd$sector)
    out <- do.call(rbind, lapply(key, function(s) {
        d <- nd[nd$sector == s, , drop = FALSE]
        # a sector is a *run* of adjacent node arcs, so its span is found by
        # walking the ring in the node order rather than by min()/max() of
        # the angles — the latter puts a sector that straddles theta = 0 on
        # the wrong side of the circle (B1)
        sp <- sector_span(d$theta0, d$theta1, d$order)
        data.frame(
            sector = s, theta0 = sp[[1]], theta1 = sp[[2]],
            n_nodes = nrow(d), stringsAsFactors = FALSE
        )
    }))
    # the sector label sits at the middle of the run, not at theta0
    out$theta <- wrap_2pi(out$theta0 + wrap_pi_arc(out$theta0, out$theta1) / 2)
    out$r <- 1
    # theta0 > theta1 is only meaningful on a circle; say so explicitly
    out$crosses_zero <- out$theta0 > out$theta1
    rownames(out) <- NULL
    out[, c("sector", "theta0", "theta1", "theta", "n_nodes", "r", "crosses_zero")]
}

#' Span of one sector given its node arcs, in ring order
#'
#' `min()`/`max()` are meaningless on a circle: a sector that straddles 0
#' would get the *rest* of the turn. Instead the arcs are merged along the
#' ring, so the result is `theta0 > theta1` when the sector crosses 0.
#' @keywords internal
#' @noRd
sector_span <- function(theta0, theta1, order) {
    ord <- order(order)
    t0 <- theta0[ord]
    t1 <- theta1[ord]
    n <- length(t0)
    if (n == 1L) return(list(t0[1], t1[1]))
    # gap between the end of one arc and the start of the next; the largest
    # one is where the sector is *not* contiguous on the ring
    ahead <- wrap_2pi(c(t0[-1], t0[1]) - t1)
    k <- which.max(ahead)
    # rotate the arcs so the seam sits last: start just after the gap and
    # walk the whole run round
    i <- ((k %% n) + seq_len(n) - 1L) %% n + 1L
    list(t0[i[1]], t1[i[n]])
}

#' Arc length from `a` to `b` measured the short way round, in `[0, 2pi)`
#'
#' `wrap_pi()` answers "which way is shorter"; this one answers "how long is
#' the short way", which is what a label placed at the run's midpoint needs
#' when the run straddles 0 (`b < a`).
#' @keywords internal
#' @noRd
wrap_pi_arc <- function(a, b) {
    d <- wrap_2pi(b - a)
    d[d > pi] <- 2 * pi - d[d > pi]
    d
}

#' Reserve an angular slot on each endpoint of every link
#'
#' Each node arc is split evenly among its incident links; the used portion is
#' `slot_fill` of the even share, centred inside it. The two endpoints are
#' allocated independently, so an edge may leave node A at a different
#' angular position than it enters node B.
#'
#' @param links A data frame with `from`, `to`.
#' @param nodes Node table with `id`, `theta0`, `width`, `order`.
#' @param slot_fill Fraction of each even share that is actually used.
#' @return `links` with slot and coordinate columns added.
#' Turn the `weight` argument into a validated numeric weight per link
#'
#' Widths only make sense for non-negative numbers, so the absolute value is
#' used (a correlation of -0.8 carries as much link as +0.8, which is what
#' circlize does too).
#' @keywords internal
#' @noRd
resolve_link_weights <- function(links, weight) {
    if (is.null(weight)) return(rep(1, nrow(links)))
    if (!is.character(weight) || length(weight) != 1L) {
        ggcircos_abort("data", "{.arg weight} must be one column name, e.g. {.code weight = \"r\"}.")
    }
    if (!weight %in% names(links)) {
        ggcircos_abort(
            "data",
            c("{.arg weight} refers to {.field {weight}}, which the links do not have.",
              i = "Available: {.val {setdiff(names(links), \".circos_w\")}}.")
        )
    }
    v <- links[[weight]]
    if (!is.numeric(v)) {
        ggcircos_abort("data", "{.field {weight}} must be numeric, not {.cls {class(v)}}.")
    }
    if (anyNA(v) || any(!is.finite(v))) {
        ggcircos_abort("data", "{.field {weight}} has missing or non-finite values.")
    }
    abs(as.numeric(v))
}

#' @keywords internal
#' @noRd
allocate_slots <- function(links, nodes, slot_fill = 0.9) {
    n <- nrow(nodes)
    if (nrow(links) == 0L) {
        links$edge_id <- integer(0)
        links$theta <- numeric(0); links$theta_end <- numeric(0)
        links$theta0 <- numeric(0); links$theta1 <- numeric(0)
        links$theta_end0 <- numeric(0); links$theta_end1 <- numeric(0)
        links$x <- numeric(0); links$y <- numeric(0)
        links$xend <- numeric(0); links$yend <- numeric(0)
        return(links)
    }
    links$edge_id <- seq_len(nrow(links))

    ep <- rbind(
        data.frame(
            row = seq_len(nrow(links)), edge_id = links$edge_id,
            node = links$from, partner = links$to, side = "from",
            stringsAsFactors = FALSE
        ),
        data.frame(
            row = seq_len(nrow(links)), edge_id = links$edge_id,
            node = links$to, partner = links$from, side = "to",
            stringsAsFactors = FALSE
        )
    )
    ep$node_order <- match(ep$node, nodes$id)
    ep$partner_order <- match(ep$partner, nodes$id)
    ep <- ep[order(ep$node_order, ep$side, ep$partner_order, ep$edge_id), , drop = FALSE]

    # Slots are allocated *per side* (spec §4.6): a node's arc is split among
    # the links on that side only, so 3 outgoing and 1 incoming edge use
    # thirds on the from side and the whole arc on the to side. Dividing by
    # the node's total degree instead leaves the spare capacity unused.
    side_order <- ep$node_order + ifelse(ep$side == "to", n, 0L)
    n_slots <- pmax(tabulate(side_order, nbins = 2L * n), 1L)
    ep$k <- sequence(n_slots)[rank(side_order, ties.method = "first")]

    # A slot takes `slot_fill` of its share of the side, and its share is its
    # link's weight over the side's total; the free `1 - slot_fill` becomes
    # k equal gaps (the first and last halved). With equal weights this is
    # bit-identical to `theta0 + s*(k-0.5) +/- p*s/2`, which is what the
    # unweighted layout used before.
    ep$w <- links$.circos_w[ep$row]
    side_total <- stats::ave(ep$w, side_order, FUN = sum)
    share <- ifelse(side_total > 0, ep$w / side_total, 1 / n_slots[side_order])
    node_w <- nodes$width[ep$node_order]
    ep$slot_w <- node_w * slot_fill * share
    gap <- node_w * (1 - slot_fill) / n_slots[side_order]
    before <- stats::ave(ep$slot_w + gap, side_order,
                         FUN = function(v) c(0, cumsum(v)[-length(v)]))
    start <- nodes$theta0[ep$node_order] + gap / 2 + before

    ep$s0 <- wrap_2pi(start)
    ep$s1 <- wrap_2pi(start + ep$slot_w)
    ep$mid <- wrap_2pi(start + ep$slot_w / 2)

    from_tab <- ep[ep$side == "from", , drop = FALSE][order(ep$row[ep$side == "from"]), ]
    to_tab <- ep[ep$side == "to", , drop = FALSE][order(ep$row[ep$side == "to"]), ]

    links$theta0 <- from_tab$s0
    links$theta1 <- from_tab$s1
    links$theta <- from_tab$mid
    links$theta_end0 <- to_tab$s0
    links$theta_end1 <- to_tab$s1
    links$theta_end <- to_tab$mid
    links$x <- nodes$r[match(links$from, nodes$id)]
    links$y <- links$theta
    links$xend <- nodes$r[match(links$to, nodes$id)]
    links$yend <- links$theta_end
    links$slot_span <- (from_tab$s1 - from_tab$s0) %% (2 * pi)
    links
}

#' @keywords internal
#' @noRd
circos_track_bands <- function(tracks, track_offset, radius = 1) {
    empty <- data.frame(
        track = character(0), width = numeric(0),
        r0 = numeric(0), r1 = numeric(0), stringsAsFactors = FALSE
    )
    if (is.null(tracks) || length(tracks) == 0L) return(empty)
    if (is.null(names(tracks)) || any(names(tracks) == "")) {
        ggcircos_abort(
            "band",
            "{.arg tracks} must be a *named* numeric vector, e.g. {.code c(values = 0.2, heat = 0.1)}."
        )
    }
    if (any(tracks < 0)) {
        ggcircos_abort("band", "{.arg tracks} widths must be non-negative.")
    }
    # r0_i = radius + offset + sum_{j<i}(width_j + offset): every band is
    # preceded by *all* the earlier ones. `head(tracks, -1)` would only add
    # the immediately previous width, so the third band landed on the second
    # (B18: two tracks looked fine, three overlapped).
    steps <- cumsum(tracks + track_offset)
    r0 <- radius + track_offset + c(0, head(steps, -1))
    data.frame(
        track = names(tracks), width = as.numeric(tracks),
        r0 = r0, r1 = r0 + as.numeric(tracks), stringsAsFactors = FALSE
    )
}

#' @keywords internal
#' @noRd
check_layout_invariants <- function(nd, links, g, sg) {
    in_turn <- function(v) all(v >= 0 & v < 2 * pi)
    # The arcs plus the *actual* seams must close the turn. The old check
    # recomputed the budget formula from the numbers that produced it, so it
    # was a tautology and could not catch an overflow (review R7).
    nxt <- c(nd$theta0[-1], nd$theta0[1])
    gaps <- wrap_2pi(nxt - nd$theta1)
    seam_ok <- all(abs(gaps - g) < 1e-9 | abs(gaps - (g + sg)) < 1e-9)
    closes <- abs(sum(nd$width) + sum(gaps) - 2 * pi) < 1e-9
    if (!(in_turn(nd$theta) && in_turn(nd$theta0) && in_turn(nd$theta1) &&
          seam_ok && closes)) {
        ggcircos_abort(
            "internal",
            c("The layout broke an invariant: the arcs and seams do not close a turn.",
              i = "This is a ggcircos bug; please report it with the data that triggered it.")
        )
    }
    if (nrow(links) > 0L) {
        arc <- (links$theta1 - links$theta0) %% (2 * pi)
        stopifnot(
            in_turn(links$theta0), in_turn(links$theta1),
            in_turn(links$theta_end0), in_turn(links$theta_end1)
        )
        stopifnot(all(arc <= nd$width[match(links$from, nd$id)] + 1e-9))
        stopifnot(all(
            ((links$theta_end1 - links$theta_end0) %% (2 * pi)) <=
                nd$width[match(links$to, nd$id)] + 1e-9
        ))
        stopifnot(all(arc <= links$slot_span + 1e-9))
    }
    invisible(TRUE)
}

#' Node table of a layout
#'
#' @param x A `circos_layout`.
#' @return A data frame; see [circos_layout()] for the column contract.
#' @rdname layout_nodes
#' @export
layout_nodes <- function(x) x$nodes

#' @rdname layout_nodes
#' @export
layout_links <- function(x) x$links

#' @rdname layout_nodes
#' @export
layout_sectors <- function(x) x$sectors

#' @rdname layout_nodes
#' @export
layout_tracks <- function(x) x$tracks

#' @rdname layout_nodes
#' @export
layout_params <- function(x) x$params

#' The layout carried by a plot
#'
#' Layers read the layout through this function and never modify it.
#'
#' @param plot A ggplot, a data frame, or a `circos_layout`.
#' @return A `circos_layout`, or `NULL` when there is none.
#' @examples
#' p <- ggcircos(cor(mtcars))
#' circos_layout_of(p)
#' @export
circos_layout_of <- function(plot) {
    if (inherits(plot, "circos_layout")) return(plot)
    if (is.data.frame(plot)) return(attr(plot, "circos_layout", exact = TRUE))
    d <- plot$data
    if (!is.data.frame(d)) return(NULL)
    attr(d, "circos_layout", exact = TRUE)
}

#' @export
print.circos_layout <- function(x, ...) {
    cat(
        "<ggcircos layout>\n",
        "  nodes:   ", nrow(x$nodes), "\n",
        "  links:   ", nrow(x$links), "\n",
        "  sectors: ", nrow(x$sectors), "\n",
        "  tracks:  ",
        if (nrow(x$tracks)) paste(x$tracks$track, collapse = ", ") else "<none>",
        "\n",
        sep = ""
    )
    invisible(x)
}