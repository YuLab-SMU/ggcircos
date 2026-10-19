#' Baseline snapshots of the intended visuals
#'
#' These are the reference images the M1.5 overhaul (notes/spec.md §14.4) is
#' checked against. Every case below is a case the spec demands, and each one
#' states the invariants it pins down:

#' - static:      chord + node layers, hclust order, gradient2 fill scale
#' - arc:         linear layout shares the same layer set
#' - facets:      one ring per facet group, shared legend and scale
#' - tracks:      three stacked tracks with disjoint radius bands
#' - sectors:     two genomes as sectors, gap between blocks, sector labels
#' - slot-fill:   slot_fill leaves visible gaps between ribbon feet
#' - axis:        track axis rings, ticks and break labels
#' - reversed:    clockwise = FALSE mirrors the circle, labels follow
#' - white:       theme_circos() sets the panel background too
#' - deprecated:  the old API still draws the intended figure

test_that("baseline visuals do not drift", {
    skip_if_not_installed("vdiffr")
    skip_on_cran()
    library(vdiffr)
    gradient2 <- ggplot2::scale_fill_gradient2
    wrap <- ggplot2::facet_wrap
    col <- ggplot2::geom_col
    set.seed(1)

    m <- cor(mtcars)
    expect_doppelganger("basic chord diagram", {
        ggcircos(m, order = "hclust") +
            geom_chord(aes(fill = r), alpha = 0.5) +
            geom_node_arc(width = 0.05) +
            geom_node_text(offset = 0.08) +
            gradient2(midpoint = 0) +
            theme_circos()
    })

    expect_doppelganger("linear arc layout", {
        ggcircos(m, order = "hclust", layout = "arc") +
            geom_chord(aes(fill = r), alpha = 0.5, type = "line")
    })

    # one complete ring per facet panel from a single ggcircos() layout
    n <- 12L
    ids <- paste0("n", seq_len(n))
    lk <- expand.grid(from = ids, to = ids, stringsAsFactors = FALSE)
    lk <- lk[lk$from != lk$to, ]
    lk$g <- ifelse(match(lk$from, ids) %% 2L == 1L, "odd", "even")
    expect_doppelganger("facets share the layout", {
        ggcircos(lk) +
            geom_chord(linetype = "solid", linewidth = 0.4) +
            geom_node_arc(width = 0.04) +
            wrap(~g)
    })

    expect_doppelganger("stacked tracks", {
        vals <- data.frame(
            id = rep(colnames(mtcars), each = 4L),
            value = runif(ncol(mtcars) * 4L),
            stringsAsFactors = FALSE
        )
        ggcircos(m, order = "hclust", tracks = c(values = 0.2, heat = 0.12)) +
            geom_chord(aes(fill = r), alpha = 0.35) +
            geom_track(
                data = vals, aes(x = value, fill = value),
                node = id, geom = "col", band = "values"
            ) +
            geom_track(
                data = vals, aes(fill = value),
                node = id, geom = "tile", band = "heat"
            )
    })

    ids4 <- paste0("g", 1:8)
    nodes4 <- data.frame(
        id = ids4,
        genome = c(rep("ref", 4L), rep("qry", 4L)),
        stringsAsFactors = FALSE
    )
    links4 <- data.frame(
        from = c("g4", "g5", "g1", "g8"),
        to = c("g7", "g2", "g6", "g3"),
        genome = c("ref->qry", "qry->ref", "ref->qry", "qry->ref"),
        stringsAsFactors = FALSE
    )
    expect_doppelganger("sectors and sector labels", {
        ggcircos(
            links4, nodes = nodes4, sectors = "genome",
            gap = 0.004, sector_gap = 0.12
        ) +
            geom_chord(aes(fill = genome), alpha = 0.45) +
            geom_node_arc(aes(fill = genome), width = 0.05) +
            geom_sector_label(offset = 0.12)
    })

    expect_doppelganger("slot fill leaves gaps between feet", {
        ggcircos(m, order = "hclust", gap = 0.01, slot_fill = 0.5) +
            geom_chord(aes(fill = r), alpha = 0.5) +
            geom_node_arc(width = 0.04)
    })

    expect_doppelganger("track axis", {
        vals <- data.frame(
            id = rep(colnames(mtcars), each = 4L),
            value = runif(ncol(mtcars) * 4L),
            stringsAsFactors = FALSE
        )
        ggcircos(m, order = "hclust", tracks = c(values = 0.2)) +
            geom_track(
                data = vals, aes(x = value),
                node = id, geom = "col", band = "values"
            ) +
            geom_track_axis(band = "values", limits = c(0, 1))
    })

    expect_doppelganger("counter-clockwise layout", {
        ggcircos(m, order = "hclust") +
            geom_chord(aes(fill = r), alpha = 0.5) +
            geom_node_text(offset = 0.08) +
            gradient2(midpoint = 0) +
            coord_circos(clockwise = FALSE)
    })

    expect_doppelganger("theme_circos paints the panel", {
        ggcircos(m, order = "hclust", gap = 0.02) +
            geom_chord(aes(fill = r)) +
            theme_circos(bgcolor = "white")
    })

    expect_doppelganger("deprecated wrappers still draw", {
        vals <- data.frame(
            id = colnames(mtcars),
            value = runif(ncol(mtcars)),
            stringsAsFactors = FALSE
        )
        ggcircos(m, order = "hclust", tracks = c(values = 0.2)) +
            suppressWarnings(geom_curve_link(aes(fill = r))) +
            suppressWarnings(geom_circos(
                data = vals, mapping = ggplot2::aes(x = value, y = id),
                geom = col()
            )) +
            suppressWarnings(geom_link_lab())
    })
})
