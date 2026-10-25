# Genomic interval tracks (spec sec.4.11)
#
# A feature is a range, not a point: `chr + start + end`. The range occupies an
# angular span inside its node's arc; the radius stays the value's job.

make_interval_plot <- function(trk, ...) {
    links <- data.frame(from = c("chr1", "chr2"), to = c("chr2", "chr1"),
                        stringsAsFactors = FALSE)
    nodes <- data.frame(id = c("chr1", "chr2"), len = c(1000, 500),
                        stringsAsFactors = FALSE)
    lay <- circos_layout(links, nodes = nodes, tracks = c(g = 0.2))
    list(
        lay = lay,
        plot = ggcircos(lay) +
            geom_track(data = trk, ggplot2::aes(xmin = start, xmax = end),
                       node = id, band = "g", domain = len, ...)
    )
}

test_that("start = 0, end = L fills the node's whole arc (4.11-1)", {
    trk <- data.frame(id = c("chr1", "chr2"), start = 0, end = c(1000, 500),
                      stringsAsFactors = FALSE)
    o <- make_interval_plot(trk, width = 1)
    nd <- layout_nodes(o$lay)
    d <- ggplot2::ggplot_build(o$plot)$data[[1]]
    expect_equal(sort(c(d$ymin[1], d$ymax[1])), sort(c(nd$theta0[1], nd$theta1[1])),
                 tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[2], d$ymax[2])), sort(c(nd$theta0[2], nd$theta1[2])),
                 tolerance = 1e-9)
})

test_that("each node normalises on its own length (4.11-3)", {
    # chr1 is 1000 long, chr2 is 500: the same half-length interval must fill
    # half of each own arc, not half of a shared scale
    trk <- data.frame(id = c("chr1", "chr2"), start = 0, end = c(500, 250),
                      stringsAsFactors = FALSE)
    o <- make_interval_plot(trk, width = 1)
    nd <- layout_nodes(o$lay)
    d <- ggplot2::ggplot_build(o$plot)$data[[1]]
    half_arc <- (nd$theta1 - nd$theta0) / 2
    expect_equal(d$ymax - d$ymin, half_arc, tolerance = 1e-9)
})

test_that("without domain the node's own xmax sets the scale (4.11-2)", {
    # no `len` column: each node normalises on the largest bound it reports
    links <- data.frame(from = c("chr1", "chr2"), to = c("chr2", "chr1"),
                        stringsAsFactors = FALSE)
    lay <- circos_layout(links, nodes = data.frame(id = c("chr1", "chr2"),
                                                   stringsAsFactors = FALSE),
                         tracks = c(g = 0.2))
    trk <- data.frame(id = c("chr1", "chr1", "chr2"),
                      start = c(0, 0, 0), end = c(1000, 400, 500),
                      stringsAsFactors = FALSE)
    d <- ggplot2::ggplot_build(
        ggcircos(lay) + geom_track(data = trk, ggplot2::aes(xmin = start, xmax = end),
                                   node = id, band = "g", width = 1)
    )$data[[1]]
    nd <- layout_nodes(lay)
    # chr1's 0-1000 row fills its arc; its 0-400 row fills 40% of it
    span_chr1 <- d$ymax[1] - d$ymin[1]
    span_chr1_short <- d$ymax[2] - d$ymin[2]
    expect_equal(span_chr1, (nd$theta1[1] - nd$theta0[1]), tolerance = 1e-9)
    expect_equal(span_chr1_short / span_chr1, 0.4, tolerance = 1e-9)
})

test_that("an interval row is not divided by its neighbours (4.11-5b)", {
    # The even-division path gives a row `1/m` of the arc, so a row that is
    # 20% of the chromosome would still take half the arc when there are two
    # rows. Assert the *proportion*, not just that adding a row changed
    # nothing: 0-200 on a 1000-long chr1 is 20% of the arc, never 50%.
    nd <- layout_nodes(make_interval_plot(
        data.frame(id = "chr1", start = 0, end = 200, stringsAsFactors = FALSE)
    )$lay)
    arc <- nd$theta1[1] - nd$theta0[1]
    one <- data.frame(id = "chr1", start = 0, end = 200, stringsAsFactors = FALSE)
    two <- data.frame(id = c("chr1", "chr1"), start = c(0, 600), end = c(200, 800),
                      stringsAsFactors = FALSE)
    d1 <- ggplot2::ggplot_build(make_interval_plot(one, width = 1)$plot)$data[[1]]
    d2 <- ggplot2::ggplot_build(make_interval_plot(two, width = 1)$plot)$data[[1]]
    # 20% of the arc in both cases -- the even split would give 100% and 50%
    expect_equal((d1$ymax[1] - d1$ymin[1]) / arc, 0.2, tolerance = 1e-9)
    expect_equal((d2$ymax[1] - d2$ymin[1]) / arc, 0.2, tolerance = 1e-9)
    # and the second interval lands where its coordinates say, not next to the
    # first: 60% along the arc
    expect_equal((d2$ymin[2] - nd$theta0[1]) / arc, 0.6, tolerance = 1e-9)
})

test_that("interval inputs are validated (4.11: errors)", {
    trk <- data.frame(id = "chr1", start = 0, end = 500, e = 1L,
                      stringsAsFactors = FALSE)
    expect_error(make_interval_plot(trk, slot = e)$plot, class = "ggcircos_error_data")
    expect_error(make_interval_plot(trk)$plot, NA)   # the baseline is fine
    expect_error(
        ggcircos(circos_layout(data.frame(from = "chr1", to = "chr2"),
                               nodes = data.frame(id = c("chr1", "chr2")),
                               tracks = c(g = 0.2))) +
            geom_track(data = trk, ggplot2::aes(xmin = start, xmax = end),
                       node = id, band = "g", domain = nope),
        class = "ggcircos_error_data"
    )
    rev <- data.frame(id = "chr1", start = 900, end = 100, stringsAsFactors = FALSE)
    expect_error(make_interval_plot(rev, width = 1)$plot, class = "ggcircos_error_data")
    half <- data.frame(id = "chr1", start = 0, end = 500, stringsAsFactors = FALSE)
    expect_error(
        ggcircos(circos_layout(data.frame(from = "chr1", to = "chr2"),
                               nodes = data.frame(id = c("chr1", "chr2")),
                               tracks = c(g = 0.2))) +
            geom_track(data = half, ggplot2::aes(xmin = start), node = id,
                       band = "g", domain = id),
        class = "ggcircos_error_data"
    )
})

test_that("intervals normalise per panel on a by= plot (4.11 x 9.1)", {
    # Each panel is its own ring, so a 1000-long chromosome in P1 and a
    # 500-long one in P2 must both fill their own arc. Scaling them on one
    # shared axis would make the short one look half as long for no reason.
    nodes <- data.frame(id = c("a1", "b1", "a2", "b2"),
                        g = rep(c("P1", "P2"), each = 2),
                        len = c(1000, 1000, 500, 500),
                        stringsAsFactors = FALSE)
    links <- data.frame(from = c("a1", "a2"), to = c("b1", "b2"),
                        g = c("P1", "P2"), stringsAsFactors = FALSE)
    trk <- data.frame(id = c("a1", "a2"), g = c("P1", "P2"),
                      start = c(0, 0), end = c(1000, 500),
                      stringsAsFactors = FALSE)
    p <- ggcircos(links, nodes = nodes, by = "g", tracks = c(f = 0.2)) +
        geom_track(data = trk, ggplot2::aes(xmin = start, xmax = end),
                   node = id, band = "f", domain = len, width = 1)
    d <- ggplot2::ggplot_build(p)$data[[1]]
    expect_equal(nrow(d), 2L)
    # both are "the whole chromosome", so both spans are one full node arc
    expect_equal(d$ymax[1] - d$ymin[1], d$ymax[2] - d$ymin[2], tolerance = 1e-9)
    # and each sits inside its own panel's arcs, not the other's
    lays <- circos_layout(links, nodes = nodes, by = "g")
    a1 <- layout_nodes(lays[["P1"]]); a2 <- layout_nodes(lays[["P2"]])
    expect_gte(d$ymin[1], a1$theta0[1] - 1e-9)
    expect_lte(d$ymax[1], a1$theta1[1] + 1e-9)
    expect_gte(d$ymin[2], a2$theta0[1] - 1e-9)
    expect_lte(d$ymax[2], a2$theta1[1] + 1e-9)
})

test_that("a half-length interval is half the arc in every panel", {
    nodes <- data.frame(id = c("a1", "b1", "a2", "b2"),
                        g = rep(c("P1", "P2"), each = 2),
                        len = c(1000, 1000, 500, 500),
                        stringsAsFactors = FALSE)
    links <- data.frame(from = c("a1", "a2"), to = c("b1", "b2"),
                        g = c("P1", "P2"), stringsAsFactors = FALSE)
    # 50% of each chromosome, expressed in that chromosome's own units
    trk <- data.frame(id = c("a1", "a2"), g = c("P1", "P2"),
                      start = c(0, 0), end = c(500, 250),
                      stringsAsFactors = FALSE)
    p <- ggcircos(links, nodes = nodes, by = "g", tracks = c(f = 0.2)) +
        geom_track(data = trk, ggplot2::aes(xmin = start, xmax = end),
                   node = id, band = "f", domain = len, width = 1)
    d <- ggplot2::ggplot_build(p)$data[[1]]
    spans <- d$ymax - d$ymin
    expect_equal(spans[1], spans[2], tolerance = 1e-9)
    lays <- circos_layout(links, nodes = nodes, by = "g")
    arc <- function(l) {
        nd <- layout_nodes(l); nd$theta1[1] - nd$theta0[1]
    }
    expect_equal(spans[1] / arc(lays[["P1"]]), 0.5, tolerance = 1e-9)
    expect_equal(spans[2] / arc(lays[["P2"]]), 0.5, tolerance = 1e-9)
})
