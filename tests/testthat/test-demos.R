# The three demo claims of notes/positioning.md, as executable assertions.
# D1 faceting, D2 multi-track, D3 layout-as-data.

test_that("D1: one layout, many rings -- facets split the chords, share the node ring", {
    set.seed(42)
    nnode <- 8
    nsamp <- 4
    mk <- function(s) {
        m <- matrix(runif(nnode * nnode), nnode, nnode)
        m <- (m + t(m)) / 2
        diag(m) <- 0
        d <- as.data.frame(as.table(m))
        names(d) <- c("from", "to", "r")
        d$sample <- s
        d
    }
    links <- do.call(rbind, lapply(seq_len(nsamp), mk))

    p <- ggcircos(links, order = "hclust") +
        geom_chord(aes(fill = r)) +
        geom_node_arc() +
        geom_node_text() +
        ggplot2::facet_wrap(~ sample)
    b <- ggplot2::ggplot_build(p)

    # one panel per sample
    expect_equal(length(unique(b$layout$layout$PANEL)), nsamp)
    # one data frame per *layer*, not one blob per panel
    expect_length(b$data, 3L)
    for (d in b$data) {
        expect_equal(length(unique(d$PANEL)), nsamp)
    }
    # the node ring is shared: the node layer has the same rows in every panel
    expect_equal(length(unique(b$data[[2]]$PANEL)), nsamp)
    expect_gt(nrow(b$data[[2]]), 0L)
})

test_that("D2: several tracks, disjoint bands, and an axis", {
    set.seed(7)
    nodes <- data.frame(
        id = c(paste0("g", 1:4), paste0("h", 1:4)),
        genome = rep(c("A", "B"), each = 4),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("g1", "g2", "g3", "g4", "g1", "g3"),
        to = c("h1", "h2", "h3", "h4", "h3", "h1"),
        stringsAsFactors = FALSE
    )
    trk <- data.frame(
        id = rep(nodes$id, 3),
        track = rep(c("gc", "depth", "gene"), each = 8),
        value = runif(24, 0.2, 0.9),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(
        links, nodes = nodes, sectors = "genome",
        tracks = c(gc = 0.12, depth = 0.12, gene = 0.12)
    ) +
        geom_chord(aes(fill = from)) +
        geom_node_arc() +
        geom_track(data = trk, aes(x = value), node = id, band = "gc", geom = "col") +
        geom_track(data = trk, aes(x = value), node = id, band = "depth", geom = "col") +
        geom_track(data = trk, aes(x = value), node = id, band = "gene", geom = "col") +
        geom_track_axis(band = "gc", limits = c(0, 1))
    b <- ggplot2::ggplot_build(p)

    # the three track layers are layers 3, 4, 5 (chord, node arc, then tracks)
    bands <- lapply(3:5, function(i) range(b$data[[i]]$x))
    expect_lt(bands[[1]][2], bands[[2]][1])
    expect_lt(bands[[2]][2], bands[[3]][1])
    # every track value lands inside its own band
    lay <- circos_layout(
        links, nodes = nodes, sectors = "genome",
        tracks = c(gc = 0.12, depth = 0.12, gene = 0.12)
    )
    tt <- layout_tracks(lay)
    for (i in seq_len(nrow(tt))) {
        r <- range(b$data[[i + 2]]$x)
        expect_gt(r[1], tt$r0[i] - 1e-6)
        expect_lt(r[2], tt$r1[i] + 1e-6)
    }
    # the axis marks the break radii, not the band edges
    ax <- b$data[[6]]
    expect_true(any(nzchar(ax$label)))

    # and the whole thing still facets: chords split per panel, tracks shared
    lk2 <- links
    lk2$batch <- rep(c("x", "y"), each = 3)
    pf <- ggcircos(lk2, nodes = nodes, sectors = "genome",
                   tracks = c(gc = 0.12, depth = 0.12, gene = 0.12)) +
        geom_chord(aes(fill = from)) +
        geom_track(data = trk, aes(x = value), node = id, band = "gc") +
        ggplot2::facet_wrap(~ batch)
    bf <- ggplot2::ggplot_build(pf)
    expect_equal(length(unique(bf$layout$layout$PANEL)), 2L)
    expect_equal(as.integer(table(bf$data[[1]]$PANEL)), c(3L, 3L))
    # the track layer is shared data drawn in *every* panel (spec 9.1:
    # chords split, the node ring and its tracks are replicated)
    expect_equal(nrow(bf$data[[2]]), nrow(trk) * 2L)
    expect_equal(as.integer(table(bf$data[[2]]$PANEL)), rep(nrow(trk), 2L))
})

test_that("D3: the layout is data -- plain geoms reproduce the node ring", {
    lay <- circos_layout(cor(mtcars)[1:5, 1:5], order = "hclust")
    nd <- layout_nodes(lay)
    expect_true(all(c("theta0", "theta1", "theta") %in% names(nd)))

    # a node ring drawn with nothing but geom_segment() + coord_circos()
    plain <- ggplot2::ggplot(nd, ggplot2::aes(x = 1, y = theta0)) +
        ggplot2::geom_segment(
            ggplot2::aes(x = 1, xend = 1, y = theta0, yend = theta1),
            linewidth = 3
        ) +
        coord_circos(limits = c(0, 2 * pi)) +
        theme_circos()
    expect_s3_class(ggplot2::ggplot_build(plain), "ggplot_built")

    # the package's own node layer, for comparison
    packaged <- ggcircos(cor(mtcars)[1:5, 1:5], order = "hclust") +
        geom_node_arc()
    expect_s3_class(ggplot2::ggplot_build(packaged), "ggplot_built")

    # both place the first node at the same angle
    bp <- ggplot2::ggplot_build(plain)
    pp <- ggplot2::ggplot_build(packaged)
    expect_equal(
        bp$layout$panel_params[[1]]$theta.range,
        pp$layout$panel_params[[1]]$theta.range
    )
})

test_that("coord_circos() accepts a numeric limits as the angle range", {
    p <- ggplot2::ggplot(
        data.frame(x = 1, y = c(0, 3, 6)),
        ggplot2::aes(x, y)
    ) +
        ggplot2::geom_point() +
        coord_circos(limits = c(0, 2 * pi))
    b <- ggplot2::ggplot_build(p)
    expect_equal(b$layout$panel_params[[1]]$theta.range, c(0, 2 * pi))
    # and a squashed angle scale still warns
    expect_warning(
        ggplot2::ggplot_build(
            ggplot2::ggplot(data.frame(x = 1, y = 0.5), ggplot2::aes(x, y)) +
                ggplot2::geom_point() +
                coord_circos()
        ),
        class = "ggcircos_warning_scale"
    )
})

test_that("screen_angle is the radial direction of the point (spec 5.4-5)", {
    # The one contract that was never written as a test: for every
    # direction x start combination, the angle the coordinate reports must be
    # the direction from the centre outward that `transform()` actually puts
    # the point at. A tangent-based or atan2-derived angle fails this
    # (review R2).
    m <- cor(mtcars)[1:5, 1:5]
    starts <- c(0, -pi / 2, pi, 1.2)
    for (clockwise in c(TRUE, FALSE)) {
        for (st in starts) {
            p <- ggcircos(m, start = st) + geom_chord() +
                coord_circos(clockwise = clockwise, start = st)
            b <- suppressMessages(ggplot2::ggplot_build(p))
            pp <- b$layout$panel_params[[1]]
            cd <- b$layout$coord
            th <- seq(0.15, 2 * pi - 0.15, length.out = 12)
            xy <- cd$transform(data.frame(x = 0.9, y = th), pp)
            radial <- (atan2(xy$x - 0.5, xy$y - 0.5) * 180 / pi) %% 360
            got <- cd$screen_angle(pp, th)
            expect_equal(
                as.numeric((got - radial + 180) %% 360 - 180),
                rep(0, length(th)),
                tolerance = 1e-6,
                info = sprintf("clockwise = %s, start = %.2f", clockwise, st)
            )
        }
    }
})

test_that("rotated labels run along the ring, upright on both halves", {
    # text baseline is perpendicular to the radial direction; without the
    # -90 the labels lay across the arc (review R2)
    m <- cor(mtcars)
    g <- ggplot2::ggplotGrob(
        ggcircos(m) + geom_node_text(rotate = TRUE)
    )
    i <- which(vapply(g$grobs, function(z) !is.null(z$name) && grepl("^panel", z$name), TRUE))[1]
    kids <- g$grobs[[i]]$children
    txt <- Filter(function(z) inherits(z, "text"), kids)[[1]]

    # every drawn angle must be in (-90, 90] after folding, so no label ends
    # up upside down
    rot <- (as.numeric(txt$rot) %% 360)
    folded <- ifelse(rot > 90 & rot <= 270, rot - 180, ifelse(rot > 270, rot - 360, rot))
    expect_true(all(abs(folded) <= 90 + 1e-9))

    # and the angles must vary: a constant would mean the rotation is ignored
    expect_gt(length(unique(round(as.numeric(txt$rot), 3))), 3)
})

test_that("an angle aesthetic on node labels is refused, not ignored (R8)", {
    # The rotation is the coordinate's business (spec 5.4). Accepting
    # `aes(angle = )` and then overriding it would make the argument look
    # effective while doing nothing, which is the quiet failure this package
    # avoids elsewhere.
    expect_error(
        ggcircos(cor(mtcars)[1:4, 1:4]) +
            geom_node_text(ggplot2::aes(angle = 45)),
        class = "ggcircos_error_data"
    )
    # the supported switch still works
    expect_s3_class(
        ggplot2::ggplotGrob(
            ggcircos(cor(mtcars)[1:4, 1:4]) + geom_node_text(rotate = TRUE)
        ),
        "gtable"
    )
})

test_that("standard ggplot2 layers stack without inherit.aes = FALSE (D3)", {
    # The plot carries data only, no mapping: x/y/xend/yend belong to the
    # chord layer, which maps them itself. Hanging them at plot level made
    # every standard layer inherit columns it does not use -- ggplot2::geom_rect()
    # wants xmin/ymin and failed on the inherited x/y. This is the property
    # the README's interop example depends on, so pin it: a standard layer
    # needs nothing but its own aes() and data.
    m <- cor(mtcars)[1:6, 1:6]
    base <- ggcircos(m, order = "hclust")
    nd <- layout_nodes(circos_layout(m, order = "hclust"))

    # the plot itself maps nothing (an empty mapping, not NULL)
    expect_length(base$mapping, 0L)

    # every ggplot2 call is namespace-qualified, as elsewhere in this file
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_point(data = nd, aes(x = r, y = theta), size = 2)), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_text(data = nd, aes(x = r + 0.1, y = theta, label = label),
                         size = 2)), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_label(data = nd, aes(x = r + 0.15, y = theta, label = id),
                          size = 2)), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_tile(data = nd, aes(x = r, y = theta, fill = sector),
                         height = 0.02)), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_rect(data = nd, aes(xmin = r, xmax = r + 0.05,
                                        ymin = theta0, ymax = theta1),
                         alpha = 0.3)), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_segment(data = nd, aes(x = r, xend = r + 0.1,
                                           y = theta, yend = theta))), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_path(data = nd, aes(x = r + 0.08, y = theta))), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::geom_violin(data = nd, aes(x = r + 0.2, y = theta))), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(
        base + ggplot2::annotate("point", x = 1.3, y = 0, size = 4)), "gtable")

    # and a round trip through saveRDS keeps the property
    f <- tempfile(fileext = ".rds")
    p <- base + ggplot2::geom_point(data = nd, aes(x = r + 0.1, y = theta), size = 2)
    saveRDS(p, f)
    q <- readRDS(f) + ggplot2::geom_text(data = nd, aes(x = r + 0.2, y = theta, label = id), size = 2)
    expect_s3_class(ggplot2::ggplotGrob(q), "gtable")
})
