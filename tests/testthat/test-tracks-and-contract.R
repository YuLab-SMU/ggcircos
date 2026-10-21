test_that("track layers build in their declared bands", {
    set.seed(1)
    vals <- data.frame(
        id = rep(colnames(mtcars), each = 4),
        value = runif(ncol(mtcars) * 4),
        stringsAsFactors = FALSE
    )
    base <- ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.18, heat = 0.12))

    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", geom = "col", band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(fill = value), node = "id", geom = "tile", band = "heat")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", geom = "point", band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(label = round(value, 1)), node = "id", geom = "text", band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", geom = "area", band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", geom = "boxplot", band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", geom = "col", band = "values", stack = TRUE, fill = "steelblue")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", band = "values", scales = "fixed", limits = c(0, 1))),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(
            base +
                geom_track(data = vals, aes(x = value), node = "id", band = "values") +
                geom_track_axis(band = "values", limits = c(0, 1))
        ),
        "gtable"
    )

    p <- base +
        geom_track(data = vals, aes(x = value), node = "id", band = "values") +
        geom_track(data = vals, aes(fill = value), node = "id", band = "heat")
    b <- ggplot2::ggplot_build(p)

    expect_equal(min(b$data[[1]]$xmin), 1.06, tolerance = 1e-6)
    expect_equal(max(b$data[[1]]$xmax), 1.24, tolerance = 1e-6)
    expect_equal(min(b$data[[2]]$xmin), 1.30, tolerance = 1e-6)
    expect_equal(max(b$data[[2]]$xmax), 1.36, tolerance = 1e-6)
})

test_that("tile tracks split each node arc into one bin per row (B7)", {
    set.seed(4)
    vals <- data.frame(
        id = rep(colnames(mtcars), each = 4L),
        value = runif(ncol(mtcars) * 4L),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(cor(mtcars), order = "hclust", tracks = c(heat = 0.15)) +
        geom_track(data = vals, aes(fill = value), node = id, geom = "tile", band = "heat")
    d <- ggplot2::ggplot_build(p)$data[[1]]
    nd <- ggcircos:::layout_nodes(ggcircos:::circos_layout_of(p))

    # the band is filled radially, not collapsed to its centre line
    expect_equal(min(d$xmin), 1.06, tolerance = 1e-6)
    expect_equal(max(d$xmax), 1.21, tolerance = 1e-6)

    # the bins of one node are contiguous and together cover width * arc
    for (id in nd$id) {
        s <- d[which(vals$id == id), ]
        s <- s[order(s$ymin), ]
        expect_equal(sum(s$ymax - s$ymin), 0.9 * nd$width[1], tolerance = 1e-9)
        if (nrow(s) > 1) {
            expect_equal(s$ymin[-1], s$ymax[-nrow(s)], tolerance = 1e-9)
        }
    }
})

test_that("the node column can be unquoted, a string, or a string variable", {
    set.seed(2)
    vals <- data.frame(
        id = colnames(mtcars),
        value = runif(ncol(mtcars)),
        stringsAsFactors = FALSE
    )
    base <- ggcircos(cor(mtcars), tracks = c(values = 0.2))
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = id, band = "values")),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = "id", band = "values")),
        "gtable"
    )
    col <- "id"
    expect_s3_class(
        ggplot2::ggplotGrob(base + geom_track(data = vals, aes(x = value), node = col, band = "values")),
        "gtable"
    )
})

test_that("track layers require explicit track data", {
    expect_error(
        ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
            geom_track(aes(x = mpg), node = cyl, band = "values"),
        class = "ggcircos_error_data"
    )
})

test_that("chord subsets, arc layout and read-only add contract hold", {
    nodes <- data.frame(
        id = c("a", "b", "c", "d"),
        genome = c("ref", "ref", "qry", "qry"),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "c", "a", "d"),
        to = c("d", "b", "c", "b"),
        stringsAsFactors = FALSE
    )
    sub <- data.frame(from = c("a", "c"), to = c("c", "b"), stringsAsFactors = FALSE)

    expect_s3_class(
        ggplot2::ggplotGrob(
            ggcircos(links, nodes = nodes, sectors = "genome") +
                geom_chord(data = links, from = from, to = to, aes(fill = from)) +
                geom_sector_label()
        ),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(ggcircos(links, nodes = nodes, layout = "arc") + geom_chord() + geom_node_text()),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(ggcircos(links, nodes = nodes) + geom_chord(data = sub, from = from, to = to, aes(fill = from))),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(ggcircos(links, nodes = nodes) + geom_chord(data = sub, aes(from = from, to = to))),
        "gtable"
    )
    expect_s3_class(
        ggplot2::ggplotGrob(ggcircos(links, nodes = nodes) + geom_chord(type = "line")),
        "gtable"
    )
    expect_error(
        ggcircos(links, nodes = nodes) + geom_chord(data = data.frame(from = "zz", to = "a"), from = from, to = to),
        class = "ggcircos_error_data"
    )

    set.seed(1)
    vals <- data.frame(id = rep(colnames(mtcars), each = 4), value = runif(44), stringsAsFactors = FALSE)
    base <- ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2))
    before_data <- base$data
    before_scales <- base$scales
    before_n <- length(base$layers)

    p <- base +
        geom_chord(aes(fill = r)) +
        geom_node_arc() +
        geom_track(aes(x = value), node = "id", data = vals, band = "values")

    expect_identical(before_data, base$data)
    expect_identical(before_scales, base$scales)
    expect_equal(before_n, length(base$layers))
    expect_s3_class(ggplot2::ggplotGrob(p), "gtable")

    a <- ggplot2::ggplot_build(base + geom_chord(type = "line") + geom_node_arc())$data
    b <- ggplot2::ggplot_build(base + geom_node_arc() + geom_chord(type = "line"))$data
    expect_identical(a[[2]]$x, b[[1]]$x)
    expect_identical(a[[1]]$y, b[[2]]$y)
})

test_that("rotate reaches the geom (B13)", {
    # `rotate` lives in layer$ggcircos, travels through geom_params, and is
    # consumed by draw_panel(). It used to be dropped by
    # `drop_geometry_params()`, so every label was silently horizontal.
    m <- cor(mtcars)
    on <- ggplot2::ggplot_build(ggcircos(m, order = "hclust") + geom_node_text(rotate = TRUE))$data[[1]]
    off <- ggplot2::ggplot_build(ggcircos(m, order = "hclust") + geom_node_text(rotate = FALSE))$data[[1]]

    # the setting really is carried on the layer, not swallowed by the add path
    tagged <- ggcircos(m) + geom_node_text(rotate = TRUE)
    expect_true("rotate" %in% names(tagged$layers[[1]]$geom_params))
    expect_true(isTRUE(tagged$layers[[1]]$geom_params$rotate))

    # and it is consumed: the rendered text grobs differ between the modes
    rots_of <- function(rot) {
        p <- ggcircos(m, order = "hclust") + geom_node_text(rotate = rot)
        g <- ggplot2::ggplotGrob(p)
        idx <- which(vapply(g$grobs, function(x) !is.null(x$name) && grepl("^panel", x$name), logical(1)))
        panel <- g$grobs[[idx[1]]]
        hit <- which(vapply(panel$children, function(t) {
            inherits(t, "text")
        }, logical(1)))
        if (!length(hit)) return(numeric(0))
        as.numeric(panel$children[[hit[1]]]$rot)
    }
    rot_on <- rots_of(TRUE)
    rot_off <- rots_of(FALSE)
    expect_false(identical(rot_on, rot_off))
    expect_true(all(rot_off == 0))
    expect_false(all(rot_on == 0))
    # a rotated label on the right half must not be upside down
    expect_true(all(abs(((rot_on[cos(rot_on * pi / 180) > 0] + 90) %% 360) - 90) < 90))
})

test_that("rotate is not silently dropped by the add path (B13)", {
    m <- cor(mtcars)
    # the explicit guard from spec §8.2: `rotate` must survive ggplot_add
    expect_false("rotate" %in% ggcircos:::geometry_params)
    expect_false("nudge_x" %in% ggcircos:::geometry_params)
    expect_false("angle" %in% ggcircos:::geometry_params)
})

test_that("the axis draws lines, not white blobs (B8)", {
    # pathGrob's default fill is white: with two rings that painted over
    # every layer under the axis. The grob must be unfilled.
    m <- cor(mtcars)
    p <- ggcircos(m, order = "hclust", tracks = c(values = 0.2)) +
        geom_chord(aes(fill = r)) +
        geom_track_axis(band = "values", limits = c(0, 1))
    g <- ggplot2::ggplotGrob(p)
    idx <- which(vapply(g$grobs, function(x) !is.null(x$name) && grepl("^panel", x$name), logical(1)))
    kids <- g$grobs[[idx[1]]]$children
    ax <- kids[[which(vapply(kids, function(x) !is.null(x$name) && grepl("track_axis", x$name), logical(1)))]]
    lines_grob <- ax$children[[1]]
    expect_true(is.null(lines_grob$gp$fill) || all(is.na(lines_grob$gp$fill)))
    # the chord layer must still be present and visible under the axis
    expect_true(any(vapply(kids, function(x) !is.null(x$name) && grepl("chord", x$name), logical(1))))
    # labels are readable: one per break, at distinct radii
    txt <- ax$children[[2]]
    expect_equal(sort(as.character(txt$label)), c("0.0", "0.5", "1.0"))
    d <- ggplot2::ggplot_build(p)$data[[2]]
    radii <- d$x[nzchar(d$label)]
    expect_equal(length(unique(round(radii, 6))), length(radii))
})

test_that("overlapping ribbons blend instead of punching a white hole (B35)", {
    # Batching links into a single `grid.path()` needs a fill rule, and both
    # are wrong here: `"evenodd"` empties the intersection of two ribbons --
    # a crossing becomes white, which is the "white patch in the middle" bug --
    # and `"winding"` also cancels a link that folds over itself. Each link is
    # therefore drawn as its own grob, the way circlize draws one `polygon()`
    # per link, so overlaps blend by alpha.
    m <- cor(mtcars)
    chord_grobs <- function(p) {
        g <- ggplot2::ggplotGrob(p)
        idx <- which(vapply(g$grobs, function(x) !is.null(x$name) && grepl("^panel", x$name), logical(1)))
        ch <- g$grobs[[idx[1]]]$children
        k <- which(vapply(ch, function(x) !is.null(x$name) && grepl("chord", x$name), logical(1)))
        ch[[k]]$children
    }
    n_links <- nrow(ggcircos:::layout_links(ggcircos:::circos_layout(m)))
    expect_gt(n_links, 24L)
    # one grob per link: a crossing cannot be emptied by a shared fill rule
    expect_equal(length(chord_grobs(ggcircos(m, order = "hclust") + geom_chord(aes(fill = r)))), n_links)
})

test_that("graph inputs keep their edge attributes (B19)", {
    skip_if_not_installed("igraph")
    df <- data.frame(from = c("a", "b", "c", "d"),
                     to = c("b", "c", "a", "a"),
                     w = c(3, 1, 2, 4),
                     stringsAsFactors = FALSE)
    g <- igraph::graph_from_data_frame(df)
    lay <- circos_layout(g)
    expect_equal(lay$links$w, df$w)
    expect_equal(lay$links$from, df$from)
    # the value column is usable in a mapping
    p <- ggcircos(g) + geom_chord(aes(fill = w)) + geom_node_arc()
    expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), 4L)

    skip_if_not_installed("tidygraph")
    tg <- tidygraph::as_tbl_graph(g)
    lay2 <- circos_layout(tg)
    expect_equal(lay2$links$from, df$from)
    expect_equal(lay2$links$w, df$w)
})

test_that("plots roundtrip through rds and facet", {
    nodes <- data.frame(
        id = c("a", "b", "c", "d"),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "c", "a", "d"),
        to = c("d", "b", "c", "b"),
        grp = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )
    set.seed(1)
    vals <- data.frame(id = rep(colnames(mtcars), each = 4), value = runif(44), stringsAsFactors = FALSE)
    p <- ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2)) +
        geom_chord(aes(fill = r)) +
        geom_track(aes(x = value), node = "id", data = vals, band = "values")

    f <- tempfile(fileext = ".rds")
    saveRDS(p, f)
    p2 <- readRDS(f)
    expect_s3_class(ggplot2::ggplotGrob(p2 + geom_node_point()), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(ggcircos(links, nodes = nodes) + geom_chord() + ggplot2::facet_wrap(~ grp)), "gtable")
})

test_that("the linear layout keeps every chord inside the panel (R16)", {
    nodes <- data.frame(id = paste0("n", 1:6), stringsAsFactors = FALSE)
    # every link joins two nodes that are half a ring apart, so the shortest
    # arc wraps the seam for half of them
    links <- data.frame(
        from = paste0("n", 1:6),
        to = paste0("n", c(4, 5, 6, 1, 2, 3)),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(links, nodes = nodes, layout = "arc") + geom_chord(type = "line")
    b <- ggplot2::ggplot_build(p)
    pp <- b$layout$panel_params[[1]]
    paths <- ggcircos:::chord_paths_cartesian(b$data[[1]], "line", NA, 40)

    # the linear y is a plain coordinate: no path may leave the turn
    expect_true(all(paths$y >= 0))
    expect_true(all(paths$y <= 2 * pi))
    # and none may leave the radius range
    expect_true(all(paths$x >= pp$x.range[1]))
    expect_true(all(paths$x <= pp$x.range[2]))
    # each chord starts and ends on the spine, at its own two slots
    ends <- do.call(rbind, lapply(split(paths, paths$.id), function(q) {
        data.frame(y0 = q$y[1], y1 = q$y[nrow(q)])
    }))
    expect_true(all(abs(ends$y0 - b$data[[1]]$y) < 1e-9))
    expect_true(all(abs(ends$y1 - b$data[[1]]$yend) < 1e-9))
})

test_that("the linear layout draws arcs at 1:1 units, not stretched loops (R17)", {
    nodes <- data.frame(id = paste0("n", 1:6), stringsAsFactors = FALSE)
    links <- data.frame(
        from = paste0("n", 1:6), to = paste0("n", c(4, 5, 6, 1, 2, 3)),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(links, nodes = nodes, layout = "arc") + geom_chord(type = "line")
    b <- ggplot2::ggplot_build(p)
    pp <- b$layout$panel_params[[1]]
    # the coordinate fixes the aspect, so one radius unit is one radian on
    # screen; `aspect()` reports height/width, i.e. the two ranges' ratio
    expect_equal(
        b$layout$coord$aspect(pp),
        diff(pp$y.range) / diff(pp$x.range)
    )

    # a chord spanning half a turn bulges 0.4 radius units, no more: the depth
    # is a fraction of the span, so arcs stay arcs instead of tall loops
    paths <- ggcircos:::chord_paths_cartesian(b$data[[1]], "line", NA, 40)
    depth <- vapply(split(paths, paths$.id), function(q) -min(q$x), numeric(1))
    span <- vapply(split(paths, paths$.id), function(q) abs(diff(range(q$y))), numeric(1))
    ratio <- depth / span
    expect_true(all(ratio > 0.1 & ratio < 0.2))
})

test_that("no chord folds over the centre (R19)", {
    # a link spanning half a turn used to get a *negative* control radius, which
    # puts a control point on the far side of the centre: the control polygon
    # stops being monotone and the ribbon runs back on itself in the middle
    m <- cor(mtcars)
    p <- ggcircos(m, order = "hclust") + geom_chord()
    b <- ggplot2::ggplot_build(p)
    paths <- ggcircos:::chord_paths_polar(b$data[[1]], "ribbon", NA, 40)

    # every path stays at or outside the centre: a convex combination of
    # non-negative control radii cannot dip below zero
    expect_true(all(paths$x >= -1e-12))
    # and the deepest a chord may reach is the centre, never past it
    expect_true(min(paths$x) >= 0)

    # the same for an explicit curvature (curvature = 1 asks for the centre)
    b2 <- ggplot2::ggplot_build(
        ggcircos(m) + geom_chord(curvature = 1)
    )
    paths2 <- ggcircos:::chord_paths_polar(b2$data[[1]], "ribbon", 1, 40)
    expect_true(all(paths2$x >= -1e-12))
})

test_that("a ribbon keeps its width and lands on both feet (R20)", {
    m <- cor(mtcars)
    p <- ggcircos(m, order = "hclust") + geom_chord()
    b <- ggplot2::ggplot_build(p)
    paths <- ggcircos:::chord_paths_polar(b$data[[1]], "ribbon", NA, 40)
    m4 <- 40L

    for (id in unique(paths$.id)) {
        q <- paths[paths$.id == id, ]
        i <- match(id, b$data[[1]]$group)  # not used; index by id below
        lg <- b$data[[1]][paths$.id[1] == paths$.id, ][1, ]
        # the four blocks: from arc, edge one, to arc, edge two
        arc1 <- q[1:m4, ]
        e1 <- q[(m4 + 1):(2 * m4), ]
        arc2 <- q[(2 * m4 + 1):(3 * m4), ]
        e2 <- q[(3 * m4 + 1):(4 * m4), ]

        # both edges span the same pair of feet, and neither leaves the turn
        expect_true(all(is.finite(e1$x)), info = id)
        # the ribbon never narrows to nothing: the two edges stay apart by at
        # least half the narrower foot, measured in angle
        w <- abs(e1$y - rev(e2$y))
        expect_gt(min(w), 0)
    }

    # the waist of every ribbon is no thinner than half its narrow foot
    d <- b$data[[1]]
    foot <- pmin(abs(d$theta1 - d$theta0), abs(d$theta_end1 - d$theta_end0))
    expect_true(all(foot >= 0))
})

test_that("both edges of a ribbon are the same curve entered from opposite ends (R20)", {
    # the old code gave each edge its own shortest-arc difference, whose signs
    # can disagree; the two edges then ran in opposite directions and pinched
    m <- cor(mtcars)
    lay <- circos_layout(m, order = "hclust")
    lk <- layout_links(lay)
    for (i in seq_len(nrow(lk))) {
        ca <- (lk$theta0[i] + lk$theta1[i]) / 2
        cb <- (lk$theta_end0[i] + lk$theta_end1[i]) / 2
        d <- ggcircos:::ang_diff(ca, cb)
        # the spine takes the shorter arc between the two foot midpoints
        expect_lt(abs(d), pi + 1e-9)
    }
})

test_that("a ribbon outline is a closed band, ported from circlize (B34)", {
    # The outline is a straight port of `circlize::circos.link()`: two
    # quadratic Beziers in the plane between the feet, closed by the two node
    # arcs. Every earlier hand-rolled version (a spine offset in angle, a
    # spine offset in the plane, two edges from opposite corners) produced a
    # recognisably different band -- a pinched waist, a hole in the middle, or
    # edges that crossed -- so what is pinned here is the shape contract
    # itself, plus a numeric check against circlize's own helpers when it is
    # installed.
    m <- cor(mtcars)
    p <- ggcircos(m, order = "hclust") + geom_chord()
    b <- ggplot2::ggplot_build(p)
    d <- b$data[[1]]
    paths <- ggcircos:::chord_paths_polar(d, "ribbon", NA, 40)

    for (id in unique(paths$.id)) {
        q <- paths[paths$.id == id, ]
        # 1. The polygon closes. It is closed in the *plane*, so the angles may
        #    differ by a whole turn: a link whose feet face each other passes
        #    through the centre and its unwrapped angle winds once.
        dx <- q$x[1] - q$x[nrow(q)]
        dy_lin <- q$y[1] - q$y[nrow(q)]
        dy_wrapped <- ggcircos:::wrap_pi(dy_lin)
        expect_lt(abs(dx), 1e-9)
        expect_lt(abs(dy_wrapped), 1e-9)
        # 2. It never leaves the ring.
        expect_true(all(q$x <= 1 + 1e-9))
        expect_true(all(q$x >= 0))
        # 3. No whole-turn spike: `atan2()` alone leaves one at the seam, and
        #    a spike draws a wedge straight across the figure.
        expect_lt(max(abs(diff(q$y))), pi)
        # 4. Both feet are attached: the arc vertices really sit at r = 1.
        expect_gt(sum(abs(q$x - 1) < 1e-9), 4L)
        # 5. The band has area but is not the whole disc (a wrong pairing
        #    between corners produces exactly 2*pi -- the entire circle).
        area <- {
            x <- q$x * cos(q$y); y <- q$x * sin(q$y); n <- length(x)
            abs(sum(x[-n] * y[-1] - x[-1] * y[-n])) / 2
        }
        expect_gt(area, 1e-3)
        expect_lt(area, 0.9 * pi)
    }
})

test_that("a ribbon covers a band's worth of area, not the whole disc (B34)", {
    # The failure mode this pins down: sweeping the closing arcs in the
    # increasing direction (as `circlize:::arc.points()` does) turns a foot a
    # few degrees wide into a 350-degree detour whenever the two corners
    # arrive in the other order. The polygon then encloses the whole disc --
    # measured area 6.0-6.2 per link (2*pi = 6.28) -- and every ribbon looks
    # like a filled circle. circlize's own ribbon for one link measures 0.24
    # on the unit circle.
    m <- cor(mtcars)
    p <- ggcircos(m, order = "hclust") + geom_chord()
    d <- ggplot2::ggplot_build(p)$data[[1]]
    area <- function(q) {
        x <- q[, 1]; y <- q[, 2]; n <- length(x)
        abs(sum(x[-n] * y[-1] - x[-1] * y[-n])) / 2
    }
    ar <- vapply(seq_len(nrow(d)), function(i) {
        area(ggcircos:::chord_ribbon_outline(
            d$theta0[i], d$theta1[i], d$theta_end0[i], d$theta_end1[i]
        ))
    }, numeric(1))
    expect_true(all(ar > 1e-3))
    # a band, not the disc: well under half of pi, which is what a ribbon that
    # encloses the centre would reach
    expect_true(all(ar < 0.5 * pi))
    expect_lt(stats::median(ar), 0.3)
})

test_that("free track scales are normalised independently per node (R3)", {
    m <- cor(mtcars)
    vals <- data.frame(
        id = rep(colnames(m)[1:2], each = 2L),
        value = c(1, 2, 50, 100),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(m, tracks = c(v = 0.2)) +
        geom_track(data = vals, aes(x = value), node = id, band = "v")
    d <- ggplot2::ggplot_build(p)$data[[1]]

    expect_equal(d$xmax, c(1.16, 1.26, 1.16, 1.26), tolerance = 1e-6)

    box <- data.frame(
        id = rep(colnames(m)[1:2], each = 4L),
        value = c(1, 2, 3, 4, 10, 20, 30, 40),
        stringsAsFactors = FALSE
    )
    bx <- ggplot2::ggplot_build(
        ggcircos(m, tracks = c(v = 0.2)) +
            geom_track(data = box, aes(x = value), node = id,
                       geom = "boxplot", band = "v")
    )$data[[1]]
    expect_equal(nrow(bx), 2L)
    expect_equal(bx$xmax, c(1.26, 1.26), tolerance = 1e-6)
    expect_true(all(bx$xmin < bx$x & bx$x < bx$xmax))
    expect_gt(diff(sort(bx$x)), 0)
})

test_that("geom_track(value = ) reaches the position and guards stacking (R4)", {
    m <- cor(mtcars)
    base <- function() ggcircos(m, tracks = c(v = 0.2))
    ok <- data.frame(id = colnames(m), v = seq(0.1, 1, length.out = 11))

    # `value =` and `aes(x = )` must produce identical geometry
    by_value <- ggplot2::ggplot_build(
        base() + geom_track(data = ok, value = v, node = id, band = "v")
    )$data[[1]]
    by_aes <- ggplot2::ggplot_build(
        base() + geom_track(data = ok, aes(x = v), node = id, band = "v")
    )$data[[1]]
    expect_equal(by_value$x, by_aes$x)

    # a negative stack is an error, not a silent flat track
    neg <- data.frame(id = rep(colnames(m), 2), v = c(rep(-1, 11), rep(1, 11)))
    expect_error(
        ggplot2::ggplot_build(
            base() + geom_track(data = neg, value = v, node = id,
                                band = "v", stack = TRUE)
        ),
        class = "ggcircos_error_data"
    )

    # a data column that happens to be called `x` must not shadow the value
    shadow <- data.frame(
        id = rep(colnames(m)[1:2], each = 2L),
        x = c(0.1, 0.2, 0.5, 1.0),
        stringsAsFactors = FALSE
    )
    got <- ggplot2::ggplot_build(
        base() + geom_track(data = shadow, value = x, node = id, band = "v")
    )$data[[1]]
    ref <- ggplot2::ggplot_build(
        base() + geom_track(data = shadow, aes(x = x), node = id, band = "v")
    )$data[[1]]
    expect_equal(got$x, ref$x)
    expect_gt(length(unique(round(got$x, 6))), 1L)

    # and `value` must name a real column
    expect_error(
        base() + geom_track(data = ok, value = nope, node = id, band = "v"),
        class = "ggcircos_error_data"
    )
})

test_that("one position object can be shared by two layers unharmed (R9)", {
    # The point of keeping only vectors on the position (spec 8.2): a shared
    # PositionTrack must not accumulate one layer's data. It holds the band,
    # the node column name and the normalisation settings -- nothing that a
    # layer can write into.
    m <- cor(mtcars)
    pos <- position_track(band = c(1.06, 1.26), node = "id", anchored = TRUE)
    before <- pos$band
    vals <- data.frame(id = colnames(m), v = seq(0.1, 1, length.out = 11))

    built <- ggplot2::ggplot_build(
        ggcircos(m) +
            geom_track(data = vals, aes(x = v), node = id, position = pos) +
            geom_track(data = vals, aes(x = v), node = id, position = pos,
                       fill = "tomato")
    )
    # both layers still draw in the band they were given, at the same radii:
    # a layer must not leave its data behind on the shared object
    expect_equal(built$data[[1]]$xmin[1], 1.06, tolerance = 1e-6)
    expect_equal(built$data[[2]]$xmin[1], 1.06, tolerance = 1e-6)
    expect_equal(built$data[[1]]$xmax, built$data[[2]]$xmax)
    # and the shared object itself is untouched
    expect_equal(pos$band, before)
    expect_null(pos$values)
    expect_null(pos$node_values)
})

test_that("multi-row tracks split the node arc for every non-aggregating geom (B37)", {
    # track_angles() used to bin the arc for `tile` only. A col/area/bar track
    # with several measurements per node then stacked every row on the whole
    # arc, so the tallest one hid the rest and the track read as a solid band
    # with no relief.
    m <- cor(mtcars)[1:5, 1:5]
    nd <- layout_nodes(circos_layout(m, order = "hclust"))
    set.seed(5)
    prof <- data.frame(id = rep(nd$id, each = 25), value = runif(5 * 25, .1, .95))
    base <- ggcircos(m, order = "hclust", tracks = c(d = 0.18)) + geom_node_arc()

    for (g in c("col", "area", "bar")) {
        d <- ggplot2::ggplot_build(
            base + geom_track(data = prof, aes(x = value), node = id,
                              band = "d", geom = g)
        )$data[[2]]
        span <- d$ymax - d$ymin
        # 25 rows per node, so each bin is a 25th of the arc, not the whole of it
        expect_lt(max(span), nd$width[1] / 5)
        expect_equal(max(span), min(span), tolerance = 1e-9)
        # bins tile each node's arc without gaps or overlap
        for (i in seq_len(nrow(nd))) {
            s <- d[which(prof$id == nd$id[i]), ]
            s <- s[order(s$ymin), ]
            expect_equal(sum(s$ymax - s$ymin), 0.9 * nd$width[i], tolerance = 1e-9)
            if (nrow(s) > 1) {
                expect_equal(s$ymin[-1], s$ymax[-nrow(s)], tolerance = 1e-9)
            }
        }
    }

    # boxplot aggregates to one mark per node, so that mark keeps the whole arc
    bp <- ggplot2::ggplot_build(
        base + geom_track(data = prof, aes(x = value), node = id,
                          band = "d", geom = "boxplot")
    )$data[[2]]
    expect_equal(bp$ymax - bp$ymin, rep(0.9 * nd$width[1], nrow(bp)), tolerance = 1e-9)

    # one row per node must be unchanged: the bin is the whole width share,
    # bit-identical to centring on theta
    one <- data.frame(id = nd$id, value = seq(.2, 1, length.out = 5))
    d1 <- ggplot2::ggplot_build(
        base + geom_track(data = one, aes(x = value), node = id,
                          band = "d", geom = "col")
    )$data[[2]]
    half <- nd$width * 0.9 / 2
    expect_equal(d1$ymin, nd$theta - half, tolerance = 1e-12)
    expect_equal(d1$ymax, nd$theta + half, tolerance = 1e-12)
})
