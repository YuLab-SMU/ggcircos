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

test_that("columns = maps a foreign link table onto from/to (B38)", {
    # BLAST table output spells its endpoints qaccver/saccver. Rather than one
    # method per format, the caller names the columns.
    blast <- data.frame(
        qaccver = c("chr1", "chr1", "chr2"),
        saccver = c("chrA", "chrB", "chrA"),
        qstart = c(100, 900, 50), qend = c(500, 1200, 300),
        pident = c(95, 80, 88),
        stringsAsFactors = FALSE
    )
    expect_error(circos_layout(blast), class = "ggcircos_error_data")

    lay <- circos_layout(blast, columns = c(from = "qaccver", to = "saccver"))
    expect_setequal(layout_nodes(lay)$id, c("chr1", "chr2", "chrA", "chrB"))
    expect_equal(nrow(layout_links(lay)), 3L)
    # the columns the caller did not map travel with the table
    expect_true(all(c("qstart", "qend", "pident") %in% names(layout_links(lay))))
    # and the caller's frame is untouched
    expect_equal(names(blast)[1:2], c("qaccver", "saccver"))

    # weight can name a mapped column
    p <- ggcircos(blast, columns = c(from = "qaccver", to = "saccver"),
                  weight = pident) + geom_chord(aes(fill = pident))
    expect_s3_class(ggplot2::ggplotGrob(p), "gtable")

    # a column that does not exist is an error, not a silent no-op
    expect_error(
        circos_layout(blast, columns = c(from = "nope", to = "saccver")),
        class = "ggcircos_error_data"
    )
})

test_that("geom_track_axis() takes its range from the track layer (B39)", {
    # The track already knows what its values were normalised against. Asking
    # for the same pair again on the axis was pure repetition, and the two
    # could drift.
    m <- cor(mtcars)[1:6, 1:6]
    vals <- data.frame(id = colnames(m), v = seq(0.2, 0.9, length.out = 6))
    base <- function(lim) {
        ggcircos(m, tracks = c(t = 0.15)) +
            geom_track(data = vals, aes(x = v), node = id, band = "t",
                       scales = "fixed", limits = lim)
    }
    axis_labels <- function(p) {
        b <- ggplot2::ggplot_build(p)
        for (d in b$data) {
            if (!"label" %in% names(d)) next
            lab <- trimws(unique(d$label[nzchar(d$label)]))
            if (length(lab)) return(lab)
        }
        character(0)
    }

    # the axis labels follow the track's own range
    expect_equal(axis_labels(base(c(0, 1)) + geom_track_axis(band = "t")),
                 c("0.0", "0.5", "1.0"))
    expect_equal(axis_labels(base(c(0, 100)) + geom_track_axis(band = "t")),
                 c("0", "50", "100"))
    # an explicit limits still wins, and breaks above 1 are labelled honestly
    # (they used to be squished to the default c(0, 1) range and all read 1.0)
    expect_equal(
        axis_labels(base(c(0, 1)) + geom_track_axis(band = "t", limits = c(0, 2))),
        c("0.0", "0.5", "1.0", "1.5", "2.0")
    )
    # with no track to read, it says so instead of drawing an empty axis
    expect_error(
        ggcircos(m, tracks = c(t = 0.15)) + geom_track_axis(band = "t"),
        class = "ggcircos_error_data"
    )
})

test_that("a violin track puts the density on the node's arc and the value on the radius (B41)", {
    m <- cor(mtcars)[1:6, 1:6]
    nd <- layout_nodes(circos_layout(m, tracks = c(t = 0.3)))
    set.seed(1)
    vals <- data.frame(id = rep(nd$id, each = 60), v = rnorm(nrow(nd) * 60, 0.5, 0.12))
    p <- ggcircos(m, tracks = c(t = 0.3)) +
        geom_track(data = vals, aes(x = v), node = id, geom = "violin", band = "t")
    d <- ggplot2::ggplot_build(p)$data[[1]]

    # one closed outline per node, and every node is present
    expect_setequal(unique(d$node), nd$id)
    expect_equal(length(unique(d$group)), nrow(nd))

    # the value ("v") runs along the radius: the built `x` is inside the band
    band <- c(1.06, 1.36)
    expect_true(all(d$x >= band[1] - 1e-8 & d$x <= band[2] + 1e-8))
    # ... and it is the data's own range, not a constant
    expect_gt(diff(range(d$x)), 0.1)

    # the density runs along the arc: each outline is centred on its node's
    # theta and reaches the node's whole (width-shrunk) arc at its widest
    for (i in seq_len(nrow(nd))) {
        s <- d[d$node == nd$id[i], ]
        span <- max(s$y) - min(s$y)
        expect_equal(span, 0.3 * 0 + nd$width[i] * 0.9, tolerance = 1e-6)
        expect_equal(mean(range(s$y)), nd$theta[i], tolerance = 1e-6)
    }

    # the widest point is where the samples pile up, not at the ends: that is
    # what makes it a violin instead of a lens
    s <- d[d$node == nd$id[1], ]
    s <- s[order(s$x), ]
    half <- nrow(s) / 2
    dev <- abs(s$y[seq_len(half)] - nd$theta[1])
    expect_gt(which.max(dev), 1)
    expect_lt(which.max(dev), half)
})

test_that("the violin's angular offset survives the stat -> position hand-off (B41)", {
    # The first implementation put the density in a stat column and asked the
    # position to spread it, which cannot work: ggplot2 hands a position only
    # the *aesthetics*, and a stat's extra columns are not aesthetics. Every
    # row then sat at the node centre and the track drew nothing visible.
    #
    # The offset therefore has to be in `y` before the position runs. Assert it
    # at the stat, so a future refactor that moves it back fails here.
    m <- cor(mtcars)[1:5, 1:5]
    nd <- layout_nodes(circos_layout(m, tracks = c(t = 0.3)))
    set.seed(2)
    vals <- data.frame(id = rep(nd$id, each = 40), v = rnorm(nrow(nd) * 40, 0.5, 0.1))
    lyr <- geom_track(data = vals, aes(x = v), node = id, geom = "violin", band = "t")
    built <- ggplot2::ggplot_build(ggcircos(m, tracks = c(t = 0.3)) + lyr)$data[[1L]]
    # more than one distinct angle per node means the arc is being used
    per_node <- tapply(built$y, built$node, function(z) length(unique(round(z, 9))))
    expect_true(all(per_node > 1))

    # and the stat's own output carries the offset, not a flat centre line
    d <- built
    expect_gt(diff(range(d$y)), 1)
})

test_that("a violin crossing the seam is cut, not squished (B41)", {
    # The y scale spans [0, 2*pi] and squishes what falls outside. A bar only
    # narrows at the seam; a closed outline would collapse into a flat edge.
    m <- cor(mtcars)[1:4, 1:4]
    lay <- circos_layout(m, start = 5.9, tracks = c(t = 0.45))
    nd <- layout_nodes(lay)
    straddles <- nd$id[nd$theta1 < nd$theta0]
    expect_length(straddles, 1L)

    set.seed(3)
    vals <- data.frame(id = rep(nd$id, each = 120), v = rnorm(nrow(nd) * 120, 0.5, 0.1))
    d <- ggplot2::ggplot_build(
        ggcircos(m, start = 5.9, tracks = c(t = 0.45)) +
            geom_track(data = vals, aes(x = v), node = id, geom = "violin", band = "t")
    )$data[[1]]

    # nothing lands outside the panel...
    expect_true(all(d$y >= -1e-9 & d$y <= 2 * pi + 1e-9))
    # ...the straddling node is drawn as two pieces, one per side of the seam
    expect_equal(length(unique(d$group[d$node == straddles])), 2L)
    # ...and the rest stay whole
    for (k in setdiff(nd$id, straddles)) {
        expect_equal(length(unique(d$group[d$node == k])), 1L)
    }
    # the outline still fills the node's whole arc: the peak deviation is half
    # of it, measured the circular way (a raw `-` across the seam is a turn
    # off, which is exactly the mistake this test guards against)
    s <- d[d$node == straddles, ]
    centre <- nd$theta[nd$id == straddles]
    dev <- abs(((s$y - centre + pi) %% (2 * pi)) - pi)
    expect_equal(max(dev), nd$width[nd$id == straddles] * 0.9 / 2, tolerance = 1e-6)
    # and no single piece crosses the seam, or it would be squished flat
    for (g in unique(s$group)) {
        expect_true(all(range(s$y[s$group == g]) >= -1e-9))
        expect_true(all(range(s$y[s$group == g]) <= 2 * pi + 1e-9))
    }
})

test_that("a violin needs a spread, and says so when there is none (B41)", {
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, tracks = c(t = 0.3)))
    base <- function(d, ...) {
        ggcircos(m, tracks = c(t = 0.3)) +
            geom_track(data = d, aes(x = v), node = id, geom = "violin",
                       band = "t", ...)
    }
    # one constant value per node has no density to estimate: the layer warns
    # instead of drawing an unexplained empty band
    flat <- data.frame(id = rep(nd$id, each = 5), v = 1)
    expect_warning(
        ggplot2::ggplot_build(base(flat)),
        "nothing to draw"
    )
    # a node with too few values is skipped, the others still draw
    set.seed(4)
    mixed <- data.frame(
        id = c(rep(nd$id[1], 2), rep(nd$id[-1], each = 30)),
        v = rnorm(2 + 30 * (length(nd$id) - 1), 0.5, 0.1)
    )
    d <- ggplot2::ggplot_build(base(mixed))$data[[1]]
    expect_false(nd$id[1] %in% d$node)
    expect_setequal(unique(d$node), nd$id[-1])
})

test_that("stacking is refused for geoms whose stat aggregates per node (B41)", {
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, tracks = c(t = 0.3)))
    set.seed(5)
    vals <- data.frame(id = rep(nd$id, each = 20), v = runif(nrow(nd) * 20))
    for (g in c("violin", "boxplot")) {
        expect_error(
            ggcircos(m, tracks = c(t = 0.3)) +
                geom_track(data = vals, aes(x = v), node = id, geom = g,
                           band = "t", stack = TRUE),
            class = "ggcircos_error_data"
        )
    }
    # the non-aggregating geoms still stack
    expect_s3_class(
        ggplot2::ggplotGrob(
            ggcircos(m, tracks = c(t = 0.3)) +
                geom_track(data = vals, aes(x = v), node = id, geom = "col",
                           band = "t", stack = TRUE)
        ),
        "gtable"
    )
})

test_that("violin honours n, adjust and trim (B41)", {
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, tracks = c(t = 0.3)))
    set.seed(6)
    vals <- data.frame(id = rep(nd$id, each = 60), v = rnorm(nrow(nd) * 60, 0.5, 0.12))
    one <- function(...) {
        d <- ggplot2::ggplot_build(
            ggcircos(m, tracks = c(t = 0.3)) +
                geom_track(data = vals, aes(x = v), node = id, geom = "violin",
                           band = "t", ...)
        )$data[[1]]
        d
    }
    # `n` is the number of density points per outline (two sides + seam rows)
    expect_lt(nrow(one(n = 16L)), nrow(one(n = 64L)))
    # a wider bandwidth smooths: fewer distinct radii, a narrower value range
    narrow <- one(adjust = 3)
    expect_lt(diff(range(narrow$x)), diff(range(one(adjust = 0.3)$x)))
    # `trim = FALSE` keeps the kernel's tails beyond the data
    expect_gt(diff(range(one(trim = FALSE)$x)), diff(range(one(trim = TRUE)$x)))
})

test_that("track_angles keeps `aggregate` out of the positional reach (B41)", {
    # `aggregate` was added after `...` on purpose. Before that, a legacy
    # positional call whose 7th argument used to fall into `...` would land on
    # `aggregate` instead and silently switch a plain track to its full-arc
    # branch -- a number is a valid `if()` condition in R, so nothing errors.
    lay <- circos_layout(cor(mtcars)[1:3, 1:3])
    ids <- colnames(cor(mtcars)[1:3, 1:3])
    # the aggregate branch is `arc * width * 2` endpoints in, i.e. `width`
    # shrinks the mark just as it does for the bin split
    full <- layout_nodes(lay)$width[1] * 0.9

    # positional 7th argument is ignored, not read as `aggregate`
    legacy <- track_angles(data.frame(id = ids), "id", lay, 0.9, "col", FALSE, 0.5)
    expect_equal(unique(round(legacy$ymax - legacy$ymin, 9)), round(full, 9))

    # the default branch is picked from the geom when `aggregate` is not given
    expect_equal(unique(round(track_angles(data.frame(id = ids), "id", lay,
                                            geom = "violin")$ymax -
                              track_angles(data.frame(id = ids), "id", lay,
                                           geom = "violin")$ymin, 9)),
                 round(full, 9))
    # ... and can still be forced by name
    forced <- track_angles(data.frame(id = ids), "id", lay, geom = "col",
                           aggregate = TRUE)
    expect_equal(unique(round(forced$ymax - forced$ymin, 9)), round(full, 9))
})

test_that("a density track draws an open ridgeline, not the violin's closed outline (B42)", {
    m <- cor(mtcars)[1:6, 1:6]
    nd <- layout_nodes(circos_layout(m, tracks = c(d = 0.3)))
    set.seed(1)
    vals <- data.frame(id = rep(nd$id, each = 60), v = rnorm(nrow(nd) * 60, 0.5, 0.12))
    base <- function(g) {
        ggcircos(m, tracks = c(d = 0.3)) +
            geom_track(data = vals, aes(x = v), node = id, geom = g, band = "d")
    }
    v <- ggplot2::ggplot_build(base("violin"))$data[[1]]
    dn <- ggplot2::ggplot_build(base("density"))$data[[1]]

    # the violin is closed (up one side, back the other): 2 points per sample.
    # The ridgeline is open: 1 point per sample, its own group per node.
    expect_equal(nrow(dn) * 2, nrow(v))
    expect_equal(length(unique(dn$group)), nrow(nd))
    s <- dn[dn$group == dn$group[1], ]
    expect_gt(nrow(s), 2)
    # open: the curve does not return to where it started
    expect_false(isTRUE(all.equal(s$y[1], s$y[nrow(s)])))

    # it rises on ONE side of the node's centre, never both
    ctr <- nd$theta[match(dn$node[1], nd$id)]
    expect_true(all(s$y >= ctr - 1e-9))
    # ...and reaches the node's half arc at its mode
    expect_equal(max(s$y - ctr), nd$width[nd$id == dn$node[1]] * 0.9 / 2,
                 tolerance = 1e-6)

    # the value still runs along the radius, inside the band
    expect_true(all(dn$x >= 1.06 - 1e-8 & dn$x <= 1.36 + 1e-8))
})

test_that("the two density geometries share one kernel (B42)", {
    # Same `n`/`adjust`/`trim`, same KDE, same normalisation: the violin's
    # outline and the ridgeline disagree only in whether the second half is
    # emitted. Asserted on the stat, so a future change to one does not
    # silently desynchronise them.
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, tracks = c(d = 0.4)))
    set.seed(2)
    vals <- data.frame(id = rep(nd$id, each = 50), v = rnorm(nrow(nd) * 50, 0.5, 0.1))
    lay <- circos_layout(m, tracks = c(d = 0.4))
    d <- track_angles(vals, "id", lay, 0.9, "density")
    d$y <- d$.circos_theta
    d$node <- d$id
    d$x <- vals$v
    dn <- StatTrackDensity$compute_panel(d, NULL, n = 40L, adjust = 1,
                                         trim = TRUE, na.rm = FALSE)
    v <- StatTrackViolin$compute_panel(d, NULL, n = 40L, adjust = 1,
                                       trim = TRUE, na.rm = FALSE)
    one_d <- dn[dn$node == nd$id[1], ]
    one_v <- v[v$node == nd$id[1], ]
    h <- d$.circos_half[d$id == nd$id[1]][1]
    ctr <- d$.circos_theta[d$id == nd$id[1]][1]
    # the ridgeline is the upper half of the outline, point for point
    expect_equal(nrow(one_d), nrow(one_v) / 2)
    expect_equal(one_d$y, one_v$y[seq_len(nrow(one_d))], tolerance = 1e-9)
    expect_equal(one_d$x, one_v$x[seq_len(nrow(one_d))], tolerance = 1e-9)
    # both normalise the height to the same half arc
    expect_equal(max(one_d$y) - ctr, h, tolerance = 1e-6)
})

test_that("a ridgeline crossing the seam is split, and never spans a turn (B42)", {
    # `seam_pieces()` is a *closed polygon* clipper, so it cannot be reused for
    # a stroke: it returns a ring whose edges lie on the seam, which draws as a
    # straight line the data never asked for. The curve uses `seam_folds()`,
    # which only splits.
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, start = 5.2, tracks = c(d = 0.45)))
    straddles <- nd$theta - nd$width / 2 < 0 | nd$theta + nd$width / 2 > 2 * pi
    expect_true(any(straddles))
    set.seed(3)
    vals <- data.frame(id = rep(nd$id, each = 120), v = rnorm(nrow(nd) * 120, 0.5, 0.1))
    d <- ggplot2::ggplot_build(
        ggcircos(m, start = 5.2, tracks = c(d = 0.45)) +
            geom_track(data = vals, aes(x = v), node = id, geom = "density",
                       band = "d")
    )$data[[1]]
    # nothing is squished against the seam
    expect_true(all(d$y >= -1e-9 & d$y <= 2 * pi + 1e-9))
    # a piece never spans the whole turn (the signature of a broken split)
    for (g in unique(d$group)) {
        expect_lt(diff(range(d$y[d$group == g])), 2 * pi - 1e-6)
    }
    # the node whose curve reaches past the seam is the one that gets split
    split_nodes <- names(which(tapply(d$group, d$node, function(z) length(unique(z))) > 1))
    expect_true(length(split_nodes) >= 1)
    # seam_folds leaves a curve that stays inside alone
    inside <- ggcircos:::seam_folds(c(1, 2, 3), c(0.5, 0.6, 0.7))
    expect_equal(length(inside), 1L)
    expect_equal(inside[[1]]$y, c(0.5, 0.6, 0.7))
})

test_that("density needs a spread, and stacking is refused (B42)", {
    m <- cor(mtcars)[1:4, 1:4]
    nd <- layout_nodes(circos_layout(m, tracks = c(d = 0.3)))
    base <- function(d, ...) {
        ggcircos(m, tracks = c(d = 0.3)) +
            geom_track(data = d, aes(x = v), node = id, geom = "density",
                       band = "d", ...)
    }
    flat <- data.frame(id = rep(nd$id, each = 5), v = 1)
    expect_warning(ggplot2::ggplot_build(base(flat)), "nothing to draw")
    expect_error(
        ggcircos(m, tracks = c(d = 0.3)) +
            geom_track(data = data.frame(id = rep(nd$id, 20), v = runif(80)),
                       aes(x = v), node = id, geom = "density", band = "d",
                       stack = TRUE),
        class = "ggcircos_error_data"
    )
})

test_that("a slot-aligned row sits on the slot of ITS node, not always the from end (B47)", {
    # a link reserves a slot on each of its two nodes, and the two arcs are
    # different. A row filed under the link's `to` node must land on the to
    # slot -- taking the from slot for every row put target-side rectangles
    # on the wrong side of the ring.
    nodes <- data.frame(id = c("a", "b", "c"), stringsAsFactors = FALSE)
    links <- data.frame(from = c("a", "b"), to = c("b", "c"),
                        stringsAsFactors = FALSE)
    lay <- circos_layout(links, nodes = nodes, tracks = c(t = 0.2))
    lk <- lay$links
    trk <- data.frame(
        id = c("a", "b", "c"),      # a: link 1's from; b: link 1's to; c: link 2's to
        v = c(0.5, 0.5, 0.5),
        edge_id = c(1L, 1L, 2L),
        stringsAsFactors = FALSE
    )
    d <- ggplot2::ggplot_build(
        ggcircos(lay) +
            geom_track(data = trk, ggplot2::aes(x = v), node = id, band = "t",
                       slot = edge_id)
    )$data[[1]]
    # rows come back in data order; `.circos_theta` is not an aesthetic, so the
    # built frame keeps only x/y/ymin/ymax and the node column is gone
    expect_equal(nrow(d), 3L)
    slot_of <- function(i) sort(c(lk$theta0[i], lk$theta1[i]))
    slot_end <- function(i) sort(c(lk$theta_end0[i], lk$theta_end1[i]))
    expect_equal(sort(c(d$ymin[1], d$ymax[1])), slot_of(1), tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[2], d$ymax[2])), slot_end(1), tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[3], d$ymax[3])), slot_end(2), tolerance = 1e-9)
    # and the three rows are three different arcs: a from-slot-for-everything
    # bug would collapse rows 2 and 3 onto their link's from side
    expect_false(isTRUE(all.equal(sort(c(d$ymin[2], d$ymax[2])),
                                  sort(c(d$ymin[1], d$ymax[1])))))
})

test_that("slot = works on a by= plot: every panel numbers its own links (B49)", {
    # Each panel renumbers its links from 1, so the stacked table repeats
    # `edge_id` (1, 1). Matching on it silently picked the first panel for both
    # rows; the caller's numbering survives as `user_edge_id` (B48), and that is
    # what a per-link track has to be matched against.
    # P1 has three nodes and P2 has two, so the two rings place link 1 on a
    # different arc from link 2 -- otherwise a wrong-panel match is invisible
    nodes <- data.frame(id = c("a1", "b1", "c1", "a2", "b2"),
                        g = c(rep("P1", 3), rep("P2", 2)),
                        stringsAsFactors = FALSE)
    links <- data.frame(from = c("a1", "a2"), to = c("b1", "b2"),
                        g = c("P1", "P2"), stringsAsFactors = FALSE)
    links$edge_id <- seq_len(nrow(links))          # the caller's own numbering
    p0 <- ggcircos(links, nodes = nodes, by = "g", tracks = c(t = 0.2))
    st <- p0$data
    # sanity: the layout renumbered per panel, the caller's column survived
    expect_equal(st$edge_id, c(1L, 1L))
    expect_equal(st$user_edge_id, c(1L, 2L))

    trk <- data.frame(id = c("a1", "b1", "a2", "b2"),
                      g = c("P1", "P1", "P2", "P2"),
                      v = 0.5, edge_id = c(1L, 1L, 2L, 2L),
                      stringsAsFactors = FALSE)
    d <- ggplot2::ggplot_build(
        p0 + geom_track(data = trk, ggplot2::aes(x = v), node = id, band = "t",
                        slot = edge_id)
    )$data[[1]]
    expect_equal(nrow(d), 4L)
    # rows 1-2 are P1's two ends of link 1, rows 3-4 are P2's two ends of link 2
    expect_equal(sort(c(d$ymin[1], d$ymax[1])), sort(c(st$theta0[1], st$theta1[1])),
                 tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[2], d$ymax[2])), sort(c(st$theta_end0[1], st$theta_end1[1])),
                 tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[3], d$ymax[3])), sort(c(st$theta0[2], st$theta1[2])),
                 tolerance = 1e-9)
    expect_equal(sort(c(d$ymin[4], d$ymax[4])), sort(c(st$theta_end0[2], st$theta_end1[2])),
                 tolerance = 1e-9)
    # and the two panels really are different arcs: a match on the repeated
    # `edge_id` would collapse both links onto the first panel's slot
    expect_false(isTRUE(all.equal(sort(c(d$ymin[1], d$ymax[1])),
                                  sort(c(d$ymin[3], d$ymax[3])))))
})

test_that("a caller's edge_id column is kept, not overwritten (B48)", {
    # allocate_slots() numbers links in layout order under the name `edge_id`.
    # A column of that name arriving from the caller is their data: it must be
    # renamed, or their numbering vanishes silently and `slot =` lines up
    # against the wrong links.
    links <- data.frame(from = "a", to = "b", edge_id = 7L, stringsAsFactors = FALSE)
    lk <- layout_links(circos_layout(links))
    expect_equal(lk$user_edge_id, 7L)
    expect_equal(lk$edge_id, 1L)
    expect_false(identical(lk$user_edge_id, lk$edge_id))
})

test_that("a node with no drawable distribution is reported, not silently dropped (B55)", {
    # One good node and one with no spread: the plot still draws, and the
    # dropped node used to vanish without a word -- on the ring that reads as
    # "no distribution measured", the opposite of what happened.
    links <- data.frame(from = c("a", "b"), to = c("b", "c"), stringsAsFactors = FALSE)
    lay <- circos_layout(links, tracks = c(t = 0.3))
    trk <- data.frame(id = c("a", "a", "a", "b", "b", "b"),
                      v = c(1, 2, 3, 5, 5, 5), stringsAsFactors = FALSE)
    p <- ggcircos(lay) +
        geom_track(data = trk, ggplot2::aes(x = v), node = id, band = "t",
                   geom = "violin", limits = c(0, 6))
    msgs <- character(0)
    withCallingHandlers(
        ggplot2::ggplot_build(p),
        message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        }
    )
    expect_true(any(grepl('Skipped 1 node .*"b"', msgs)))
})
