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

    expect_equal(min(b$data[[1]]$xmin), 1.02, tolerance = 1e-6)
    expect_equal(max(b$data[[1]]$xmax), 1.20, tolerance = 1e-6)
    expect_equal(min(b$data[[2]]$xmin), 1.22, tolerance = 1e-6)
    expect_equal(max(b$data[[2]]$xmax), 1.28, tolerance = 1e-6)
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
    expect_equal(min(d$xmin), 1.02, tolerance = 1e-6)
    expect_equal(max(d$xmax), 1.17, tolerance = 1e-6)

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

test_that("continuous fill does not explode into one grob per link (B11)", {
    m <- cor(mtcars)
    grobs_of <- function(p) {
        g <- ggplot2::ggplotGrob(p)
        idx <- which(vapply(g$grobs, function(x) !is.null(x$name) && grepl("^panel", x$name), logical(1)))
        ch <- g$grobs[[idx[1]]]$children
        k <- which(vapply(ch, function(x) !is.null(x$name) && grepl("chord", x$name), logical(1)))
        length(ch[[k]]$children)
    }
    n_links <- nrow(ggcircos:::layout_links(ggcircos:::circos_layout(m)))
    expect_gt(n_links, 24L)
    expect_lt(grobs_of(ggcircos(m, order = "hclust") + geom_chord(aes(fill = r))), 25L)
    # the cheap paths must stay cheap
    expect_equal(grobs_of(ggcircos(m) + geom_chord()), 1L)
    expect_equal(grobs_of(ggcircos(m) + geom_chord(type = "line")), 1L)
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
