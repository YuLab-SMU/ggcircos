# Multi-radius nodes (spec §4.9): one node id may occupy several radii, the
# layers stacked above each other.

test_that("a node may occupy several radii, and the layers are radially aligned", {
    nodes <- data.frame(
        id = c("a", "a", "b", "b"),
        r = c(1, 1.5, 1, 1.5),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "a", "b"), to = c("b", "b", "a"),
        from_r = c(1, 1.5, 1), to_r = c(1, 1.5, 1),
        stringsAsFactors = FALSE
    )
    lay <- circos_layout(links, nodes = nodes)
    nd <- layout_nodes(lay)

    # one row per arc
    expect_equal(nrow(nd), 4L)
    expect_setequal(nd$r, c(1, 1.5))
    # the layers of one node share the node's place on the ring
    for (id in c("a", "b")) {
        s <- nd[nd$id == id, ]
        expect_equal(s$theta0[1], s$theta0[2], tolerance = 1e-12)
        expect_equal(s$theta1[1], s$theta1[2], tolerance = 1e-12)
        expect_equal(s$width[1], s$width[2], tolerance = 1e-12)
    }
    # the angle budget counts *ids*: two ids still split the turn evenly
    expect_equal(nd$width[nd$id == "a"][1], nd$width[nd$id == "b"][1],
                 tolerance = 1e-12)
    # n_links counts per arc
    expect_equal(nd$n_links[nd$id == "a" & nd$r == 1], 2L)
    expect_equal(nd$n_links[nd$id == "a" & nd$r == 1.5], 1L)
})

test_that("a link's endpoint radius picks the arc it sits on", {
    nodes <- data.frame(id = c("a", "a", "b", "b"), r = c(1, 1.5, 1, 1.5),
                        stringsAsFactors = FALSE)
    links <- data.frame(
        from = c("a", "a"), to = c("b", "b"),
        from_r = c(1, 1.5), to_r = c(1, 1.5),
        stringsAsFactors = FALSE
    )
    lk <- layout_links(circos_layout(links, nodes = nodes))
    # the endpoint radius is the arc's own
    expect_equal(lk$x, c(1, 1.5))
    expect_equal(lk$xend, c(1, 1.5))
})

test_that("an ambiguous link endpoint is refused, not guessed", {
    nodes <- data.frame(id = c("a", "a", "b"), r = c(1, 1.5, 1), stringsAsFactors = FALSE)
    links <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    # "a" has two arcs and the link does not say which
    expect_error(circos_layout(links, nodes = nodes), class = "ggcircos_error_node")
    # naming a radius that does not exist is an error too
    bad <- data.frame(from = "a", to = "b", from_r = 2, to_r = 1, stringsAsFactors = FALSE)
    expect_error(circos_layout(bad, nodes = nodes), class = "ggcircos_error_node")
    # a single-arc id needs no radius
    ok <- data.frame(from = "a", to = "b", from_r = 1, to_r = 1, stringsAsFactors = FALSE)
    expect_s3_class(circos_layout(ok, nodes = nodes), "circos_layout")
})

test_that("the same node at the same radius twice is an error", {
    nodes <- data.frame(id = c("a", "a"), r = c(1, 1), stringsAsFactors = FALSE)
    links <- data.frame(from = "a", to = "a", stringsAsFactors = FALSE)
    expect_error(circos_layout(links, nodes = nodes), class = "ggcircos_error_node")
})

test_that("the panel reaches the outermost arc", {
    nodes <- data.frame(id = c("a", "a", "b", "b"), r = c(1, 1.6, 1, 1.6),
                        stringsAsFactors = FALSE)
    links <- data.frame(from = c("a", "a"), to = c("b", "b"),
                        from_r = c(1, 1.6), to_r = c(1, 1.6),
                        stringsAsFactors = FALSE)
    p <- ggcircos(links, nodes = nodes) + geom_chord() + geom_node_arc()
    # the upper limit is data-driven now (`limits = c(0, NA)`), so the panel's
    # trained range is what must clear the outermost arc
    b <- ggplot2::ggplot_build(p)
    lim <- b$layout$panel_params[[1]]$r.range
    expect_gt(lim[2], 1.6)
})

test_that("a single-radius layout is unchanged by the arc machinery", {
    # the compatibility floor (spec §4.9): one radius per node must give the
    # same table as before, down to the columns
    lay <- circos_layout(cor(mtcars))
    nd <- layout_nodes(lay)
    expect_equal(nrow(nd), ncol(cor(mtcars)))
    expect_equal(nd$r, rep(1, ncol(cor(mtcars))))
    expect_true(all(c("id", "label", "sector", "order", "n_links",
                      "theta0", "theta1", "theta", "width", "r") %in% names(nd)))
    expect_false(anyDuplicated(nd$id) > 0)
})

test_that("by= and multi-radius compose: a node may sit at several radii per panel", {
    # the node identity is (group, id, r): one panel, two rings, the same id
    # at two radii in each
    nodes <- data.frame(
        id = c("a", "a", "b", "b"),
        r = c(1, 1.5, 1, 1.5),
        grp = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "a", "b", "b"),
        to = c("b", "b", "a", "a"),
        from_r = c(1, 1.5, 1, 1.5),
        to_r = c(1, 1.5, 1, 1.5),
        grp = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )
    lays <- circos_layout(links, nodes = nodes, by = "grp")
    expect_setequal(names(lays), c("A", "B"))
    for (k in c("A", "B")) {
        nd <- layout_nodes(lays[[k]])
        expect_equal(nrow(nd), 2L)
        expect_setequal(nd$r, c(1, 1.5))
        # the two layers of one node share its place on that panel's ring
        expect_equal(nd$theta0[1], nd$theta0[2], tolerance = 1e-12)
    }
})

test_that("a link whose ends are not in its own group is dropped with a message", {
    nodes <- data.frame(id = c("a", "b"), grp = c("A", "B"), stringsAsFactors = FALSE)
    links <- data.frame(from = "a", to = "b", grp = "A", stringsAsFactors = FALSE)
    expect_message(lays <- circos_layout(links, nodes = nodes, by = "grp"), "not both")
    expect_equal(nrow(layout_links(lays$A)), 0L)
    expect_equal(nrow(layout_links(lays$B)), 0L)
})
