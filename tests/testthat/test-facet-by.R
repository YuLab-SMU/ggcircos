# The facet B-plan (spec §9.1): one ring per group, each with its own node set
# and ordering. The A-plan shares one ring across panels; this asks for more.

test_that("circos_layout(by=) builds one independent layout per group", {
    nodes <- data.frame(
        id = c("a", "b", "c", "x", "y", "z"),
        grp = rep(c("A", "B"), each = 3),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "b", "x", "y"),
        to   = c("b", "c", "y", "z"),
        stringsAsFactors = FALSE
    )
    lays <- circos_layout(links, nodes = nodes, by = "grp")
    expect_s3_class(lays, "circos_layouts")
    expect_setequal(names(lays), c("A", "B"))
    # each panel has only its own nodes and its own links
    expect_setequal(layout_nodes(lays$A)$id, c("a", "b", "c"))
    expect_setequal(layout_nodes(lays$B)$id, c("x", "y", "z"))
    expect_equal(nrow(layout_links(lays$A)), 2L)
    expect_equal(nrow(layout_links(lays$B)), 2L)
    # the panel value is recorded
    expect_equal(attr(lays$A, "panel"), "A")
    expect_equal(attr(lays$B, "panel"), "B")
})

test_that("each panel is laid out on its own, with its own arc widths", {
    # the point of the B-plan: arc width follows the panel's own node count,
    # so two panels do not have to agree on anything spatial
    nodes <- data.frame(
        id = c("a", "b", "c", "d", "x", "y"),
        grp = c(rep("A", 4), rep("B", 2)),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "b", "c", "x"),
        to   = c("b", "c", "d", "y"),
        grp  = c(rep("A", 3), "B"),
        stringsAsFactors = FALSE
    )
    lays <- circos_layout(links, nodes = nodes, by = "grp")
    # 4 nodes in A vs 2 in B: A's arcs are narrower, B's wider -- independent
    expect_equal(nrow(layout_nodes(lays$A)), 4L)
    expect_equal(nrow(layout_nodes(lays$B)), 2L)
    expect_lt(layout_nodes(lays$A)$width[1], layout_nodes(lays$B)$width[1])
})

test_that("a link's own group column files it; ids may repeat across groups", {
    nodes <- data.frame(
        id = c("a", "b", "c", "b", "c", "d"),
        grp = rep(c("A", "B"), each = 3),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("a", "b", "b", "c"),
        to   = c("b", "c", "c", "d"),
        grp  = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
    )
    lays <- circos_layout(links, nodes = nodes, by = "grp")
    expect_setequal(layout_nodes(lays$A)$id, c("a", "b", "c"))
    expect_setequal(layout_nodes(lays$B)$id, c("b", "c", "d"))
    expect_equal(nrow(layout_links(lays$A)), 2L)
    expect_equal(nrow(layout_links(lays$B)), 2L)
})

test_that("without a link group column, a spanning link is dropped with a message", {
    nodes <- data.frame(id = c("a", "b", "x", "y"),
                        grp = rep(c("A", "B"), each = 2), stringsAsFactors = FALSE)
    links <- data.frame(from = c("a", "a"), to = c("b", "y"), stringsAsFactors = FALSE)
    expect_message(lays <- circos_layout(links, nodes = nodes, by = "grp"),
                   "different")
    expect_equal(nrow(layout_links(lays$A)), 1L)  # a -> b
    expect_equal(nrow(layout_links(lays$B)), 0L)  # a -> y spans, dropped
})

test_that("ggcircos(by=) draws one ring per panel, each its own node set", {
    nodes <- data.frame(
        id = paste0("n", 1:8),
        grp = rep(c("s1", "s2"), each = 4),
        stringsAsFactors = FALSE
    )
    links <- data.frame(
        from = c("n1", "n2", "n3", "n5", "n6", "n7"),
        to   = c("n2", "n3", "n4", "n6", "n7", "n8"),
        grp  = rep(c("s1", "s2"), each = 3),
        stringsAsFactors = FALSE
    )
    p <- ggcircos(links, nodes = nodes, by = "grp") +
        geom_chord() + geom_node_arc() + ggplot2::facet_wrap(~grp)
    b <- ggplot2::ggplot_build(p)

    expect_equal(length(unique(b$layout$layout$PANEL)), 2L)
    # the node ring is NOT shared: each panel has its own 4 nodes
    arc_rows <- b$data[[2]]
    expect_equal(as.integer(table(arc_rows$PANEL)), c(4L, 4L))
    # the chords split too
    expect_equal(as.integer(table(b$data[[1]]$PANEL)), c(3L, 3L))
    expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("a by= plot refuses tracks and from/to chords clearly (first release)", {
    nodes <- data.frame(id = c("a", "b", "x", "y"),
                        grp = rep(c("A", "B"), each = 2), stringsAsFactors = FALSE)
    links <- data.frame(from = c("a", "x"), to = c("b", "y"),
                        grp = c("A", "B"), stringsAsFactors = FALSE)
    base <- ggcircos(links, nodes = nodes, by = "grp", tracks = c(t = 0.2))
    expect_error(base + geom_track(data = data.frame(id = "a", val = 1),
                                   ggplot2::aes(x = val), node = id, band = "t"),
                 class = "ggcircos_error_data")
    expect_error(ggcircos(links, nodes = nodes, by = "grp") +
                     geom_chord(data = links, from = from, to = to),
                 class = "ggcircos_error_data")
})

test_that("by= needs the grouping column on the node table", {
    links <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    expect_error(circos_layout(links, by = "grp"), class = "ggcircos_error_data")
    expect_error(
        circos_layout(links, nodes = data.frame(id = c("a", "b")), by = "grp"),
        class = "ggcircos_error_data"
    )
})
