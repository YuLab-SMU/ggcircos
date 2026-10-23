# Boundary inputs (C2): each case asserts the *class* of the outcome, not just
# "does not crash", because a silent wrong answer is worse than an error.

test_that("an empty link table is a legal layout with a full column set (B2)", {
    lk <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    # with no nodes either, there is nothing to lay out
    expect_error(circos_layout(lk), class = "ggcircos_error_data")

    lay <- circos_layout(lk, nodes = data.frame(id = c("a", "b"), stringsAsFactors = FALSE))
    expect_equal(nrow(layout_links(lay)), 0L)
    expect_true(all(c("theta0", "theta1", "theta_end0", "theta_end1", "x", "y") %in%
                    names(layout_links(lay))))
    expect_equal(nrow(layout_nodes(lay)), 2L)
    # and it still builds a plot
    expect_s3_class(ggplot2::ggplotGrob(ggcircos(lay) + geom_node_arc()), "gtable")
})

test_that("a single node with no links owns the whole turn (minus gaps)", {
    lay <- circos_layout(data.frame(from = character(0), to = character(0),
                                    stringsAsFactors = FALSE),
                         nodes = data.frame(id = "a", stringsAsFactors = FALSE))
    nd <- layout_nodes(lay)
    expect_equal(nrow(nd), 1L)
    expect_equal(nd$n_links, 0L)
    # one node still spans most of the circle
    expect_gt(nd$width, 2 * pi - 0.1)
})

test_that("a node table without `id` falls back to its first column; duplicates error", {
    lk <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    # no `id` column: the first column is taken as the id (a deliberate
    # convenience, so `data.frame(name = )` works)
    lay <- circos_layout(lk, nodes = data.frame(name = c("a", "b"),
                                                stringsAsFactors = FALSE))
    expect_setequal(layout_nodes(lay)$id, c("a", "b"))
    # duplicated ids are a genuine mistake though
    expect_error(circos_layout(lk, nodes = data.frame(id = c("a", "a", "b"),
                                                      stringsAsFactors = FALSE)),
                 class = "ggcircos_error_node")
})

test_that("a link to a node not in an explicit node table is a classed error", {
    lk <- data.frame(from = "a", to = "zz", stringsAsFactors = FALSE)
    expect_error(circos_layout(lk, nodes = data.frame(id = c("a", "b"),
                                                      stringsAsFactors = FALSE)),
                 class = "ggcircos_error_node")
    # ... but with no node table, an unseen id simply becomes a node
    lay <- circos_layout(lk)
    expect_setequal(layout_nodes(lay)$id, c("a", "zz"))
})

test_that("NA sector, negative gaps and NA links are refused with a class", {
    lk <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    expect_error(
        circos_layout(lk, nodes = data.frame(id = c("a", "b"), sec = c(NA, "x")),
                      sectors = "sec"),
        class = "ggcircos_error_node"
    )
    expect_error(circos_layout(lk, gap = -0.1), class = "ggcircos_error_layout")
    expect_error(circos_layout(lk, sector_gap = -0.1), class = "ggcircos_error_layout")
    expect_error(
        circos_layout(data.frame(from = c("a", "b"), to = c("b", NA),
                                 stringsAsFactors = FALSE)),
        class = "ggcircos_error_data"
    )
})

test_that("a matrix with NA drops that link but keeps the nodes", {
    m <- matrix(c(1, NA, NA, 1), 2, 2,
                dimnames = list(c("a", "b"), c("a", "b")))
    lay <- circos_layout(m)
    # upper triangle only; the single NA cell is the only off-diagonal cell, so
    # there are no links but both nodes remain
    expect_equal(nrow(layout_links(lay)), 0L)
    expect_setequal(layout_nodes(lay)$id, c("a", "b"))
})

test_that("a non-square matrix and a mismatched-names matrix are refused (B3)", {
    m <- matrix(1:6, 2, 3, dimnames = list(c("a", "b"), c("x", "y", "z")))
    expect_error(circos_layout(m), class = "ggcircos_error_data")
    m2 <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("c", "d")))
    expect_error(circos_layout(m2), class = "ggcircos_error_data")
})

test_that("weight must be a real, finite column (R4)", {
    lk <- data.frame(from = c("a", "b"), to = c("b", "c"), w = c(1, Inf),
                     stringsAsFactors = FALSE)
    expect_error(circos_layout(lk, weight = "w"), class = "ggcircos_error_data")
    lk$w <- c("x", "y")
    expect_error(circos_layout(lk, weight = "w"), class = "ggcircos_error_data")
    expect_error(circos_layout(lk[1:2, ], weight = "nope"), class = "ggcircos_error_data")
})

test_that("the non-square matrix message is well formed (B50)", {
    # a malformed cli template does not break the code, it only shows up when
    # someone really hits that error -- and it is the first thing the package
    # ever says to them. Assert the text, not just that it errors.
    msg <- conditionMessage(tryCatch(ggcircos(matrix(1:6, 2, 3)), error = identity))
    expect_match(msg, "2x3", fixed = TRUE)
    expect_false(grepl("}", msg, fixed = TRUE))
    expect_match(msg, "must be square", fixed = TRUE)
})
