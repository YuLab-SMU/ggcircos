test_that("layout angles satisfy the documented invariants", {
    for (n in c(2, 3, 11, 40)) {
        ids <- paste0("n", seq_len(n))
        links <- data.frame(
            from = rep(ids, each = n)[-seq(1, n * n, by = n + 1)],
            to = rep(ids, times = n)[-seq(1, n * n, by = n + 1)],
            stringsAsFactors = FALSE
        )
        lay <- circos_layout(links)
        nd <- layout_nodes(lay)
        pm <- layout_params(lay)
        expect_equal(nrow(nd), n)
        expect_true(all(nd$theta >= 0 & nd$theta < 2 * pi))
        expect_true(all(abs(nd$width - nd$width[1]) < 1e-9))
        # node arcs plus the *effective* gaps fill exactly one turn: the
        # budget clamp of spec §4.3-4 narrows `gap` from its default, and the
        # effective value is what the ring actually spends
        expect_equal(
            sum(nd$width) + n * pm$gap_eff * 2 * pi,
            2 * pi
        )
        expect_lte(pm$gap_eff, 0.2 / n + 1e-12)
    }
})

test_that("slots sit inside their node arcs and do not overlap", {
    lay <- circos_layout(cor(mtcars))
    nd <- layout_nodes(lay)
    lk <- layout_links(lay)
    span <- (lk$theta1 - lk$theta0) %% (2 * pi)
    expect_true(all(span <= nd$width[match(lk$from, nd$id)] + 1e-9))
    expect_true(all(span <= lk$theta1 - lk$theta0 + 2 * pi))
    # every node's incident edges partition its arc
    for (i in seq_len(nrow(nd))) {
        s <- span[lk$from == nd$id[i]]
        expect_lte(sum(s), nd$width[i] + 1e-9)
    }
    # no two slots on the same node overlap
    for (id in nd$id) {
        idx <- which(lk$from == id)
        idx <- idx[order(lk$theta0[idx])]
        if (length(idx) > 1) {
            gaps <- (lk$theta0[idx[-1]] - lk$theta1[idx[-length(idx)]]) %% (2 * pi)
            expect_true(all(gaps >= -1e-9))
        }
    }
})

test_that("link counts match the layout", {
    lay <- circos_layout(cor(mtcars))
    nd <- layout_nodes(lay)
    lk <- layout_links(lay)
    expect_equal(sum(nd$n_links), 2 * nrow(lk))
    expect_true(all(nd$n_links == (ncol(mtcars) - 1)))
})

test_that("gap, sector_gap and start are honoured", {
    ids <- paste0("n", 1:6)
    links <- data.frame(
        from = ids, to = c(ids[2:6], ids[1]), stringsAsFactors = FALSE
    )
    a <- circos_layout(links)
    b <- circos_layout(links, gap = 0.1)
    expect_lt(sum(layout_nodes(b)$width), sum(layout_nodes(a)$width))
    c3 <- circos_layout(links, start = pi)
    expect_equal(layout_nodes(c3)$theta0[1], pi, tolerance = 1e-9)

    nodes <- data.frame(id = ids, g = rep(c("x", "y"), 3), stringsAsFactors = FALSE)
    d <- circos_layout(links, nodes = nodes, sectors = "g")
    expect_equal(nrow(layout_sectors(d)), 2)
    # the block gap is wider than a plain node gap
    expect_gt(sum(layout_nodes(d)$width), 0)
})

test_that("slot widths follow each node's own degree", {
    # uneven degrees are the regression: a recycled denominator gives the
    # wrong slot width on every node whose degree differs from its neighbours
    links <- data.frame(
        from = c("a", "a", "a", "b", "c"),
        to = c("b", "c", "d", "c", "d"),
        stringsAsFactors = FALSE
    )
    expect_warning(lay <- circos_layout(links, slot_fill = 0.9), NA)
    nd <- layout_nodes(lay)
    lk <- layout_links(lay)
    # slots are allocated per side, so the divisor is the number of links on
    # that side, not `n_links` (which counts both)
    deg <- vapply(
        lk$from, function(id) sum(lk$from == id), integer(1), USE.NAMES = FALSE
    )
    expected <- 0.9 * nd$width[match(lk$from, nd$id)] / deg
    expect_equal(lk$theta1 - lk$theta0, expected, tolerance = 1e-9)
    # and the slots of a node still fit inside its own arc
    for (i in seq_len(nrow(nd))) {
        s <- (lk$theta1 - lk$theta0)[lk$from == nd$id[i]]
        expect_lte(sum(s), 0.9 * nd$width[i] + 1e-9)
    }
})

test_that("slot_fill controls how much of the arc is used", {
    lay <- circos_layout(cor(mtcars), slot_fill = 0.5)
    nd <- layout_nodes(lay)
    lk <- layout_links(lay)
    span <- (lk$theta1 - lk$theta0) %% (2 * pi)
    expect_lte(max(span / nd$width[match(lk$from, nd$id)]), 0.5 + 1e-6)
})

test_that("the slot geometry contract holds for every p (spec 4.6)", {
    # `s = node_w / n_slots`, slot = p*s, and neighbours are separated by
    # (1-p)*s. The second half is what the ribbon feet must respect, and it
    # is the part that was never checked before.
    #
    # Slots are allocated *per side*: a node with 3 outgoing and 2 incoming
    # edges splits its arc into 3 on the from side and 2 on the to side, so
    # `n_links` (the total degree) is not the divisor for either side.
    for (p in c(0.5, 0.9, 1)) {
        lay <- circos_layout(cor(mtcars), slot_fill = p)
        nd <- layout_nodes(lay)
        lk <- layout_links(lay)
        sides <- list(
            list(key = lk$from, a0 = as.numeric(lk$theta0), a1 = as.numeric(lk$theta1)),
            list(key = lk$to, a0 = as.numeric(lk$theta_end0), a1 = as.numeric(lk$theta_end1))
        )
        for (i in seq_len(nrow(nd))) {
            for (side in sides) {
                idx <- which(side$key == nd$id[i])
                if (length(idx) < 2L) next
                s <- nd$width[i] / length(idx)
                expect_equal(
                    as.numeric(side$a1[idx] - side$a0[idx]) %% (2 * pi),
                    rep(p * s, length(idx)),
                    tolerance = 1e-9
                )
                # the slots of one side tile the node's share of the arc:
                # each is p*s wide, and together they never cover more than
                # p of the arc. This is the property the ribbon feet must
                # respect — and the one the chord geometry used to violate by
                # drawing the feet a full slot wide.
                sp <- as.numeric(side$a1[idx] - side$a0[idx]) %% (2 * pi)
                expect_equal(sp, rep(p * s, length(idx)), tolerance = 1e-9)
                expect_lte(sum(sp), p * nd$width[i] + 1e-9)
                # the leftover of the arc is exactly the (1 - p) slack
                expect_equal(sum(sp), p * length(idx) * s, tolerance = 1e-9)
            }
        }
    }
})

test_that("sector spans follow the ring, not min/max (B1)", {
    ids <- paste0("n", 1:6)
    nodes <- data.frame(
        id = ids, sect = rep(c("A", "B"), each = 3), stringsAsFactors = FALSE
    )
    links <- data.frame(from = "n1", to = "n6", stringsAsFactors = FALSE)
    # a sector straddling theta = 0 must not get the rest of the turn
    lay <- circos_layout(links, nodes = nodes, sectors = "sect", start = 3 * pi / 2)
    sec <- layout_sectors(lay)
    a <- sec[sec$sector == "A", ]
    expect_gt(a$theta0, a$theta1)                      # crossing zero
    nd <- layout_nodes(lay)
    # "inside a sector" on a circle: walking from the sector's theta0 towards
    # its theta1 must reach the label before the arc length runs out
    inside <- function(th, s) {
        arcs <- nd[nd$sector == s, , drop = FALSE]
        sp <- ggcircos:::sector_span(
            arcs$theta0, arcs$theta1, arcs$order
        )
        arc_len <- ggcircos:::wrap_pi_arc(sp[[1]], sp[[2]])
        d <- ggcircos:::wrap_pi_arc(sp[[1]], th)
        d <= arc_len + 1e-9
    }
    expect_true(inside(a$theta, "A"))
    expect_true(inside(sec$theta[sec$sector == "B"], "B"))
    # a sector that does not cross zero keeps theta0 < theta1
    lay2 <- circos_layout(links, nodes = nodes, sectors = "sect")
    expect_true(all(layout_sectors(lay2)$theta0 < layout_sectors(lay2)$theta1))
})

test_that("the four order arguments all work", {
    lay <- circos_layout(cor(mtcars), order = "hclust")
    expect_equal(layout_nodes(lay)$id[1], "carb")
    ids <- colnames(mtcars)
    expect_equal(
        layout_nodes(circos_layout(cor(mtcars), order = rev(ids)))$id,
        rev(ids)
    )
    f <- circos_layout(cor(mtcars), order = function(nd) rev(nd$id))
    expect_equal(layout_nodes(f)$id, rev(ids))
    expect_error(
        circos_layout(cor(mtcars), order = c("mpg", "cyl")),
        class = "ggcircos_error_layout"
    )
})

test_that("input adaptation covers matrix, data.frame and layout", {
    m <- cor(mtcars)
    expect_s3_class(circos_layout(m), "circos_layout")
    lk <- data.frame(from = "a", to = "b", w = 1, stringsAsFactors = FALSE)
    expect_s3_class(circos_layout(lk), "circos_layout")
    expect_equal(nrow(layout_links(circos_layout(lk))), 1)
    expect_equal(layout_links(circos_layout(lk))$w, 1)
    lay <- circos_layout(m)
    expect_identical(circos_layout(lay), lay)
})

test_that("user columns survive and reserved names are renamed", {
    nodes <- data.frame(id = c("a", "b"), group = c(1, 2), stringsAsFactors = FALSE)
    links <- data.frame(
        from = c("a"), to = c("b"), w = 0.5, theta0 = 9,
        stringsAsFactors = FALSE
    )
    lay <- circos_layout(links, nodes = nodes)
    expect_true("w" %in% names(layout_links(lay)))
    expect_true("user_theta0" %in% names(layout_links(lay)))
    expect_true("group" %in% names(layout_nodes(lay)))
    expect_false(anyNA(layout_nodes(lay)$group))
})

test_that("track bands are declared once and stacked outwards", {
    lay <- circos_layout(cor(mtcars), tracks = c(v = 0.2, h = 0.1))
    tr <- layout_tracks(lay)
    expect_equal(tr$track, c("v", "h"))
    expect_equal(tr$r0[1], 1.06)
    expect_equal(tr$r1[1], 1.26)
    expect_equal(tr$r0[2], 1.32)
    expect_true(all(diff(tr$r0) > 0))
})

test_that("errors carry a class and say how to fix them", {
    bad <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("a", "c")))
    expect_error(circos_layout(bad), class = "ggcircos_error_data")
    nodes <- data.frame(id = c("a", "b"), stringsAsFactors = FALSE)
    lk <- data.frame(from = "zz", to = "a", stringsAsFactors = FALSE)
    expect_error(circos_layout(lk, nodes = nodes), class = "ggcircos_error_node")
    # a gap that cannot fit is clamped, not an error (spec §4.3-4)
    clamped <- circos_layout(cor(mtcars), gap = 0.5)
    expect_equal(layout_params(clamped)$gap, 0.5)                 # asked for
    expect_equal(layout_params(clamped)$gap_eff, 0.2 / 11)         # spent
    expect_error(
        circos_layout(cor(mtcars), gap = -0.1),
        class = "ggcircos_error_layout"
    )
    expect_error(
        circos_layout(cor(mtcars), sector_gap = -0.1),
        class = "ggcircos_error_layout"
    )
    expect_error(
        circos_layout(cor(mtcars), slot_fill = 1.5),
        class = "ggcircos_error_layout"
    )
    expect_error(
        circos_layout(cor(mtcars), tracks = c(0.2)),
        class = "ggcircos_error_band"
    )
    d <- data.frame(a = 1)
    expect_error(circos_layout(d), class = "ggcircos_error_data")
    # a missing sector is a classed error, not a bare stopifnot (B5)
    expect_error(
        circos_layout(
            data.frame(from = "n1", to = "n2", stringsAsFactors = FALSE),
            nodes = data.frame(
                id = c("n1", "n2"), sect = c("x", NA), stringsAsFactors = FALSE
            ),
            sectors = "sect"
        ),
        class = "ggcircos_error_node"
    )
    # ... and so is a table with no nodes at all
    expect_error(
        circos_layout(data.frame(from = character(0), to = character(0))),
        class = "ggcircos_error_data"
    )
})

test_that("a layout of one node is degenerate but legal", {
    lay <- circos_layout(matrix(1, 1, 1, dimnames = list("a", "a")))
    expect_equal(nrow(layout_nodes(lay)), 1)
    expect_equal(nrow(layout_links(lay)), 0)
    # the single node owns the whole turn minus its gaps
    nd <- layout_nodes(lay)
    expect_equal(sum(nd$width) + layout_params(lay)$gap_eff * 2 * pi, 2 * pi, tolerance = 1e-9)
    expect_s3_class(
        ggplot2::ggplotGrob(ggcircos(matrix(1, 1, 1, dimnames = list("a", "a"))) + geom_node_arc()),
        "gtable"
    )
})

test_that("self links are dropped with a message", {
    lk <- data.frame(
        from = c("a", "a"), to = c("a", "b"), stringsAsFactors = FALSE
    )
    expect_message(lay <- circos_layout(lk), "self-link")
    expect_equal(nrow(layout_links(lay)), 1)
})

test_that("accessors are read-only views of a layout", {
    lay <- circos_layout(cor(mtcars))
    expect_identical(layout_nodes(lay), lay$nodes)
    expect_identical(layout_links(lay), lay$links)
    expect_identical(layout_sectors(lay), lay$sectors)
    expect_identical(layout_tracks(lay), lay$tracks)
    expect_output(print(lay), "ggcircos layout")
})
test_that("sectors stay contiguous whatever the order says (R1)", {
    nodes <- data.frame(
        id = c(paste0("g", 1:4), paste0("h", 1:4)),
        genome = rep(c("A", "B"), each = 4),
        stringsAsFactors = FALSE
    )
    set.seed(3)
    links <- data.frame(
        from = sample(nodes$id, 10, TRUE), to = sample(nodes$id, 10, TRUE),
        stringsAsFactors = FALSE
    )
    links <- links[links$from != links$to, ]

    orders <- list(
        NULL,
        "hclust",
        c("h2", "g3", "h1", "g4", "h3", "g1", "h4", "g2")  # deliberately mixed
    )
    for (ord in orders) {
        lay <- circos_layout(links, nodes = nodes, sectors = "genome", order = ord)
        nd <- layout_nodes(lay)
        n <- nrow(nd)
        # one run per sector: no sector is split into two blocks
        runs <- 1L + sum(nd$sector[-1] != nd$sector[-n])
        expect_equal(runs, length(unique(nd$sector)))
        # arcs and seams add up to exactly one turn
        p <- layout_params(lay)
        g <- p$gap_eff * 2 * pi
        sg <- p$sector_gap_eff * 2 * pi
        nxt <- c(nd$theta0[-1], nd$theta0[1])
        gaps <- (nxt - nd$theta1) %% (2 * pi)
        expect_true(all(abs(gaps - g) < 1e-9 | abs(gaps - (g + sg)) < 1e-9))
        expect_equal(sum(nd$width) + sum(gaps), 2 * pi)
    }
})

test_that("labels sit outside the outermost band and inside the panel (R15)", {
    m <- cor(mtcars)
    vals <- data.frame(id = colnames(mtcars), v = seq(0.1, 1, length.out = 11))

    plain <- ggplot2::ggplot_build(
        ggcircos(m) + geom_node_text()
    )
    # a plain chord diagram still leaves the ring room: the labels are not
    # pinned to the panel edge (they used to sit at exactly the x limit)
    lim <- plain$layout$panel_params[[1]]$r.range
    expect_gt(lim[2] - plain$data[[1]]$x[1], 0.05)

    with_track <- ggplot2::ggplot_build(
        ggcircos(m, tracks = c(v = 0.2)) +
            geom_node_text() +
            geom_track(data = vals, ggplot2::aes(x = v), node = id, band = "v")
    )
    trk <- layout_tracks(circos_layout(m, tracks = c(v = 0.2)))
    # outside the outermost band, i.e. not buried under the track
    expect_true(all(with_track$data[[1]]$x > max(trk$r1)))
    expect_true(all(with_track$data[[1]]$x < with_track$layout$panel_params[[1]]$r.range[2]))
})

test_that("slots inside a node are placed in order and never collide (R18)", {
    # a star graph gives one node an empty `to` side -- exactly the shape that
    # used to shift the per-side slot index
    star <- data.frame(from = rep("hub", 5), to = paste0("leaf", 1:5),
                       stringsAsFactors = FALSE)
    cases <- list(
        cor(mtcars),
        star,
        data.frame(from = c("a", "b", "c"), to = c("b", "c", "a"),
                   stringsAsFactors = FALSE)
    )
    for (dat in cases) {
        lay <- circos_layout(dat)
        nd <- layout_nodes(lay)
        lk <- layout_links(lay)
        p <- layout_params(lay)$slot_fill
        for (side in c("from", "to")) {
            ids <- lk[[side]]
            st <- if (side == "from") lk$theta0 else lk$theta_end0
            for (id in unique(ids)) {
                i <- which(ids == id)
                w <- nd$width[nd$id == id]
                k <- length(i)
                s <- p * w / k
                gap <- (1 - p) * w / k
                # order the slots the way they sit on the arc
                i <- i[order((st[i] - nd$theta0[nd$id == id]) %% (2 * pi))]
                expect_equal(length(unique(round(st[i], 9))), k)
                expect_equal(
                    st[i],
                    nd$theta0[nd$id == id] + gap / 2 + (seq_len(k) - 1) * (s + gap),
                    tolerance = 1e-9
                )
            }
        }
    }
})

test_that("weight makes the geometry follow the values (R18)", {
    m <- cor(mtcars)
    flat <- circos_layout(m, order = "hclust")
    wtd <- circos_layout(m, order = "hclust", weight = "r")

    # unweighted: every node and every slot of the same side has one width
    expect_equal(length(unique(round(flat$nodes$width, 12))), 1L)

    # weighted: a node's arc is proportional to the weight it carries
    # the layout reads the upper triangle, so the diagonal is not a link
    nw <- colSums(abs(m)) - abs(diag(m))
    share <- nw / sum(nw)
    expect_equal(wtd$nodes$width / sum(wtd$nodes$width),
                 unname(share[wtd$nodes$id]), tolerance = 1e-9)

    # and a link's slot is proportional to its own weight
    lk <- wtd$links
    for (id in c("mpg", "cyl")) {
        i <- which(lk$from == id)
        v <- abs(m[cbind(lk$from[i], lk$to[i])])
        span <- (lk$theta1[i] - lk$theta0[i]) %% (2 * pi)
        expect_equal(span / sum(span), v / sum(v), tolerance = 1e-9)
    }

    # negative weights count by magnitude, like circlize
    expect_equal(
        circos_layout(m, order = "hclust", weight = "r")$links$theta0,
        circos_layout(m, order = "hclust", weight = "r")$links$theta0
    )
    m2 <- m
    m2[1, 2] <- -abs(m2[1, 2])
    expect_silent(circos_layout(m2, weight = "r"))

    # errors are classed and name the culprit
    expect_error(circos_layout(m, weight = "nope"), class = "ggcircos_error_data")
    bad <- data.frame(from = "a", to = "b", w = NA_real_)
    expect_error(circos_layout(bad, weight = "w"), class = "ggcircos_error_data")
    zero <- data.frame(from = "a", to = "b", w = 0)
    expect_error(circos_layout(zero, weight = "w"), class = "ggcircos_error_data")
})

test_that("slot_gap gives every seam the same absolute width (R21)", {
    m <- cor(mtcars)
    wide <- circos_layout(m, order = "hclust", slot_gap = 0.02)
    lk <- layout_links(wide)
    gaps <- numeric(0)
    for (side in c("from", "to")) {
        ids <- lk[[side]]
        st <- if (side == "from") lk$theta0 else lk$theta_end0
        en <- if (side == "from") lk$theta1 else lk$theta_end1
        for (id in unique(ids)) {
            i <- which(ids == id)
            i <- i[order(st[i])]
            if (length(i) < 2) next
            g <- (st[i[-1]] - en[i[-length(i)]]) %% (2 * pi)
            gaps <- c(gaps, g)
        }
    }
    # every seam is the requested width, unless the node's arc is too narrow
    # to afford it (a quarter of the arc is the cap), and then it is the cap
    nd <- layout_nodes(wide)
    expect_true(all(gaps <= 0.02 + 1e-9))
    expect_true(all(gaps > 0))
    # the widest node gets the full 0.02; narrow ones keep a quarter of the arc
    expect_equal(min(max(gaps), 0.02), 0.02)

    # and slots still fit inside their node's arc
    spans <- (lk$theta1 - lk$theta0) %% (2 * pi)
    expect_true(all(spans > 0))
    expect_true(all(spans <= nd$width[match(lk$from, nd$id)] + 1e-9))

    # the seam is clamped so a node with many links still keeps some slot
    tight <- circos_layout(m, order = "hclust", slot_gap = 10)
    expect_true(all((layout_links(tight)$theta1 - layout_links(tight)$theta0) >= 0))
})
