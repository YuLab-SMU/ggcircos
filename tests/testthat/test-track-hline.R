# Threshold lines in a track band (spec sec.6.4b)
#
# `geom_hline()` draws on the angle axis because a track's `y` is an angle;
# `geom_track_hline()` takes a value in the band's own units and runs it
# through the same normalisation the data went through.

hline_setup <- function(scales = "fixed", limits = c(0, 1)) {
    links <- data.frame(from = c("a", "b"), to = c("b", "c"), stringsAsFactors = FALSE)
    lay <- circos_layout(links, tracks = c(g = 0.2))
    trk <- data.frame(id = c("a", "b", "c"), v = c(0.2, 0.5, 0.8),
                      stringsAsFactors = FALSE)
    list(
        lay = lay,
        plot = ggcircos(lay) +
            geom_track(data = trk, ggplot2::aes(x = v), node = id, band = "g",
                       scales = scales, limits = limits)
    )
}

test_that("the threshold is a value, not an angle (6.4b-1)", {
    o <- hline_setup()
    p <- o$plot + geom_track_hline(band = "g", value = 0.5)
    h <- ggplot2::ggplot_build(p)$data[[2]]
    band <- c(o$lay$tracks$r0[1], o$lay$tracks$r1[1])
    # 0.5 of a [0, 1] scale is the middle of the band, NOT theta = 0.5
    expect_equal(unique(h$x), mean(band), tolerance = 1e-9)
    expect_false(isTRUE(all.equal(unique(h$x), 0.5)))
})

test_that("the line follows the layer's own scales and limits (6.4b-2)", {
    # same value, different limits: the radius has to move with the data
    a <- hline_setup(limits = c(0, 1))
    b <- hline_setup(limits = c(0, 2))
    ha <- ggplot2::ggplot_build(a$plot + geom_track_hline(band = "g", value = 0.5))$data[[2]]
    hb <- ggplot2::ggplot_build(b$plot + geom_track_hline(band = "g", value = 0.5))$data[[2]]
    band <- c(a$lay$tracks$r0[1], a$lay$tracks$r1[1])
    expect_equal(unique(ha$x), band[1] + 0.5 * diff(band), tolerance = 1e-9)
    expect_equal(unique(hb$x), band[1] + 0.25 * diff(band), tolerance = 1e-9)
})

test_that("the line is drawn per node and skips the gaps (6.4b-3)", {
    o <- hline_setup()
    p <- o$plot + geom_track_hline(band = "g", value = 0.5)
    h <- ggplot2::ggplot_build(p)$data[[2]]
    nd <- layout_nodes(o$lay)
    expect_equal(nrow(h), nrow(nd))
    # each arc lies inside its node's own span
    for (i in seq_len(nrow(h))) {
        expect_gte(h$y[i], nd$theta0[i] - 1e-9)
        expect_lte(h$yend[i], nd$theta1[i] + 1e-9)
    }
})

test_that("values outside the band are clipped and reported (6.4b-4)", {
    o <- hline_setup()
    band <- c(o$lay$tracks$r0[1], o$lay$tracks$r1[1])
    expect_message(p <- o$plot + geom_track_hline(band = "g", value = 2))
    h <- ggplot2::ggplot_build(p)$data[[2]]
    expect_equal(unique(h$x), band[2], tolerance = 1e-9)
})

test_that("a vector of values draws one line each (6.4b-5)", {
    o <- hline_setup()
    p <- o$plot + geom_track_hline(band = "g", value = c(0, 0.5, 1))
    h <- ggplot2::ggplot_build(p)$data[[2]]
    band <- c(o$lay$tracks$r0[1], o$lay$tracks$r1[1])
    radii <- sort(unique(h$x))
    # three thresholds at 0, 0.5 and 1 of the scale: both edges and the middle
    expect_equal(radii, c(band[1], mean(band), band[2]), tolerance = 1e-9)
    expect_equal(length(unique(h$group)), 3L)
})
