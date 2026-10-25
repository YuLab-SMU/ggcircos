# A figure with nothing in it must fail.
#
# Every structural test passes on a blank plot: the grob builds, the layer
# count is right, no error is raised. README's layout-as-data figure was blank
# for a while -- its chunk drew every segment at a fixed `x` on a scale that
# only spans the data -- and no test noticed; an eyeball did.
#
# These check ink coverage on figures built the way the docs build them, which
# is the path the test suite otherwise never walks: a ggplot assembled by hand
# from `layout_nodes()`, bypassing `ggcircos()`.

ink_of <- function(plot, file = tempfile(fileext = ".png")) {
    ggplot2::ggsave(file, plot, width = 4.2, height = 4.2, dpi = 100)
    d <- png::readPNG(file)
    rgb <- d[, , 1:3, drop = FALSE]
    mean(apply(rgb, c(1, 2), function(p) any(p < 0.98)))
}

test_that("a hand-built circular plot from layout_nodes() is not blank", {
    skip_if_not_installed("png")
    nd <- layout_nodes(circos_layout(cor(mtcars)[1:5, 1:5], order = "hclust"))
    # the shape used by the README: the radius axis is an ordinary axis
    p <- ggplot2::ggplot(nd, ggplot2::aes(xmin = 1, xmax = 1.03,
                                          ymin = theta0, ymax = theta1)) +
        ggplot2::geom_rect(ggplot2::aes(fill = order)) +
        ggplot2::geom_point(ggplot2::aes(x = 1.08, y = theta), size = 2.5,
                            inherit.aes = FALSE) +
        coord_circos(limits = c(0, 2 * pi)) +
        theme_circos(legend.position = "none")
    expect_gt(ink_of(p), 0.02)
})

test_that("a blank figure is caught by the same check", {
    skip_if_not_installed("png")
    # the guard has to fail on the thing it exists for
    p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
        ggplot2::geom_blank() + coord_circos(limits = c(0, 2 * pi)) +
        theme_circos()
    expect_lt(ink_of(p), 0.005)
})
