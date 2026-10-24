test_that("deprecated wrappers warn and still work", {
    set.seed(1)
    vals <- data.frame(
        id = rep(colnames(mtcars), each = 4),
        value = runif(44),
        stringsAsFactors = FALSE
    )
    base <- ggcircos(cor(mtcars), order = "hclust", tracks = c(values = 0.2))

    expect_warning(
        expect_s3_class(ggplot2::ggplotGrob(base + geom_curve_link()), "gtable"),
        class = "ggcircos_warning_deprecated"
    )
    expect_warning(
        expect_s3_class(ggplot2::ggplotGrob(base + geom_link_lab()), "gtable"),
        class = "ggcircos_warning_deprecated"
    )
    expect_warning(
        expect_s3_class(
            ggplot2::ggplotGrob(base + geom_circos(mapping = ggplot2::aes(x = value, y = id), geom = ggplot2::geom_col, data = vals)),
            "gtable"
        ),
        class = "ggcircos_warning_deprecated"
    )
    expect_warning(
        expect_s3_class(as.tbl_lk(cor(mtcars)), "circos_layout"),
        class = "ggcircos_warning_deprecated"
    )
    expect_warning(
        expect_true(is.data.frame(tidy_link(cor(mtcars)))),
        class = "ggcircos_warning_deprecated"
    )
})

test_that("geom_circos() works without a geom argument (B52)", {
    # `geom` is documented as optional and defaults to a column track. The
    # missing-check used to live in the helper, where the argument has already
    # been supplied, so the default call failed with `cannot translate ""`.
    old <- data.frame(label = c("chr1", "chr2"), value = c(3, 7),
                      stringsAsFactors = FALSE)
    base <- ggcircos(data.frame(from = "chr1", to = "chr2"),
                     nodes = data.frame(id = c("chr1", "chr2")))
    expect_warning(
        expect_s3_class(
            ggplot2::ggplotGrob(base + geom_circos(mapping = ggplot2::aes(x = value), data = old)),
            "gtable"
        ),
        class = "ggcircos_warning_deprecated"
    )
})
