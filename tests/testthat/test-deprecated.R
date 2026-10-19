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
