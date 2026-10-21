# Extracted from test-demos.R:239

# test -------------------------------------------------------------------------
m <- cor(mtcars)[1:6, 1:6]
base <- ggcircos(m, order = "hclust")
nd <- layout_nodes(circos_layout(m, order = "hclust"))
expect_length(base$mapping, 0L)
expect_s3_class(ggplot2::ggplotGrob(
        base + geom_point(data = nd, aes(x = r, y = theta), size = 2)), "gtable")
