#' Deprecated ggcircos API
#'
#' These are the 0.0.0.9000 entry points. They still work, but they warn and
#' forward to their 0.1.0 replacements; see the individual functions for how
#' the old arguments map.
#'
#' @name ggcircos-deprecated
#' @keywords internal
NULL

#' @rdname ggcircos-deprecated
#' @export
geom_curve_link <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            arrow = NULL,
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    deprecate_soft("0.1.0", "ggcircos::geom_curve_link()", "ggcircos::geom_chord(type = \"line\")")
    geom_chord(
        mapping = mapping, data = data, stat = stat, position = position,
        type = "line", ..., arrow = arrow, na.rm = na.rm,
        show.legend = show.legend, inherit.aes = inherit.aes
    )
}

#' @rdname ggcircos-deprecated
#' @param geom,mapping,pwidth,offset,grid.params,axis.params Legacy
#'   arguments, mapped onto [geom_track()] and [geom_track_axis()].
#' @export
geom_circos <- function(mapping,
                        geom,
                        data = NULL,
                        position = "auto",
                        pwidth = 0.3,
                        offset = 0.02,
                        inherit.aes = FALSE,
                        grid.params = NULL,
                        axis.params = NULL,
                        ...) {
    deprecate_soft("0.1.0", "ggcircos::geom_circos()", "ggcircos::geom_track()")
    node_col <- deprecated_circos_node(mapping)
    value <- mapping$x %||% deprecated_circos_x(mapping)
    band <- pwidth + offset
    # `missing(geom)` has to be asked *here*, where the user's call is still
    # visible. Passing `substitute(geom)` on hands the helper a missing-arg
    # marker, its own missing() test comes out FALSE, and as.character() of the
    # marker is "" -- so the documented default call used to fail with
    # `cannot translate ""` (B52).
    geom_name <- if (missing(geom)) "col" else geom_circos_geom_name(substitute(geom))
    args <- list(
        mapping = ggplot2::aes(x = !!rlang::enquo(value)),
        data = data,
        geom = geom_name,
        node = node_col,
        band = c(1 + offset, 1 + offset + band),
        width = 0.9,
        ...
    )
    out <- do.call(geom_track, args)
    out
}

#' @rdname ggcircos-deprecated
#' @export
geom_link_lab <- function(data = NULL,
                          mapping = NULL,
                          hjust = 0,
                          offset = 0,
                          geom = "text",
                          as_ylab = FALSE,
                          ...) {
    deprecate_soft("0.1.0", "ggcircos::geom_link_lab()", "ggcircos::geom_node_text()")
    geom_node_text(
        mapping = mapping, data = data,
        offset = offset, nudge_x = 0, ...
    )
}

#' @rdname ggcircos-deprecated
#' @export
as.tbl_lk <- function(x, ...) {
    deprecate_soft("0.1.0", "ggcircos::as.tbl_lk()", "ggcircos::circos_layout()")
    circos_layout(x, ...)
}

#' @rdname ggcircos-deprecated
#' @export
tidy_link <- function(x, radius = 1, ...) {
    deprecate_soft("0.1.0", "ggcircos::tidy_link()", "ggcircos::circos_layout()")
    layout_links(circos_layout(x, radius = radius, ...))
}

#' @keywords internal
#' @noRd
deprecate_soft <- function(when, what, with) {
    cli::cli_warn(
        c(
            "{.code {what}} was deprecated in ggcircos {when}.",
            i = "Use {.code {with}} instead."
        ),
        class = "ggcircos_warning_deprecated"
    )
    invisible(NULL)
}

#' @keywords internal
#' @noRd
deprecated_circos_node <- function(mapping) {
    if (!is.null(mapping$y)) {
        expr <- rlang::quo_get_expr(mapping$y)
        if (rlang::is_symbol(expr)) return(rlang::as_name(expr))
    }
    "label"
}

#' @keywords internal
#' @noRd
deprecated_circos_x <- function(mapping) {
    if (is.null(mapping$x)) {
        ggcircos_abort(
            "data",
            "{.fn geom_circos} needs an {.field x} in its {.arg mapping} for the value column."
        )
    }
    expr <- rlang::quo_get_expr(mapping$x)
    if (!rlang::is_symbol(expr)) {
        ggcircos_abort(
            "data",
            "{.fn geom_circos} needs a plain column name for {.field x}, not
             {.code {rlang::as_label(expr)}}."
        )
    }
    rlang::as_name(expr)
}

#' @keywords internal
#' @noRd
geom_circos_geom_name <- function(geom) {
    if (missing(geom) || is.null(geom)) geom <- quote(geom_col)
    name <- as.character(geom)
    name <- tail(name, 1)
    name <- sub("^geom_", "", name)
    known <- c("col", "bar", "tile", "point", "text", "area", "boxplot")
    if (!name %in% known) {
        ggcircos_abort(
            "data",
            "{.fn geom_circos} cannot translate {.val {name}}; use {.val {known}}.",
            i = "Or pass the geometry directly to {.fn geom_track}."
        )
    }
    name
}