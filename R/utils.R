#' @keywords internal
#' @noRd
wrap_2pi <- function(x) {
    x <- x %% (2 * pi)
    # keep [0, 2pi) exactly, %% can return 2pi for tiny negatives
    x[x >= 2 * pi] <- 0
    x
}

#' @keywords internal
#' @noRd
wrap_pi <- function(x) {
    x <- wrap_2pi(x)
    x[x > pi] <- x[x > pi] - 2 * pi
    x
}

#' Signed shortest angular distance from `a` to `b`, in `(-pi, pi]`.
#' @keywords internal
#' @noRd
ang_diff <- function(a, b) wrap_pi(b - a)

#' Raise a classed ggcircos error
#'
#' `message` is a cli string; anything it interpolates is looked up in the
#' calling frame, which keeps the call sites free of `.envir` bookkeeping.
#' @keywords internal
#' @noRd
ggcircos_abort <- function(.type, message, ..., call = rlang::caller_env()) {
    # interpolation variables: whatever the caller passed by name, else
    # whatever the caller has in scope
    env <- new.env(parent = parent.frame())
    vars <- list(...)
    if (length(vars) && !is.null(names(vars)) && all(nzchar(names(vars)))) {
        list2env(vars, env)
    }
    cli::cli_abort(
        message,
        class = c(paste0("ggcircos_error_", .type), "ggcircos_error"),
        call = call,
        .envir = env
    )
}

#' @keywords internal
#' @noRd
is_waive <- function(x) inherits(x, "waiver")

#' Lightweight replacement for ggplot2's internal `ggname()`.
#' @keywords internal
#' @noRd
ggname <- function(name, grob) {
    grob$name <- name
    grob
}

#' Affine rescale of `v` onto `[from, to]` with a fixed `base` point.
#'
#' Values equal to `base` map to `from`; `top` maps to `to`. Used by both
#' [position_track()] and the axis builder so they can never disagree.
#' @keywords internal
#' @noRd
rescale_from <- function(v, base, top, from, to) {
    if (is.na(top) || abs(top - base) < .Machine$double.eps^0.5) {
        out <- rep((from + to) / 2, length(v))
    } else {
        out <- from + (to - from) * (v - base) / (top - base)
    }
    out
}

#' `pretty()` breaks constrained to `limits`.
#' @keywords internal
#' @noRd
pretty_within <- function(limits, n = 4) {
    br <- pretty(limits, n = n)
    br[br >= limits[1] & br <= limits[2]]
}

#' Turn an unquoted column name (`enquo()`-ed) into a character value
#'
#' Interpolation variables live in this frame, so `ggcircos_abort()` finds
#' them without the caller passing `.envir`.
#' @keywords internal
#' @noRd
column_name <- function(quo, arg) {
    if (rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)) {
        ggcircos_abort(
            "data",
            "`{arg}` is required: give the unquoted column name of the node id, e.g. `node = id`.",
            arg = arg
        )
    }
    expr <- rlang::quo_get_expr(quo)
    # a literal string is accepted as well: it is common in wrappers and in
    # loops, where the column name is not known when the call is written
    if (is.character(expr) && length(expr) == 1L) {
        return(expr)
    }
    if (!rlang::is_symbol(expr)) {
        ggcircos_abort(
            "data",
            "`{arg}` must be a plain column name, not `{expr_str}`.",
            arg = arg,
            expr_str = rlang::as_label(expr)
        )
    }
    rlang::as_name(expr)
}

#' Column name that may also arrive as a variable holding a string
#'
#' Internal callers pass the already-resolved name (`node = node_col`), so the
#' promise can be a symbol bound to a character value. A symbol bound to
#' nothing is read as a column name, which keeps `node = id` working.
#' @keywords internal
#' @noRd
column_or_string <- function(quo, arg) {
    expr <- rlang::quo_get_expr(quo)
    if (is.character(expr) && length(expr) == 1L) return(expr)
    # `node = params$node` (internal) and `node = col` where `col` is a string
    # both resolve here; a symbol with nothing behind it is a column name
    val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
    if (is.character(val) && length(val) == 1L) return(val)
    column_name(quo, arg)
}

#' @keywords internal
#' @noRd
compact <- function(x) {
    x[!vapply(x, is.null, logical(1))]
}

#' Number of segments used when sampling a circular arc.
#' @keywords internal
#' @noRd
arc_samples <- function(ncp) max(2L, as.integer(ncp))
#' Tag a layer so its `ggplot_add()` method is found, and hand it back to
#' ggplot2 with the tag removed.
#'
#' Layers are plain ggplot2 layers; the class tag exists only so that
#' ggcircos can run a read-only hook before ggplot2 processes the layer.
#' @keywords internal
#' @noRd
tag_layer <- function(layer, class, settings = list()) {
    layer$ggcircos <- settings
    class(layer) <- c(class, class(layer))
    layer
}

#' ggcircos settings carried by a tagged layer
#' @keywords internal
#' @noRd
layer_settings <- function(object) object$ggcircos %||% list()

#' @keywords internal
#' @noRd
#' The multiple-layout list attached by `ggcircos(by = )`, or `NULL`
#' @keywords internal
#' @noRd
circos_layouts_of <- function(plot) {
    d <- plot$data
    if (!is.data.frame(d)) return(NULL)
    attr(d, "circos_layouts", exact = TRUE)
}

#' Stacked tables for the B-plan, with the grouping column
#'
#' `ggcircos(by = )` lays each group out separately and stacks the tables here
#' (spec sec.9.1). A layer that would read `lay$nodes` in the single-layout case
#' reads this instead; the extra column is what `facet_wrap()` splits on.
#' @keywords internal
#' @noRd
circos_stacked <- function(plot, kind = c("nodes", "sectors", "tracks")) {
    kind <- match.arg(kind)
    d <- plot$data
    nm <- paste0("circos_layouts_", kind)
    if (is.data.frame(d)) {
        v <- attr(d, nm, exact = TRUE)
        if (!is.null(v)) return(v)
    }
    lays <- circos_layouts_of(plot)
    if (is.null(lays)) return(NULL)
    key <- attr(d, "circos_by_key", exact = TRUE)
    getter <- switch(kind, nodes = layout_nodes, sectors = layout_sectors, tracks = layout_tracks)
    pieces <- lapply(names(lays), function(k) {
        z <- as.data.frame(getter(lays[[k]]), stringsAsFactors = FALSE)
        if (!is.null(key)) z[[key]] <- if (nrow(z)) k else character(0)
        z
    })
    do.call(rbind, pieces)
}

re_add <- function(object, plot, object_name) {
    class(object) <- setdiff(class(object), c(
        "ggcircos_chord", "ggcircos_node", "ggcircos_track", "ggcircos_axis"
    ))
    ggplot2::ggplot_add(object, plot, object_name)
}
