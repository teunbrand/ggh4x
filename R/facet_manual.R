#' @include facet_wrap2.R
NULL

# Constructor -------------------------------------------------------------

#' Manual layout for panels
#'
#' In `facet_manual()` the layout for panels is determined by a custom design.
#' Inspired by base-R graphics [`layout()`][graphics::layout()] function, this
#' variant of facets offers more freedom in how panels are displayed, but
#' comes with less guarantees that it looks right.
#'
#' @param design Specification of panel areas in the layout. Can either be
#'   specified as a `character(1)` string or as a `matrix`. See examples.
#' @param widths,heights A `numeric` or `unit` vector setting the sizes of
#'   panels. A `numeric` vector is converted to relative `"null"` units.
#'   Alternatively, when `NULL` (default), the sizes are set per instructions of
#'   coord or theme aspect ratio. Note that these widths and heights apply to
#'   the cells where panels can be drawn. In between such cells, room will be
#'   made to fit plot decoration such as paddings, axes and strips.
#' @param respect A `logical(1)`. If `TRUE`, widths and heights specified in
#'   `"null"` units are proportional. If `FALSE`, `"null"` units in the x- and
#'   y-directions can vary independently. Alternatively, when `NULL`, the
#'   `respect` parameter takes instructions from the coord or theme.
#' @param trim_blank A `logical(1)`. When `TRUE` (default), the design will
#'   be trimmed to remove empty rows and columns.
#' @inheritParams facet_wrap2
#'
#' @return A `Facet` ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#' @md
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#'
#' # The `design` argument can be a character string.
#' # New rows are indicated by newline symbol (`\n`), which are added
#' # automatically for multi-line strings.
#' # The `#`-symbol indicates empty cells.
#' design <- "
#'  A##
#'  AB#
#'  #BC
#'  ##C
#' "
#' p + facet_manual(~ cyl, design)
#'
#' # Alternatively, the `design` argument can be a matrix.
#' # Using `NA`s will leave the cell empty.
#' design <- matrix(c(1,2,3,3), 2, 2, byrow = TRUE)
#' p + facet_manual(~ cyl, design)
#'
#' # The sizes of columns and rows can be adjusted with the `widths` and
#' # `heights`parameters respectively.
#' p + facet_manual(
#'   ~ cyl, t(design),
#'   widths = c(2, 1), heights = c(2, 1), respect = TRUE
#' )
facet_manual <- function(
  facets,
  design = NULL,
  widths = NULL,
  heights = NULL,
  respect = FALSE,
  drop = TRUE,
  strip.position = "top",
  scales = "fixed",
  labeller = "label_value",
  trim_blank = TRUE,
  strip = strip_vanilla()
) {
  strip.position <- arg_match0(
    strip.position, c("top", "bottom", "left", "right")
  )

  design <- validate_design(design, trim_blank)
  facets <- .int$wrap_as_facets_list(facets)

  if (length(facets) == 0) {
    return(facet_null())
  }

  if (!is.unit(widths) && !is.null(widths)) {
    widths <- unit(widths, "null")
  }
  if (!is.unit(heights) && !is.null(heights)) {
    heights <- unit(heights, "null")
  }
  dim <- dim(design)

  if (!is.null(widths)) {
    widths  <- rep_len(widths,  dim[2])
  }
  if (!is.null(heights)) {
    heights <- rep_len(heights, dim[1])
  }

  free  <- .match_facet_arg(scales, c("fixed", "free_x", "free_y", "free"))
  strip <- assert_strip(strip)

  params <- list(
    design = design,
    facets = facets,
    widths = widths,
    heights = heights,
    respect = respect,
    strip.position = strip.position,
    labeller = labeller,
    drop = drop,
    nrow = dim[1],
    ncol = dim[2],
    free = free,
    dim = dim
  )

  ggproto(
    NULL, FacetManual,
    params = params,
    strip = strip
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetManual <- ggproto(
  "FacetManual", FacetWrap2,

  compute_layout = function(data, params) {

    vars <- params$facets
    if (length(vars) == 0) {
      df <- .int$new_data_frame(list(
        .TOP = 1, .RIGHT = 1, .BOTTOM = 1, .LEFT = 1,
        PANEL = factor(1), SCALE_X = 1, SCALE_Y = 1
      ))
      return(df)
    }

    # Translate design to layout
    design <- params$design
    rows <- vapply(split(row(design), design), range, integer(2))
    cols <- vapply(split(col(design), design), range, integer(2))
    id   <- vapply(split(design, design), unique, integer(1))
    layout <- .int$new_data_frame(list(
      .TOP = rows[1,],
      .RIGHT = cols[2,],
      .BOTTOM = rows[2,],
      .LEFT = cols[1,],
      PANEL = factor(id, levels = unique(id))
    ))

    base <- combine_vars(data, params$plot_env, vars, drop = params$drop)
    rownames(base) <- NULL
    id <- .int$id(base, drop = TRUE)
    n <- attr(id, "n")

    if (n > nrow(layout)) {
      n  <- nrow(layout)
      id <- id[seq_len(n)]
      rlang::warn(paste0(
        "Found more facetting levels than designed. The following levels are ",
        "dropped: ", paste0(base[[1]][-seq_len(n)], collapse = ", ")
      ))
      base <- base[id, , drop = FALSE]
    }
    lnames <- attr(layout, "design_names")
    isect  <- intersect(lnames, base[[1]])
    if (length(isect) != 0 && length(isect) != nrow(base)) {
      rlang::warn(paste0(
        "Only partial match found between facetting levels and design levels."
      ))
    } else if (length(isect) > 0){
      base <- base[match(base[[1]], isect), , drop = FALSE]
    }
    if (n < nrow(layout)) {
      keep   <- as.integer(as.character(layout$PANEL)) <= n
      layout <- layout[keep, , drop = FALSE]
      layout$PANEL <- droplevels(layout$PANEL)
    }

    panels <- cbind(layout, base[id, , drop = FALSE])
    panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
    panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L
    panels
  },

  map_data = function(data, layout, params) {

    if (.int$empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    vars <- params$facets

    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }

    facet_vals <- .int$eval_facets(vars, data, params$.possible_columns)
    facet_vals[] <- lapply(facet_vals[], as.factor)

    missing_facets <- setdiff(names(vars), names(facet_vals))
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])
      data_rep  <- rep.int(seq_nrow(data), nrow(to_add))
      facet_rep <- rep(seq_nrow(to_add), each = nrow(data))

      data <- data[data_rep, , drop = FALSE]
      rownames(data) <- NULL

      facet_vals <- cbind(
        facet_vals[data_rep, , drop = FALSE],
        to_add[facet_rep, , drop = FALSE]
      )
      rownames(facet_vals) <- NULL
    }

    keys <- .int$join_keys(facet_vals, layout, by = names(vars))
    data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    data <- data[!is.na(data$PANEL),] # Drop data
    data
  },

  setup_aspect_ratio = function(coord, free, theme, ranges) {
    # Rules for aspect ratio
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio) && !free$x && !free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }
    if (!is.null(aspect_ratio)) {
      attr(aspect_ratio, "respect") <- TRUE
    }
    aspect_ratio
  },

  setup_axes = function(axes, layout, params, theme) {
    # ncol  <- max(layout$.LEFT, layout$.RIGHT)
    # nrow  <- max(layout$.TOP, layout$.BOTTOM)
    panel <- as.integer(layout$PANEL)

    .int$new_data_frame(list(
      t = panel, b = panel, l = panel, r = panel,
      axes_top    = axes$x$top[layout$SCALE_X],
      axes_bottom = axes$x$bottom[layout$SCALE_X],
      axes_left   = axes$y$left[layout$SCALE_Y],
      axes_right  = axes$y$right[layout$SCALE_Y]
    ))
  },

  attach_axes = function(panels, axes, sizes ) {
    # Top axis
    panels <- weave_panel_rows(panels, axes, -1, sizes$top,
                               "axis-t", 3, "off", "t", "axes_top")
    # Bottom axis
    panels <- weave_panel_rows(panels, axes, 0, sizes$bottom,
                               "axis-b", 3, "off", "b", "axes_bottom")
    # Left axis
    panels <- weave_panel_cols(panels, axes, -1, sizes$left,
                               "axis-l", 3, "off", "l", "axes_left")
    # Right axis
    panels <- weave_panel_cols(panels, axes, 0, sizes$right,
                               "axis-r", 3, "off", "r", "axes_right")
    panels
  },

  draw_panels = function(self, panels, layout,
                         x_scales, y_scales,
                         ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      stop(.int$snake_class(coord), " doesn't support free scales",
           call. = FALSE)
    }
    strip <- self$strip

    # Setup panels
    panels <- self$setup_panel_table(
      panels, layout, theme, coord, ranges, params
    )

    # Deal with axes
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
    axes <- self$setup_axes(axes, layout, params, theme)
    dims <- panels$layout[grepl("^panel", panels$layout$name),
                          c("t", "b", "l", "r"), drop = FALSE]
    sizes <- list(
      top    = split_heights_cm(axes$axes_top,    split = dims$t),
      bottom = split_heights_cm(axes$axes_bottom, split = dims$b),
      left   = split_widths_cm(axes$axes_left,    split = dims$l),
      right  = split_widths_cm(axes$axes_right,   split = dims$r)
    )
    panels <- self$attach_axes(panels, axes, sizes)

    # Deal with strips
    simplify <- switch(
      params$strip.position,
      top = c(".TOP", ".LEFT"),
      bottom = c(".BOTTOM", ".LEFT"),
      left = c(".TOP", ".LEFT"),
      right = c(".TOP", ".RIGHT")
    )
    layout[, c("ROW", "COL")] <- layout[, simplify]

    strip$setup(layout, params, theme, type = "wrap")
    panels <- strip$incorporate_wrap(
      panels, params$strip.position, clip = coord$clip, sizes
    )

    self$finish_panels(panels = panels, layout = layout,
                       params = params, theme = theme)

  }
)

# Helpers -----------------------------------------------------------------

validate_design <- function(design = NULL, trim = TRUE) {
  if (is.null(design)) {
    stop("Cannot interpret design.", call. = FALSE)
  }
  # Inspired by patchwork:::as_areas()
  if (is.character(design)) {
    x <- strsplit(design, "\n")[[1]]
    x <- unname(vapply(x, trimws, character(1)))
    x <- x[nzchar(x)]
    x <- strsplit(x, "")
    ncols <- lengths(x)
    if (length(unique(ncols)) != 1) {
      stop("Design must be rectangular.", call. = FALSE)
    }
    nrow <- length(x)
    x <- unlist(x)
    design <- matrix(x, nrow, ncols[1], byrow = TRUE)
  }
  # Attempt to force matrix
  if (!is.matrix(design) && is.atomic(design)) {
    design <- as.matrix(design)
  }
  if (is.matrix(design)) {
    dim <- dim(design)
    if (length(dim) != 2 || any(dim < 1) || any(is.na(dim))) {
      stop("The `design` argument has invalid dimensions.")
    }
    if (typeof(design) == "character") {
      design[design == "#"] <- NA
    }
    uniq <- unique(sort(design))
    design <- match(design, uniq)
    dim(design) <- dim

    if (trim) {
      non_empty <- !is.na(design)
      keep_row <- seq_range(which(apply(non_empty, 1, any)))
      keep_col <- seq_range(which(apply(non_empty, 2, any)))
      design <- design[keep_row, keep_col, drop = FALSE]
    }

    if (!is.numeric(uniq)) {
      attr(design, "design_names") <- uniq
    }
    return(design)
  } else {
    stop("The `design` argument should be interpretable as a matrix.")
  }
}




