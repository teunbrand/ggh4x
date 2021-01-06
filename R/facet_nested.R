# Main function -----------------------------------------------------------

#' @title Layout panels in a grid with nested strips
#'
#' @description \code{facet_nested()} forms a matrix of panels defined by row
#'   and column faceting variables and nests grouped facets.
#'
#' @inheritParams ggplot2::facet_grid
#' @param nest_line a \code{logical} vector of length 1, indicating whether to
#'   draw a nesting line to indicate the nesting of variables. Control the look
#'   of the nesting line by setting the \code{ggh4x.facet.nestline} theme
#'   element.
#' @param resect  a \code{unit} vector of length 1, indicating how much the
#'   nesting line should be shortened.
#' @param bleed a \code{logical} vector of length 1, indicating whether merging
#'   of lower-level variables is allowed when the higher-level variables are
#'   separate. See details.
#'
#' @details Unlike \code{facet_grid()}, this function only automatically expands
#'   missing variables when they have no variables in that direction, to allow
#'   for unnested variables. It still requires at least one layer to have all
#'   faceting variables.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   \code{rows} or \code{cols}. The first variable is interpreted to be the
#'   outermost variable, while the last variable is interpreted to be the
#'   innermost variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. Strips are
#'   automatically grouped when they span a nested variable.
#'
#'   The \code{bleed} argument controls whether lower-level variables are allowed
#'   to be merged when higher-level are different, i.e. they can bleed over
#'   hierarchies. Suppose the \code{facet_grid()} behaviour would be the
#'   following:
#'
#'   \code{[_1_][_2_][_2_]} \cr \code{[_3_][_3_][_4_]}
#'
#'   In such case, the default \code{bleed = FALSE} argument would result in the
#'   following:
#'
#'   \code{[_1_][___2____]} \cr \code{[_3_][_3_][_4_]}
#'
#'   Whereas \code{bleed = TRUE} would allow the following:
#'
#'   \code{[_1_][___2____]} \cr \code{[___3____][_4_]}
#'
#' @export
#'
#' @return A \emph{FacetNested} ggproto object.
#' @family facetting functions
#'
#' @seealso See \code{\link[ggplot2]{facet_grid}} for descriptions of the
#'   original arguments. See \code{\link[grid]{unit}} for the construction of a
#'   \code{unit} vector.
#'
#' @examples
#' df <- iris
#' df$nester <- ifelse(df$Species == "setosa",
#'                     "Short Leaves",
#'                     "Long Leaves")
#'
#' ggplot(df, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   facet_nested(~ nester + Species)
#'
#' # Controlling the nest line
#' ggplot(df, aes(Sepal.Length, Petal.Length)) +
#'   geom_point() +
#'   facet_nested(~ nester + Species, nest_line = TRUE) +
#'   theme(ggh4x.facet.nestline = element_line(linetype = 3))
facet_nested <- function(
  rows = NULL, cols = NULL, scales = "fixed", space = "fixed",
  shrink = TRUE, labeller = "label_value", as.table = TRUE,
  switch = NULL, drop = TRUE, margins = FALSE, facets = NULL,
  nest_line = FALSE, resect = unit(0, "mm"), bleed = FALSE
) {
  if (!is.null(facets)) {
    rows <- facets
  }
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))

  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(x = any(space %in% c("free_x", "free")),
                     y = any(space %in% c("free_y", "free")))

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }

  facets_list <- .int$grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions",
         .call = FALSE)
  }
  if (n == 1L) {
    rows <- quos()
    cols <- facets_list[[1]]
  } else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }
  labeller <- .int$check_labeller(labeller)
  ggproto(NULL, FacetNested, shrink = shrink,
          params = list(
            rows = rows,
            cols = cols,
            margins = margins,
            free = free,
            space_free = space_free,
            labeller = labeller,
            as.table = as.table,
            switch = switch,
            drop = drop,
            nest_line = nest_line,
            resect = resect,
            bleed = bleed
          ))
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetNested <- ggproto(
  "FacetNested", FacetGrid,
  map_data = function(data, layout, params) {
    # Handle empty data
    if (.int$empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    # Setup variables
    rows <- params$rows
    cols <- params$cols
    vars <- c(names(rows), names(cols))

    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }

    margin_vars <- list(intersect(names(rows), names(data)),
                        intersect(names(cols), names(data)))

    # Add variables
    data <- .int$reshape_add_margins(data, margin_vars, params$margins)
    facet_vals <- .int$eval_facets(c(rows, cols), data, params$.possible_columns)

    # Only set as missing if it has no variable in that direction
    missing_facets <- character(0)
    if (!any(names(rows) %in% names(facet_vals))){
      missing_facets <- c(missing_facets,
                          setdiff(names(rows), names(facet_vals)))
    }
    if (!any(names(cols) %in% names(facet_vals))){
      missing_facets <- c(missing_facets,
                          setdiff(names(cols), names(facet_vals)))
    }

    # Fill in missing values
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      data <- data[data_rep, , drop = FALSE]
      rownames(data) <- NULL
      facet_vals <- cbind(facet_vals[data_rep, , drop = FALSE],
                          to_add[facet_rep, , drop = FALSE])
      rownames(facet_vals) <- NULL
    }

    # Match columns to facets
    if (nrow(facet_vals) == 0) {
      data$PANEL <- -1
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      keys <- .int$join_keys(facet_vals, layout,
                             by = vars[vars %in% names(facet_vals)])
      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  compute_layout = function(data, params) {
    rows <- params$rows
    cols <- params$cols
    dups <- intersect(names(rows), names(cols))

    if (length(dups) > 0) {
      stop("Facetting variables can only appear in row or cols, not both.\n",
           "Problems: ", paste0(dups, collapse = "'"), call. = FALSE)
    }

    base_rows <- combine_nested_vars(data, params$plot_env,
                                     rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(.int$ulevels(x)))
    }
    base_cols <- combine_nested_vars(data, params$plot_env, cols,
                                     drop = params$drop)
    base <- .int$df.grid(base_rows, base_cols)

    if (nrow(base) == 0) {
      return(.int$new_data_frame(list(PANEL = 1L, ROW = 1L, COL = 1L,
                                      SCALE_X = 1L, SCALE_Y = 1L)))
    }

    base <- .int$reshape_add_margins(
      base, list(names(rows), names(cols)), params$margins
    )
    base <- unique(base)

    panel <- .int$id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))

    rows <- if (!length(names(rows))) {
      rep(1L, length(panel))
    } else {
      .int$id(base[names(rows)], drop = TRUE)
    }
    cols <- if (!length(names(cols))) {
      rep(1L, length(panel))
    } else {
      .int$id(base[names(cols)], drop = TRUE)
    }

    panels <- .int$new_data_frame(
      c(list(PANEL = panel, ROW = rows, COL = cols), base)
    )
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL
    panels$SCALE_X <- if (params$free$x) {
      panels$COL
    } else {
      1L
    }
    panels$SCALE_Y <- if (params$free$y) {
      panels$ROW
    } else {
      1L
    }
    panels
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params) {
    panel_table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales,
                                         ranges, coord, data, theme, params)

    # Setup strips
    col_vars  <- unique(layout[names(params$cols)])
    row_vars  <- unique(layout[names(params$rows)])
    attr(col_vars, "type")  <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type")  <- "rows"
    attr(row_vars, "facet") <- "grid"

    # Build strips
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")

    # Merging strips
    merge_cols <- apply(col_vars, 2, function(x) any(rle(x)$lengths > 1))
    merge_rows <- apply(row_vars, 2, function(x) any(rle(x)$lengths > 1))

    if (any(merge_cols)) {
      if (switch_x) {
        panel_table <- merge_strips(panel_table,
                                    col_vars, switch_x, params, theme, "b")
      } else {
        panel_table <- merge_strips(panel_table,
                                    col_vars, switch_x, params, theme, "t")
      }
    }

    if (any(merge_rows)) {
      if (switch_y) {
        panel_table <- merge_strips(panel_table,
                                    row_vars, switch_y, params, theme, "l")
      } else {
        panel_table <- merge_strips(panel_table,
                                    row_vars, switch_y, params, theme, "r")
      }
    }
    panel_table
  }
)

# Helper functions -----------------------------------------------

combine_nested_vars <- function(
  data, env = emptyenv(), vars = NULL, drop = TRUE
) {
  if (length(vars) == 0) {
    return(.int$new_data_frame())
  }

  possible_columns <- unique(unlist(lapply(data, names)))

  values <- .int$compact(lapply(data, .int$eval_facets, facets = vars,
                                possible_columns = possible_columns))
  has_all <- unlist(lapply(values, length)) == length(vars)
  if (!any(has_all)) {
    missing <- lapply(values, function(x) setdiff(names(vars), names(x)))
    missing_txt <- vapply(missing, .int$var_list, character(1))
    name <- c("Plot", paste0("Layer ", seq_len(length(data) - 1)))
    stop("At least one layer must contain all faceting variables: ",
         .int$var_list(names(vars)), ".\n", paste0("* ", name, " is missing ",
                                              missing_txt, collapse = "\n"),
         call. = FALSE)
  }
  base <- unique(.int$rbind_dfs(values[has_all]))
  if (!drop) {
    base <- .int$unique_combs(base)
  }
  for (value in values[!has_all]) {
    if (.int$empty(value))
      next
    old <- base[setdiff(names(base), names(value))]
    new <- unique(value[intersect(names(base), names(value))])
    if (drop) {
      new <- .int$unique_combs(new)
    }
    old[setdiff(names(base), names(value))] <- rep("", nrow(old))
    base <- rbind(base, .int$df.grid(old, new))
  }
  if (.int$empty(base)) {
    stop("Facetting variables must have at least one value",
         call. = FALSE)
  }
  base
}

# New merge strips --------------------------------------------------------

merge_strips <- function(
  panel_table, vars, switch, params, theme, where = "t"
) {
  orient <- if (where %in% c("t", "b")) "x" else "y"
  nlevels <- ncol(vars)

  these_strips <- grep(paste0("strip-", where), panel_table$layout$name)
  strp_rows <- range(panel_table$layout$t[these_strips])
  strp_cols <- range(panel_table$layout$l[these_strips])
  strp_rows <- seq(strp_rows[1], strp_rows[2])
  strp_cols <- seq(strp_cols[1], strp_cols[2])
  strp <- panel_table[strp_rows, strp_cols]

  # Make empty template
  template <- strp
  template$grobs <- list()
  template$layout <- template$layout[0,]

  # Inflate strips
  for (i in seq_along(strp$grobs)) {
    sub <- strp$grobs[[i]]
    if (where == "b") {
      sub$layout$t <- rev(sub$layout$t)
      sub$layout$b <- rev(sub$layout$b)
    }
    n <- length(sub$grobs)
    lay <- strp$layout[i,]
    lay <- lay[rep(1, n),]
    rownames(lay) <- NULL
    sub <- lapply(seq_len(n), function(j) {
      x <- sub
      x$grobs <- x$grobs[j]
      x$layout <- x$layout[j,]
      x
    })
    template <- gtable_add_grob(
      template,
      sub, t = lay$t, l = lay$l, b = lay$b, r = lay$r,
      z = lay$z, clip = lay$clip, name = paste0(lay$name, "-", seq_len(n))
    )
  }

  if (!params$bleed) {
    vars[] <- lapply(seq_len(ncol(vars)), function(i) {
      do.call(paste0, vars[, seq(i), drop = FALSE])
    })
  }
  merge <- apply(vars, 2, function(x) any(rle(x)$lengths > 1))

  if (where == "r") {
    vars <- rev(vars)
    merge <- rev(merge)
  }

  # Abstract away strips
  strip_ids <- strsplit(template$layout$name, "-", fixed = TRUE)
  strip_ids <- do.call(rbind, strip_ids)
  strip_ids <- strip_ids[,3:ncol(strip_ids)]
  mode(strip_ids) <- "integer"

  template$layout$delete <- rep(FALSE, nrow(strip_ids))
  template$layout$aquire <- seq_along(template$grobs)

  for (i in seq_len(nlevels)) {
    if (!merge[i]) {
      next()
    }
    ii <- strip_ids[, 2] == i

    # Figure out what to merge
    j <- as.numeric(as.factor(vars[, i]))

    ends <- cumsum(rle(j)$lengths)
    starts <- c(1, which(diff(j) != 0) + 1)

    # Figure out what strip to remove
    seqs <- unlist(Map(seq, from = starts, to = ends))
    delete_this <- seqs[!(seqs %in% starts)]
    delete_this <- which(strip_ids[, 1] %in% delete_this & ii)
    template$layout$delete[delete_this] <- TRUE

    # Figure out what cells to expand
    expand <- seqs[seqs %in% starts]
    expand <- which(strip_ids[, 1] %in% expand & ii)
    expand_where <- seqs[seqs %in% ends]
    expand_where <- which(strip_ids[, 1] %in% expand_where & ii)
    template$layout$aquire[expand] <- template$layout$aquire[expand_where]
  }

  # Do expansion
  if (orient == "x") {
    template$layout$r <- template$layout$r[template$layout$aquire]
  } else {
    template$layout$b <- template$layout$b[template$layout$aquire]
  }

  # Do deletion
  template$grobs  <- template$grobs[!template$layout$delete]
  strip_ids <- strip_ids[!template$layout$delete,]
  template$layout <- template$layout[!template$layout$delete,]

  # Add nesting indicator
  if (params$nest_line) {
    active <- unit(c(0, 1), "npc") + c(1, -1) * params$resect
    passive <- if (switch) c(1, 1) else c(0, 0)
    nindi <- element_render(
      theme, "ggh4x.facet.nestline",
      x = switch(orient, x = active,  y = passive),
      y = switch(orient, x = passive, y = active)
    )
    i <- which(with(template$layout, t != b | l != r))
    offset <- switch(
      orient,
      x = vapply(template$grobs, function(grob){grob$layout$t}, numeric(1)),
      y = vapply(template$grobs, function(grob){grob$layout$l}, numeric(1))
    )
    offset <- if (where %in% c("r", "b")) offset else nlevels - offset
    template$grobs[i] <- lapply(template$grobs[i], function(grb) {
      grb <- with(grb$layout, gtable_add_grob(
        grb, nindi, t = t, l = l, r = r, b = b,
        z = z,
        name = "nester",
        clip = "off"
      ))
    })
    template$layout$z <- template$layout$z + offset
  }

  # Delete old strips
  panel_table <- gtable_filter(panel_table, paste0("strip-", where),
                               fixed = TRUE, trim = FALSE, invert = TRUE)

  # Place back new strips
  panel_table <- with(template$layout, gtable_add_grob(
    panel_table,
    template$grobs,
    t = t - 1 + strp_rows[1],
    l = l - 1 + strp_cols[1],
    b = b - 1 + strp_rows[1],
    r = r - 1 + strp_cols[1],
    z = z, clip = clip, name = name
  ))
  panel_table
}
