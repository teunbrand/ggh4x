# User function -----------------------------------------------------------

# nocov start

#' Nested axis guide
#'
#' `r lifecycle::badge("deprecated")`
#' Discrete position scales containing interacting factors can be visualised
#' more clearly with a nested axis guide. Nested axis guides separate labels
#' based on a delimiter and groups identical later labels, indicating the
#' grouping with a line spanning the earlier labels.
#' The function is deprecated due to superior alternatives such as
#' `legendry::guide_axis_nested()`.
#'
#' @inheritParams guide_axis_truncated
#' @param delim A `character` of length 1 to tell `strsplit` how
#'   hierarchies should be broken up. Internally defaults to `"."` to match
#'   `interaction`'s default delimiter.
#' @param extend A `numeric` of length 1 indicating how much to extend
#'   nesting lines relative to the smallest difference in break positions.
#' @param inv A `logical(1)` which if `TRUE`, flips the grouping order. If
#'   `FALSE` (default), the grouping order is as-is.
#'
#' @details The guide itself makes no effort to group and order labels. To get
#'   nice groupings, consider re-ordering the levels of factor variables, or try
#'   setting the 'breaks' argument of a scale appropriately.
#'
#' @section Theme elements:
#'   This axis guide uses the following the theme elements:
#'
#'   \describe{
#'     \item{[`ggh4x.axis.nestline.x/y`][theme_extensions]}{
#'       An [`element_line()`][ggplot2::element_line] object to alter the
#'       display of the line separating different layers of labels.
#'     }
#'     \item{[`ggh4x.axis.nesttext.x/y`][theme_extensions]}{
#'       An [`element_text()`][ggplot2::element_text] object to differentiate
#'       text higher up in the hierarchy, from the text closest to the axis line.
#'     }
#'   }
#'
#' @return A *axis_nested* guide class object.
#' @seealso [ggplot2::guide_axis()] for the classic axis
#'   documentation. \cr [ggh4x::weave_factors()] for an alternative to
#'   `interaction()`.
#'
#' @export
#' @keywords internal
#'
#' @family axis-guides
#'
#' @examples
#' # The defaults are suited for interaction variables
#' ggplot(mpg, aes(interaction(cyl, class), hwy)) +
#'   geom_boxplot() +
#'   scale_x_discrete(guide = "axis_nested")
#'
#' # Control where labels are cut with the delim argument
#' ggplot(mpg, aes(interaction(cyl, class, sep = "~!~"), hwy)) +
#'   geom_boxplot() +
#'   scale_x_discrete(guide = guide_axis_nested(delim = "!"))
#'
#' # The nesting lines inherit looks from axis ticks
#' ggplot(mpg, aes(interaction(cyl, class), hwy)) +
#'   geom_boxplot() +
#'   scale_x_discrete(guide = "axis_nested") +
#'   theme(axis.ticks = element_line(colour = "red"))
#'
#' # The looks can be controlled independently by setting `ggh4x.axis.nestline`
#' ggplot(mpg, aes(interaction(cyl, class), hwy)) +
#'   geom_boxplot() +
#'   scale_x_discrete(guide = "axis_nested") +
#'   theme(ggh4x.axis.nestline = element_line(linetype = 2))
guide_axis_nested <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver(),
  delim = waiver(),
  inv   = FALSE,
  trunc_lower = NULL,
  trunc_upper = NULL,
  colour = NULL,
  color = NULL,
  extend = 0.5
) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "guide_axis_nested()",
    "legendry::guide_axis_nested()"
  )
  colour <- color %||% colour
  check_trunc_arg(trunc_lower, trunc_upper)
  structure(
    list(
      title = title,
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,
      order = order,
      position = position,
      available_aes = c("x", "y"),
      delim = delim,
      trunc_lower = trunc_lower,
      trunc_upper = trunc_upper,
      colour = colour,
      extend = extend,
      inv = inv,
      name = "axis"
    ),
    class = c("guide", "axis_nested", "axis_ggh4x", "axis")
  )
}

# Internals ---------------------------------------------------------------

#' @noRd
#' @export
guide_train.axis_nested <- function(
  guide, scale, aesthetic = NULL
) {
  if (inherits(guide$delim, "waiver")) {
    guide$delim <- "."
  }
  NextMethod()
}

#' @export
#' @noRd
guide_gengrob.axis_nested <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_nested_axis(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge,
    delim = guide$delim,
    extend = guide$extend,
    trunc = guide$trunc,
    colour = guide$colour,
    inv = guide$inv
  )
}

draw_nested_axis <- function(
  break_positions,
  break_labels,
  axis_position,
  theme,
  check.overlap,
  angle = NULL,
  n.dodge = 1,
  delim = ".",
  extend = 0.5,
  trunc,
  colour,
  inv = FALSE
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  aes <- if (axis_position %in% c("t", "b")) "x" else "y"

  # Calculate elements
  elements <- build_axis_elements(axis_position, angle, theme, colour = colour)
  elements$nest_line <- calc_element(paste0("ggh4x.axis.nestline.", aes), theme)
  elements$nest_text <- calc_element(paste0("ggh4x.axis.nesttext.", aes), theme)
  if (!is.null(colour)) {
    elements$nest_line$colour <- colour
    elements$nest_text$colour <- colour
  }

  params <- setup_axis_params(axis_position)
  fun <- params$labels_measure
  newfun <- function(x) {
    if (inherits(x, "polyline")) {
      unit(x$gp$lwd, "points")
    } else {
      fun(x)
    }
  }
  params$labels_measure <- newfun

  line_grob <- build_trunc_axis_line(elements$line, params, trunc)

  if ({n_breaks <- length(break_positions)} == 0) {
    out <- grid::gTree(
      children = grid::gList(line_grob),
      width = grid::grobWidth(line_grob),
      height = grid::grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels_nested(
    elements, labels = break_labels, position = break_positions,
    dodge = n.dodge, check.overlap = check.overlap, params = params,
    extend = extend, delim = delim, inv = inv
  )

  # Setup ticks
  tick_grob <- build_axis_ticks(elements$ticks, elements$tick_length,
                                break_positions, params)

  assemble_axis_grobs(ticks = tick_grob, labels = label_grobs,
                      lines = line_grob, elements = elements,
                      params = params)
}

# Helpers -----------------------------------------------------------------

build_axis_labels_nested <- function(elements, labels, position, dodge = 1,
                                     check.overlap = FALSE, params,
                                     extend = 0.5, delim = ".", inv = FALSE) {
  # Validate labels
  if (is.list(labels)) {
    if (any(vapply(labels, is.language, logical(1)))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }
  n_breaks <- length(position)
  if (length(n_breaks) == 0) {
    return(list(zeroGrob()))
  }

  dodge_pos <- rep(seq_len(dodge), length.out = n_breaks)
  dodge_idxs <- split(seq_len(n_breaks), dodge_pos)

  splitlabels <- strsplit(labels, delim, fixed = TRUE)
  lens <- lengths(splitlabels)
  if (!all(lens == max(lens))) {
    maxlen <- max(lens)
    splitlabels <- lapply(splitlabels, function(lab) {
      c(lab, rep("", maxlen - length(lab)))
    })
  }
  splitlabels <- do.call(rbind, splitlabels)
  if (isTRUE(inv)) {
    splitlabels <- splitlabels[, rev(seq_len(ncol(splitlabels))), drop = FALSE]
  }

  first_labels <- lapply(dodge_idxs, function(idx) {
    draw_axis_labels(
      break_positions = position[idx],
      break_labels    = splitlabels[,1][idx],
      label_element   = elements$label,
      is_vertical     = params$vertical,
      check.overlap   = check.overlap
    )
  })

  if (ncol(splitlabels) == 1) {
    return(first_labels)
  }

  later_labels <- lapply(tail(seq_ncol(splitlabels), -1), function(i) {
    labs <- splitlabels[,i]
    id <- rle(labs)
    ends <- cumsum(id$lengths)
    starts <- ends - id$lengths + 1
    mid <- (position[starts] + position[ends])/2
    newlabs <- draw_axis_labels(
      break_positions = mid,
      break_labels    = id$values,
      label_element   = elements$nest_text,
      is_vertical     = params$vertical,
      check.overlap   = check.overlap
    )
    nz <- nzchar(id$values)
    if (length(position) > 1) {
      xtend <- min(diff(position)) * 0.5 * extend
    } else {
      xtend <- 0.5 * extend
    }
    pos <- rbind(position[starts[nz]] - xtend, position[ends[nz]] + xtend)
    pos_len <- length(pos)
    if (params$vertical) {
      divider <- element_grob(elements$nest_line,
                              x = rep(0.5, pos_len),
                              y = pmin.int(c(pos), 1),
                              id.lengths = rep(2, pos_len/2))
    } else {
      divider <- element_grob(elements$nest_line,
                              x = pmin.int(c(pos), 1),
                              y = rep(0.5, pos_len),
                              id.lengths = rep(2, pos_len/2))
    }
    list(divider, newlabs)
  })
  later_labels <- unlist(later_labels, recursive = FALSE)

  c(first_labels, later_labels)
}

# nocov end
