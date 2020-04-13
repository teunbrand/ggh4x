# User function -----------------------------------------------------------

#' Nested axis guide
#'
#' Discrete position scales containing interacting factors can be visualised
#' more clearly with a nested axis guide. Nested axis guides seperate labels
#' based on a delimiter and groups identical later labels, indicating the
#' grouping with a line spanning the earlier labels.
#'
#' @inheritParams ggplot2::guide_axis
#' @param delim A \code{character} of length 1 to tell \code{strsplit} how
#'   hierarchies should be broken up. Internally defaults to \code{"."} to match
#'   \code{interaction}'s default delimiter.
#' @param extend A \code{numeric} of length 1 indicating how much to extend
#'   nesting lines relative to the smallest difference in break positions.
#'
#' @details The guide itself makes no effort to group and order labels. To get
#'   nice groupings, consider re-ordering the levels of factor variables, or
#'   try setting the 'breaks' argument of a scale appropriately.
#'
#' @return A \emph{axis_nested} guide class object.
#' @export
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
  extend = 0.5
) {
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
      extend = extend,
      name = "axis"
    ),
    class = c("guide", "axis_nested", "axis")
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
    extend = guide$extend
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
  extend = 0.5
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  aes <- if (axis_position %in% c("t", "b")) "x" else "y"

  # Calculate elements
  elements <- build_axis_elements(axis_position, angle, theme)
  elements$nest_line <- calc_element(paste0("ggh4x.axis.nestline.", aes), theme)
  elements$nest_text <- calc_element(paste0("ggh4x.axis.nesttext.", aes), theme)

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

  line_grob <- build_axis_line(elements$line, params)

  if ({n_breaks <- length(break_positions)} == 0) {
    out <- grid::gTree(
      children = grid::gList(line_grob),
      width = grid::grobWidth(line_grob),
      height = grid::grobHeight(line_grob),
      cl = "abosluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels_nested(
    elements, labels = break_labels, position = break_positions,
    dodge = n.dodge, check.overlap = check.overlap, params = params,
    extend = extend, delim = delim
  )

  # Setup ticks
  tick_grob <- build_axis_ticks(elements$ticks, elements$tick_length,
                                break_positions, params)

  assemble_axis_grobs(ticks = tick_grob, labels = label_grobs,
                      lines = line_grob, elements = elements,
                      params = params)
}

# Helpers -----------------------------------------------------------------

build_axis_elements <- function(axis_position = "b", angle = NULL, theme) {
  aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"
  axis_position <- match.arg(axis_position, c("top", "bottom", "left", "right"))

  element_names = c(line = "axis.line.", ticks = "axis.ticks.",
                    tick_length = "axis.ticks.length.", label = "axis.text.")
  element_names <- setNames(paste0(element_names, aesthetic, ".",
                                   axis_position),
                            names(element_names))
  elements <- lapply(element_names, calc_element, theme)

  if (inherits(elements$label, "element_text")) {
    lab_overrides <- .int$axis_label_element_overrides(axis_position, angle)
    elements$label$angle <- lab_overrides$angle %||% elements$label$angle
    elements$label$hjust <- lab_overrides$hjust %||% elements$label$hjust
    elements$label$vjust <- lab_overrides$vjust %||% elements$label$vjust
  }
  elements
}

build_axis_line <- function(element, params) {
  args <- list(element, unit(c(0, 1), "npc"), rep(params$pos, 2L))
  names(args) <- c("element", params$aes, params$non_aes)
  do.call(element_grob, args)
}

build_axis_labels <- function(
  elements, labels, position, dodge = 1, check.overlap = FALSE, params
) {
  # Validate labels
  if (is.list(labels)) {
    if (any(vapply(labels, is.language, logical(1)))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }
  n_breaks <- length(position)

  dodge_pos <- rep(seq_len(dodge), length.out = n_breaks)
  dodge_idxs <- split(seq_len(n_breaks), dodge_pos)
  label_grobs <- lapply(dodge_idxs, function(idx) {
    .int$draw_axis_labels(
      break_positions = position[idx],
      break_labels = labels[idx],
      label_element = elements$label,
      is_vertical = params$vertical,
      check.overlap = check.overlap
    )
  })
}

build_axis_labels_nested <- function(elements, labels, position, dodge = 1,
                                     check.overlap = FALSE, params,
                                     extend = 0.5, delim = ".") {
  # Validate labels
  if (is.list(labels)) {
    if (any(vapply(labels, is.language, logical(1)))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }
  n_breaks <- length(position)

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

  first_labels <- lapply(dodge_idxs, function(idx) {
    .int$draw_axis_labels(
      break_positions = position[idx],
      break_labels = splitlabels[,1][idx],
      label_element = elements$label,
      is_vertical = params$vertical,
      check.overlap = check.overlap
    )
  })

  if (ncol(splitlabels) == 1) {
    return(first_labels)
  }

  later_labels <- lapply(tail(seq_len(ncol(splitlabels)), -1), function(i) {
    labs <- splitlabels[,i]
    id <- rle(labs)
    ends <- cumsum(id$lengths)
    starts <- ends - id$lengths + 1
    mid <- (position[starts] + position[ends])/2
    newlabs <- .int$draw_axis_labels(
      break_positions = mid,
      break_labels = id$values,
      label_element = elements$nest_text,
      is_vertical = params$vertical,
      check.overlap = check.overlap
    )
    nz <- nzchar(id$values)
    xtend <- min(diff(position)) * 0.5 * extend
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

build_axis_ticks <- function(element, length, position, params) {
  n_breaks <- length(position)
  pos <- params$pos + (params$tick_dir * length)
  pos <- rep(grid::unit.c(pos, params$pos)[params$tick_ord], times = n_breaks)

  args <- list(element, unit(rep(position, each = 2), "native"),
               pos, rep(2, times = n_breaks))
  names(args) <- c("element", params$aes, params$non_aes, "id.lengths")

  do.call(element_grob, args)
}

assemble_axis_grobs <- function(ticks, labels, lines, elements, params) {
  non_dims <- paste0(params$non_dim, "s")
  label_dims <- do.call(unit.c, lapply(labels, params$labels_measure))
  grobs <- c(list(ticks), labels)
  grob_dims <- unit.c(elements$tick_length, label_dims)

  if (params$labels_first) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- base::do.call(
    params$gtable_element,
    setNames(list("axis", grobs, grob_dims, unit(1, "npc")),
             c("name", "grobs", non_dims, params$dim))
  )

  # create viewport
  justvp <- base::do.call(
    grid::viewport,
    setNames(list(params$pos, params$gtable_measure(gt),
                  params$opposite_axis),
             c(params$non_aes, params$non_dim, "just"))
  )

  gTree(children = gList(lines, gt),
        width = gtable::gtable_width(gt),
        height = gtable::gtable_height(gt),
        xmin = NULL, ymin = NULL, vp = justvp,
        cl = "absoluteGrob")
}

setup_axis_params <- function(axis_position) {
  aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"

  # Set verticality parameters
  if (is_vertical <- axis_position %in% c("l", "r")) {
    position_dim <- "y"
    position_size <- "height"
    gtable_element <- gtable::gtable_row
    gtable_measure <- gtable::gtable_width
    measure_labels <- grid::grobWidth
  } else {
    position_dim <- "x"
    position_size <- "width"
    gtable_element <- gtable::gtable_col
    gtable_measure <- gtable::gtable_height
    measure_labels <- grid::grobHeight
  }
  non_position_dim <- setdiff(c("x", "y"), position_dim)
  non_position_size <- setdiff(c("width", "height"), position_size)

  # Set secondarity parameters
  if (is_second <- axis_position %in% c("r", "t")) {
    tick_direction <- 1
    non_position_panel <- unit(0, "npc")
    tick_coord_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- unit(1, "npc")
    tick_coord_order <- c(1, 2)
  }

  labels_first <- axis_position %in% c("l", "t")
  axis_opposite <- chartr("tblr", "btrl", axis_position)
  axis_opposite <- match.arg(axis_opposite, c("top", "bottom", "left", "right"))

  list(aes = aesthetic, non_aes = non_position_dim,
       dim = position_size, non_dim = non_position_size,
       gtable_element = gtable_element,
       gtable_measure = gtable_measure,
       labels_measure = measure_labels,
       tick_dir = tick_direction,
       tick_ord = tick_coord_order,
       pos = non_position_panel,
       labels_first = labels_first,
       opposite_axis = axis_opposite,
       vertical = is_vertical)
}
