# User function -----------------------------------------------------------

#' Axis guide with truncated line.
#'
#' This axis guide is similar to the normal axis guides for position scales, but
#' can shorten the axis line that is being drawn.
#'
#' @inheritParams ggplot2::guide_axis
#' @param trunc_lower,trunc_upper The lower and upper range of the truncated
#'   axis:
#' * `NULL` to not perform any truncation.
#' * A `function` that takes the break positions as input and returns the lower
#'   or upper boundary. Note that also for discrete scales, positions are the
#'   mapped positions as `numeric`.
#' * A `numeric` value in data units for the lower and upper boundaries.
#' * A `unit` object.
#'
#' @return An *axis_truncated* guide class object.
#' @export
#' @family axis-guides
#' @md
#'
#' @examples
#' # Make a plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme(axis.line = element_line(colour = "black"))
#'
#' # Setting the default truncated axis
#' p + guides(x = "axis_truncated")
#'
#' # Truncating in data units
#' p + guides(x = guide_axis_truncated(
#'   trunc_lower = 2.5, trunc_upper = 4.5
#' ))
#'
#' # Truncate by setting units
#' p + guides(x = guide_axis_truncated(
#'   trunc_lower = unit(0.1, "npc"),
#'   trunc_upper = unit(0.9, "npc")
#' ))
#'
#' # Truncating with functions
#' p + guides(x = guide_axis_truncated(
#'   trunc_lower = function(x) {x - 0.2},
#'   trunc_upper = function(x) {x + 0.2}
#' ))
guide_axis_truncated <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  trunc_lower = min,
  trunc_upper = max,
  position = waiver()
) {
  check_trunc_arg(trunc_lower, trunc_upper)
  structure(
    list(
      title = title,
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,
      order = order,
      trunc_lower = trunc_lower,
      trunc_upper = trunc_upper,
      position = position,
      available_aes = c("x", "y"),
      name = "axis"
    ),
    class = c("guide", "axis_truncated", "axis")
  )
}

# Internals ---------------------------------------------------------------

#' @export
#' @method guide_train axis_truncated
guide_train.axis_truncated <- function(guide, scale, aesthetic = NULL) {
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  guide <- NextMethod()
  guide <- truncate_guide(guide, scale, aesthetic)
  guide
}

#' @export
#' @method guide_transform axis_truncated
guide_transform.axis_truncated <- function(guide, coord, panel_params) {
  guide <- NextMethod()
  guide$trunc <- transform_truncated(guide$trunc, coord, panel_params)
  return(guide)
}

#' @export
#' @method guide_gengrob axis_truncated
guide_gengrob.axis_truncated <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis_truncated(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge,
    trunc = guide$trunc
  )
}

# Helpers -----------------------------------------------------------------

draw_axis_truncated <- function(
  break_positions,
  break_labels,
  axis_position,
  theme = theme,
  check.overlap,
  angle = NULL,
  n.dodge = 1,
  trunc
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  aes <- if (axis_position %in% c("t", "b")) "x" else "y"

  elements <- build_axis_elements(axis_position, angle, theme)
  params <- setup_axis_params(axis_position)
  line_grob <- build_trunc_axis_line(elements$line, params, trunc)

  if ({n_breaks <- length(break_positions)} == 0) {
    out <- gTree(
      children = gList(line_grob),
      width    = grobWidth(line_grob),
      height   = grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels(
    elements,
    labels = break_labels,
    position = break_positions,
    dodge = n.dodge, check.overlap = check.overlap, params = params
  )

  # Setup ticks
  sizes <- unit.c(elements$tick_length)
  tick_grob <- build_axis_ticks(elements$ticks, sizes,
                                break_positions, params)
  elements$tick_length <- max(sizes)
  assemble_axis_grobs(
    ticks = tick_grob, labels = label_grobs,
    lines = line_grob, elements = elements,
    params = params
  )
}

build_trunc_axis_line <- function(element, params, trunc) {
  pos <- unit.c(trunc[[1]], trunc[[2]])
  pos[c(TRUE, FALSE)] <- trunc[[1]]
  pos[c(FALSE, TRUE)] <- trunc[[2]]
  args <- list(element, pos,
               rep(params$pos, nrow(trunc) * 2),
               rep(2, nrow(trunc)))
  names(args) <- c("element", params$aes, params$non_aes, "id.lengths")
  do.call(element_grob, args)
}

transform_truncated <- function(trunc, coord, panel_params) {
  are_units <- c(is.unit(trunc[[1]]),
                 is.unit(trunc[[2]]))
  if (any(!are_units)) {
    trunc[, !are_units] <- coord$transform(
      trunc[, !are_units, drop = FALSE], panel_params
    )
    trunc[!are_units] <- lapply(
      trunc[, !are_units, drop = FALSE], unit, units = "npc"
    )
  }
  return(trunc)
}

truncate_guide <- function(guide, scale, aesthetic) {
  trunc_lower <- axis_truncate(
    guide$key[[aesthetic]], guide$trunc_lower, scale, "lower"
  )
  trunc_upper <- axis_truncate(
    guide$key[[aesthetic]], guide$trunc_upper, scale, "upper"
  )
  if (!(length(guide$trunc_lower) == length(guide$trunc_upper))) {
    abort(paste0("Axis truncation must have an equal number of upper and lower",
                 " truncation points."))
  }
  trunc <- .int$new_data_frame(list(
    "start" = trunc_lower,
    "end"   = trunc_upper
  ))
  trunc <- trunc[!is.na(trunc$start),]
  trunc <- trunc[!is.na(trunc$end),]
  trunc <- setNames(trunc, paste0(aesthetic, c("", "end")))
  guide$trunc <- trunc
  guide$trunc_lower <- NULL
  guide$trunc_upper <- NULL
  return(guide)
}

axis_truncate <- function(breaks, trunc, scale, type = "lower") {
  if (is.null(trunc)) {
    x <- unit(switch(
      type,
      "lower" = 0,
      "upper" = 1,
    ), "npc")
    return(x)
  } else if (is.unit(trunc)) {
    return(trunc)
  } else if (is.function(trunc)) {
    if (scale$is_discrete()) {
      x <- trunc(breaks)
    } else {
      # Function is expected to work on untransformed data
      x <- scale$scale$trans$transform(trunc(scale$scale$trans$inverse(breaks)))
    }
    return(x)
  } else { # Expecting input in dataspace here
    x <- scale$scale$trans$transform(trunc)
    return(x)
  }
}

check_trunc_arg <- function(lower, upper) {
  if (!is.function(lower) && !is.function(upper)) {
    lens <- c(length(lower), length(upper))
    if (lens[1] != lens[2] & !any(lens == 0)) {
      abort(paste0("Axis truncation must have an equal number of upper and ",
                   "lower truncation points."))
    }
  }
}
