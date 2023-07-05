# User function -----------------------------------------------------------

#' Axis guide with truncated line
#'
#' This axis guide is similar to the normal axis guides for position scales, but
#' can shorten the axis line that is being drawn. The `guide_axis_colour()`
#' function is the same but with different defaults for the truncation that do
#' not truncate the axis. Axis truncation and recolouring is supported
#' throughout axes in ggh4x.
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
#' @param colour,color A `character(1)` with a valid colour for colouring the
#'   axis text, axis ticks and axis line. Overrules the colour assigned by the
#'   theme.
#'
#' @return An *axis_ggh4x* guide class object.
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
#'
#' # Recolouring the axes outside the theme
#' p + guides(x = guide_axis_colour(colour = "red"),
#'            y = guide_axis_colour(colour = "blue"))
guide_axis_truncated <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  colour = NULL,
  color = NULL,
  trunc_lower = min,
  trunc_upper = max,
  position = waiver()
) {
  colour <- color %||% colour
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
      colour = colour,
      position = position,
      available_aes = c("x", "y"),
      name = "axis"
    ),
    class = c("guide", "axis_ggh4x", "axis")
  )
}

#' @rdname guide_axis_truncated
#' @export
guide_axis_colour <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  colour = NULL,
  color = NULL,
  trunc_lower = NULL,
  trunc_upper = NULL,
  position = waiver()
) {
  colour <- color %||% colour
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
      colour = colour,
      position = position,
      available_aes = c("x", "y"),
      name = "axis"
    ),
    class = c("guide", "axis_ggh4x", "axis")
  )
}

#' @rdname guide_axis_truncated
#' @export
guide_axis_color <- guide_axis_colour

# Internals ---------------------------------------------------------------

#' @export
#' @method guide_train axis_ggh4x
guide_train.axis_ggh4x <- function(guide, scale, aesthetic = NULL) {
  aesthetic <- aesthetic %||% scale$aesthetics[1]

  breaks <- scale$get_breaks()

  empty_ticks <- data_frame0(
    !!aesthetic := numeric(0),
    .value       = numeric(0),
    .label       = character(0)
  )

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    cli::cli_warn(c(
      "Axis guide lacks appropriate scales.",
      i = "Use one of {.or {.field {guide$available_aes}}}"
    ))
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    mapped_breaks <- if (scale$is_discrete()) scale$map(breaks) else breaks
    ticks <- data_frame0(!!aesthetic := mapped_breaks)
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)
    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
  }

  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- hash(list(guide$title, guide$key$.value,
                          guide$key$.label, guide$name))
  guide <- truncate_guide(guide, scale, aesthetic)
  guide
}

#' @export
#' @method guide_transform axis_ggh4x
guide_transform.axis_ggh4x <- function(guide, coord, panel_params) {
  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }

  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]

  dual <- all(c("x", "y") %in% aesthetics)
  if (!dual) {
    other_aes <- setdiff(c("x", "y"), aesthetics)
    override  <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aes]] <- override
  }
  guide$key <- coord$transform(guide$key, panel_params)
  if (!dual) {
    warn_for_guide_position(guide)
  }
  guide$trunc <- transform_truncated(guide$trunc, coord, panel_params)
  return(guide)
}

#' @export
#' @method guide_geom axis_ggh4x
guide_geom.axis_ggh4x <- function(guide, ...) {
  guide
}

#' @export
#' @method guide_gengrob axis_ggh4x
guide_gengrob.axis_ggh4x <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis_ggh4x(
    key = guide$key,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge,
    trunc = guide$trunc,
    colour = guide$colour
  )
}

# Helpers -----------------------------------------------------------------

draw_axis_ggh4x <- function(
  key,
  axis_position,
  theme = theme,
  check.overlap,
  angle = NULL,
  n.dodge = 1,
  trunc,
  colour = NULL
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  elements <- build_axis_elements(axis_position, angle, theme, colour)
  params <- setup_axis_params(axis_position)
  line_grob <- build_trunc_axis_line(elements$line, params, trunc)

  if (nrow(key) == 0) {
    out <- gTree(
      children = gList(line_grob),
      width    = grobWidth(line_grob),
      height   = grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels(
    elements, key = key,
    dodge = n.dodge, check.overlap = check.overlap, params = params
  )

  # Setup ticks
  sizes <- unit.c(elements$tick_length)
  tick_grob <- build_axis_ticks(elements$ticks, sizes,
                                key[[params$aes]], params)
  elements$tick_length <- max(sizes)
  assemble_axis_grobs(
    ticks = tick_grob, labels = label_grobs,
    lines = line_grob, elements = elements,
    params = params
  )
}

build_trunc_axis_line <- function(element, params, trunc) {
  if (inherits(element, "element_blank")) {
    return(zeroGrob())
  }
  if (is.null(trunc)) {
    trunc <- data_frame(x = unit(0, "npc"), xend = unit(1, "npc"))
  }
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
  if (is.null(trunc)) {
    return(NULL)
  }
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
  if (is.null(guide$trunc_lower) && is.null(guide$trunc_upper)) {
    return(guide)
  }
  trunc_lower <- axis_truncate(
    guide$key[[aesthetic]], guide$trunc_lower, scale, "lower"
  )
  trunc_upper <- axis_truncate(
    guide$key[[aesthetic]], guide$trunc_upper, scale, "upper"
  )
  if (!(length(guide$trunc_lower) == length(guide$trunc_upper))) {
    cli::cli_abort(paste0(
      "Axis truncation must have an equal number of upper and lower ",
      "truncation points."
    ))
  }
  trunc <- data_frame0(
    "start" = trunc_lower,
    "end"   = trunc_upper
  )
  trunc <- trunc[!is.na(trunc$start),]
  trunc <- trunc[!is.na(trunc$end),]
  trunc <- setNames(trunc, paste0(aesthetic, c("", "end")))
  guide$trunc <- trunc
  guide$trunc_lower <- NULL
  guide$trunc_upper <- NULL
  return(guide)
}

axis_truncate <- function(breaks, trunc, scale, type = "lower") {
  if (rlang::is_formula(trunc)) {
    trunc <- rlang::as_function(trunc)
  }
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
    if (scale$is_discrete()) {
      x <- scale$scale$map(trunc)
    } else {
      x <- scale$scale$trans$transform(trunc)
    }
    return(x)
  }
}

check_trunc_arg <- function(lower, upper) {
  if (!is.function(lower) && !is.function(upper)) {
    lens <- c(length(lower), length(upper))
    if (lens[1] != lens[2] & !any(lens == 0)) {
      cli::cli_abort(paste0(
        "Axis truncation must have an equal number of upper and lower ",
        "truncation points."
      ))
    }
  }
}

