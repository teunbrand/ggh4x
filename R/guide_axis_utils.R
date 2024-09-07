build_axis_elements <- function(
  axis_position = "b", angle = NULL, theme, colour = NULL
) {
  aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"
  axis_position <- match.arg(axis_position, c("top", "bottom", "left", "right"))

  element_names = c(line = "axis.line.", ticks = "axis.ticks.",
                    tick_length = "axis.ticks.length.", label = "axis.text.")
  element_names <- setNames(paste0(element_names, aesthetic, ".",
                                   axis_position),
                            names(element_names))
  elements <- lapply(element_names, calc_element, theme)

  if (inherits(elements$label, "element_text")) {
    lab_overrides <- axis_label_element_overrides(axis_position, angle)
    elements$label$angle <- lab_overrides$angle %||% elements$label$angle
    elements$label$hjust <- lab_overrides$hjust %||% elements$label$hjust
    elements$label$vjust <- lab_overrides$vjust %||% elements$label$vjust
  }
  if (!is.null(colour)) {
    elements <- lapply(elements, function(el) {
      if ("colour" %in% names(el)) {
        el$colour <- colour
      }
      return(el)
    })
  }

  elements
}

build_axis_line <- function(element, params) {
  args <- list(element, unit(c(0, 1), "npc"), rep(params$pos, 2L))
  names(args) <- c("element", params$aes, params$non_aes)
  do.call(element_grob, args)
}

build_axis_labels <- function(
  elements, key, dodge = 1, check.overlap = FALSE, params
) {
  if ({n_breaks <- nrow(key)} == 0) {
    return(list(zeroGrob()))
  }

  if (is.list(key$.label)) {
    if (any(vapply(key$.label, is.language, logical(1)))) {
      key$.label <- do.call(expression, key$.label)
    } else {
      key$.label <- unlist(key$.label)
    }
  }

  dodge_pos  <- rep(seq_len(dodge), length.out = n_breaks)
  dodge_idxs <- split(seq_len(n_breaks), dodge_pos)

  if (params$vertical) {
    pos_dim <- "y"
    margin_name <- "margin_x"
  } else {
    pos_dim <- "x"
    margin_name <- "margin_y"
  }

  lapply(dodge_idxs, function(idx) {
    subkey <- key[idx, , drop = FALSE]
    if (check.overlap) {
      priority <- axis_label_priority(nrow(subkey))
      subkey   <- subkey[priority, , drop = FALSE]
    }
    breaks <- subkey[[params$aes]]
    n <- length(breaks)
    labs <- subkey$.label

    args <- setNames(
      list(elements$label, labs, breaks, TRUE, check.overlap,
           # Following can all be NULL
           subkey$.family, subkey$.face, subkey$.colour, subkey$.size,
           subkey$.hjust, subkey$.vjust, subkey$.lineheight,
           params$margin),
      c("element", "label", pos_dim, margin_name, "check.overlap",
        "family", "face", "colour", "size", "hjust", "vjust", "lineheight",
        "margin")
    )
    do.call(element_grob, args)
  })
}

build_axis_ticks <- function(element, length, position, params) {
  n_breaks <- length(position)
  pos <- unit(c(params$pos, params$pos + (params$tick_dir * 1)), "npc")
  pos <- rep(pos[params$tick_ord], times = n_breaks)

  position <- rep(position, each = 2)
  if (!is.unit(position)) {
    position <- unit(position, "native")
  }

  args <- setNames(
    list(element, position, pos, rep(2, times = n_breaks)),
    c("element", params$aes, params$non_aes, "id.lengths")
  )

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
        width  = max(gtable::gtable_width(gt),  unit(0, "pt")),
        height = max(gtable::gtable_height(gt), unit(0, "pt")),
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
    non_position_panel <- 0
    tick_coord_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- 1
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
