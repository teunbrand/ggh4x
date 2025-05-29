# These are functions copied straight from the ggplot2 package to avoid issues
# with depending directly on ggplot2 internals. The functions and objects below
# are copied due to the MIT licence that ggplot2 wields.


# nocov start
add_margins <- function(
  grob,
  height,
  width,
  margin   = NULL,
  gp       = gpar(),
  margin_x = FALSE,
  margin_y = FALSE
) {
  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }
  if (margin_x && margin_y) {
    widths  <- unit.c(margin[4], width,  margin[2])
    heights <- unit.c(margin[1], height, margin[3])
    vp <- viewport(
      layout = grid.layout(3, 3, heights = heights, widths = widths),
      gp = gp
    )
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths  <- unit.c(margin[4], width, margin[2])
    heights <- unit(1, "null")
    vp <- viewport(
      layout = grid.layout(1, 3, widths = widths),
      gp = gp
    )
    child_vp <- viewport(layout.pos.col = 2)
  } else if (margin_y) {
    widths  <- unit(1, "null")
    heights <- unit.c(margin[1], height, margin[3])
    vp <- viewport(
      layout = grid.layout(3, 1, heights = heights),
      gp = gp
    )
    child_vp <- viewport(layout.pos.row = 2)
  } else {
    widths  <- width
    heights <- height
    return(
      gTree(
        children = grob, widths = widths, heights = heights, cl = "titleGrob"
      )
    )
  }
  gTree(
    children = grob,
    vp = vpTree(vp, vpList(child_vp)),
    widths = widths, heights = heights,
    cl = "titleGrob"
  )
}

axis_label_element_overrides <- function(axis_position, angle = NULL) {
  if (is.null(angle)) {
    return(
      element_text(angle = NULL, hjust = NULL, vjust = NULL)
    )
  }
  if (angle > 90 || angle < -90) {
    cli::cli_abort("{.arg angle} must be between 90 and -90")
  }
  if (axis_position == "bottom") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 1
    )
  } else if (axis_position == "left") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 1,
      vjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5
    )
  } else if (axis_position == "top") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 0
    )
  } else if (axis_position == "right") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 0,
      vjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5
    )
  } else {
    cli::cli_abort(c(
      "Unrecognised {.arg axis_position}: {.val {axis_position}}.",
      i = "Use one of {.val top}, {.val bottom}, {.val left} or {.val right}"
    ))
  }
}

check_labeller <- function(labeller) {
  labeller <- match.fun(labeller)
  is_deprecated <- all(c("varaible", "value") %in% names(formals(labeller)))
  if (is_deprecated) {
    old_labeller <- labeller
    labeller <- function(labels) {
      Map(old_labeller, names(labels), labels)
    }
    cli::cli_warn(c(
      "The {.arg labeller} API has been updated. Labellers taking \\
      {.arg variable} and {.arg value} arguments are now deprecated.",
      i = "See labellers documentation."
    ))
  }
  labeller
}

unrowname <- function(x) {
  if (is.data.frame(x)) {
    attr(x, "row.names") <- .set_row_names(.row_names_info(x, 2L))
  } else if (is.matrix(x)) {
    dimnames(x)[1] <- list(NULL)
  } else {
    cli::cli_abort(
      "Can only remove rownames from {.cls data.frame} and \\
      {.cls matrix} objects."
    )
  }
  x
}

df.grid = function(a, b) {
  if (is.null(a) || nrow(a) == 0) {
    return(b)
  }
  if (is.null(b) || nrow(b) == 0) {
    return(a)
  }
  indexes <- expand.grid(i_a = seq_len(nrow(a)), i_b = seq_len(nrow(b)))
  vec_cbind(
    unrowname(a[indexes$i_a, , drop = FALSE]),
    unrowname(b[indexes$i_b, , drop = FALSE])
  )
}

draw_axis_labels = function(
  break_positions,
  break_labels,
  label_element,
  is_vertical,
  check.overlap = FALSE
) {
  position_dim <- if (is_vertical) "y" else "x"
  label_margin_name <- if (is_vertical) "margin_x" else "margin_y"
  n_breaks <- length(break_positions)
  break_positions <- unit(break_positions, "native")
  if (check.overlap) {
    priority <- axis_label_priority(n_breaks)
    break_labels    <- break_labels[priority]
    break_positions <- break_positions[priority]
  }
  labels_grob <- rlang::exec(
    element_grob,
    label_element,
    `:=`(!!position_dim, break_positions),
    `:=`(!!label_margin_name, TRUE),
    label = break_labels,
    check.overlap = check.overlap
  )
}

axis_label_priority <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }
  c(1, n, axis_label_priority_between(1, n))
}

axis_label_priority_between <- function(x, y) {
  n <- y - x + 1
  if (n <= 2) {
    return(numeric(0))
  }
  mid <- x - 1 + (n + 1) %/% 2
  c(mid,
    axis_label_priority_between(x, mid),
    axis_label_priority_between(mid, y))
}

reshape_add_margins = function(df, vars, margins = TRUE, margin_nm = "(all)") {
  margin_vars <- reshape_margins(vars, margins)
  if (length(margin_vars) == 0) {
    return(df)
  }
  add_all <- function(x) {
    x <- addNA(x, TRUE)
    factor(x, levels = c(levels(x), margin_nm), exclude = NULL)
  }
  vars <- unique0(unlist(margin_vars))
  df[vars] <- lapply(df[vars], add_all)
  rownames(df) <- NULL
  margin_dfs <- lapply(
    margin_vars,
    function(vars) {
      df[vars] <- rep(list(factor(margin_nm)), length(vars))
      df
    }
  )
  vec_rbind(!!!margin_dfs)
}

reshape_margins = function(vars, margins = NULL) {
  if (is.null(margins) || identical(margins, FALSE)) {
    return(NULL)
  }
  all_vars <- unlist(vars)
  if (isTRUE(margins)) {
    margins <- all_vars
  }
  dims <- lapply(vars, intersect, margins)
  dims <- Map(function(vars, margin) {
    lapply(margin, downto, vars)
  }, vars, dims, USE.NAMES = FALSE)
  seq_0 <- function(x) c(0, seq_along(x))
  indices <- expand.grid(lapply(dims, seq_0), KEEP.OUT.ATTRS = FALSE)
  lapply(seq_nrow(indices), function(i) {
    unlist(Map("[", dims, indices[i, ]))
  })
}

downto <- function(a, b) rev(upto(a, rev(b)))
upto   <- function(a, b) b[seq_len(match(a, b, nomatch = 0))]

# defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  .variables <- .variables[lengths(.variables) > 0]
  if (length(.variables) == 0) {
    n  <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1),
                      USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- rlang::inject(paste(!!!ids, sep = "'\r"))
    res <- match(char_id, unique0(char_id))
  } else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat   <- rlang::inject(cbind(!!!ids))
    res   <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  } else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}

id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n  <- 0L
  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x  <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n  <- length(levels(x))
  } else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n  <- max(id)
  }
  attr(id, "n") <- n
  id
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

eval_facets <- function(facets, data, possible_columns = NULL) {
  vars <- lapply(facets, eval_facet, data, possible_columns = possible_columns)
  vars <- vars[lengths(vars) > 0]
  data_frame(!!!vars)
}

eval_facet <- function(facet, data, possible_columns = NULL) {
  if (rlang::quo_is_symbol(facet)) {
    facet <- as.character(rlang::quo_get_expr(facet))
    if (facet %in% names(data)) {
      out <- data[[facet]]
    } else {
      out <- NULL
    }
    return(out)
  }

  env <- rlang::new_environment(data)
  missing_columns <- setdiff(possible_columns, names(data))
  undefined_error <- function(e) {
    cli::cli_abort("", class = "ggplot2_missing_facet_var")
  }
  bindings <- rlang::rep_named(missing_columns, list(undefined_error))
  rlang::env_bind_active(env, !!!bindings)
  mask <- rlang::new_data_mask(env)
  mask$.data <- rlang::as_data_pronoun(mask)
  tryCatch(
    eval_tidy(facet, mask),
    ggplot2_missing_facet_var = function(e) NULL
  )
}

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

is.zero <- function(x) {
  is.null(x) || inherits(x, "zeroGrob")
}

snake_class <- function(x) {
  x <- class(x)[1]
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  chartr(
    paste0(LETTERS, collapse = ""),
    paste0(letters, collapse = ""),
    x
  )
}

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique0(x))
  }
}

unique_combs <- function(df) {
  if (length(df) == 0) {
    return()
  }
  unique_values <- lapply(df, ulevels)
  rev(expand.grid(
    rev(unique_values), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = TRUE
  ))
}

warn_for_guide_position <- function(guide) {
  breaks_are_unique <- !duplicated(guide$key$.value)
  if (empty(guide$key) || sum(breaks_are_unique) == 1) {
    return()
  }
  if (guide$position %in% c("top", "bottom")) {
    position_aes <- "x"
  } else if (guide$position %in% c("left", "right")) {
    position_aes <- "y"
  } else {
    return()
  }
  if (length(unique0(guide$key[[position_aes]][breaks_are_unique])) == 1) {
    cli::cli_warn(c(
      "Position guide is perpendicular to the intended axis.",
      i = "Did you mean to specify a different guide {.arg position}?"
    ))
  }
}

weave_tables_col <- function(table, table2, col_shift, col_width, name, z = 1,
                             clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    table <- gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(
        table, table2[, i],
        t = panel_row, l = col_ind + 1,
        clip = clip, name = paste0(
          name, "-", seq_along(panel_row), "-", i
        ),
        z = z
      )
    }
  }
  table
}

weave_tables_row <- function(table, table2, row_shift, row_height, name, z = 1,
                             clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    table <- gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(
        table, table2[i, ], t = row_ind + 1,
        l = panel_col, clip = clip, name = paste0(
          name, "-", seq_along(panel_col), "-", i
        ), z = z
      )
    }
  }
  table
}

join_keys <- function(x, y, by) {
  joint <- vec_rbind(x[by], y[by])
  keys  <- id(joint, drop = TRUE)
  list(
    x = keys[seq_nrow(x)],
    y = keys[nrow(x) + seq_nrow(y)],
    n = attr(keys, "n")
  )
}

interleave <- function(x, ...) {
  if (is.unit(x)) {
    units <- c(as.list(x), lapply(list(...), as.list))
    interleaved_list <- vec_interleave(!!!units)
    rlang::inject(unit.c(!!!interleaved_list))
  } else {
    vec_interleave(x, ...)
  }
}

justify_grobs <- function(
  grobs,
  x = NULL, y = NULL,
  hjust = 0.5, vjust = 0.5,
  int_angle = 0, debug = FALSE
) {
  if (!inherits(grobs, "grob")) {
    if (is.list(grobs)) {
      return(
        lapply(grobs, justify_grobs, x, y, hjust, vjust, int_angle, debug)
      )
    } else {
      cli::cli_abort(
        "{.arg grobs} must be an individual {.cls grob} or list of \\
        {.cls grob} objects."
      )
    }
  }
  if (inherits(grobs, "zeroGrob")) {
    return(grobs)
  }
  just <- rotate_just(int_angle, hjust, vjust)
  x <- x %||% unit(just$hjust, "npc")
  y <- y %||% unit(just$vjust, "npc")

  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "lightcyan", col = NA)),
      grobs
    )
  } else {
    children <- gList(grobs)
  }
  result_grob <- gTree(
    children = children,
    vp = viewport(
      x = x, y = y,
      width  = grobWidth(grobs),
      height = grobHeight(grobs),
      just = unlist(just)
    )
  )

  if (isTRUE(debug)) {
    grobTree(
      result_grob,
      pointsGrob(x, y, pch = 20, gp = gpar(col = "mediumturquoise"))
    )
  } else {
    result_grob
  }
}

rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust
  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust
  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }
  list(hjust = hnew, vjust = vnew)
}

compute_just <- function(just, a, b = a, angle = 0) {
  if (any(grepl("outward|inward", just))) {
    angle <- angle %% 360
    angle <- ifelse(angle >  180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)

    rotated_forward <- grepl("outward|inward", just) &
      (angle > 45 & angle < 135)
    rotated_backwards <- grepl("outward|inward", just) &
      (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135

    inward <- (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]

    outward <- (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]
  }
  unname(c(
    left   = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top   = 1
  )[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

`%|W|%` <- function(a, b) {
  if (!inherits(a, "waiver")) a else b
}

is_ggproto <- function(x) {
  inherits(x, "ggproto")
}

on_load(
  if ("is_ggproto" %in% getNamespaceExports("ggplot2")) {
    is_ggproto <- getFromNamespace("is_ggproto", asNamespace("ggplot2"))
  }
)
# nocov end
