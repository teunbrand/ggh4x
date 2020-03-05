# Constructor -------------------------------------------------------------

#' Dendrogram guide
#'
#' Visual representation of a discrete variable with hierarchical relationships
#' between members, like those detailed in
#' \code{\link[=scale_x_dendrogram]{scale_(x|y)_dendrogram)}}.
#'
#' @inheritParams ggplot2::guide_axis
#' @param dendro Relevant plotting data for a dendrogram such as those returned
#'   by \code{\link[ggdendro]{dendro_data}}.
#'
#' @export
#'
#' @return A \emph{dendroguide} class object.
#'
#' @examples
#' clust <- hclust(dist(USArrests), "ave")
#'
#' df <- cbind(State = rownames(USArrests),
#'             USArrests)
#' df <- reshape2::melt(df, id.vars = "State")
#'
#' # The guide function can be used to customise the axis
#' ggplot(df, aes(variable, State, fill = value)) +
#'   geom_raster() +
#' scale_y_dendrogram(hclust = clust,
#'                    guide = guide_dendro(n.dodge = 2))
guide_dendro <- function(
  title = waiver(),
  check.overlap = FALSE,
  n.dodge = 1,
  order = 0,
  position = waiver(),
  dendro = waiver()
) {
  structure(
    list(title = title,
         check.overlap = check.overlap,
         n.dodge = n.dodge,
         order = order,
         position = position,
         available_aes = c("x", "y"),
         dendro = dendro,
         name = "axis"),
    class = c("guide", "dendroguide", "axis")
  )
}


# Transformer -------------------------------------------------------------

#' @method guide_transform dendroguide
#' @export
#' @noRd
guide_transform.dendroguide <- function(guide, coord, panel_params) {
  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }

  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]

  if (all(c("x", "y") %in% aesthetics)) {
    guide$key <- coord$transform(guide$key, panel_params)
  } else {
    other_aesthetic <- setdiff(c("x", "y"), aesthetics)
    override_value <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aesthetic]] <- override_value
    guide$key <- coord$transform(guide$key, panel_params)

    .int$warn_for_guide_position(guide)
  }

  denseg <- guide$dendro$segments
  xvars <- c("x", "xend")
  yvars <- c("y", "yend")
  if (isTRUE(aesthetics == "y")) {
    colnames(denseg) <- chartr("xy", "yx", colnames(denseg))
    denseg[, yvars] <- coord$transform(denseg[, yvars],
                                       panel_params)
    upper <- max(do.call(c, denseg[, xvars]), na.rm = TRUE)
    denseg[, xvars] <- lapply(denseg[, xvars], function(y) {
      scales::rescale(y, from = c(0, upper))
    })
  } else {
    denseg[, xvars] <- coord$transform(denseg[, xvars],
                                       panel_params)
    upper <- max(do.call(c, denseg[, yvars]), na.rm = TRUE)
    denseg[, yvars] <- lapply(denseg[, yvars], function(y) {
      scales::rescale(y, from = c(0, upper))
    })
  }

  guide$dendro$segments <- denseg

  guide
}

# Grob generator ----------------------------------------------------------

#' @method guide_gengrob dendroguide
#' @export
#' @noRd
guide_gengrob.dendroguide <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]

  draw_dendroguide(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$dendro$labels$label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    n.dodge = guide$n.dodge,
    dendro = guide$dendro$segments
  )
}

# Drawing -----------------------------------------------------------------

draw_dendroguide <- function(
  break_positions, break_labels, axis_position, theme,
  check.overlap = FALSE, n.dodge = 1, dendro = NULL
) {
  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"

  # Resolve elements
  line_element_name  <- paste0("axis.line.",  aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.",  aesthetic, ".", axis_position)
  tick_element_name  <- paste0("axis.ticks.", aesthetic, ".", axis_position)

  line_element  <- calc_element(line_element_name, theme)
  label_element <- calc_element(label_element_name, theme)
  tick_element  <- calc_element(tick_element_name, theme)

  # Set vertical labels
  is_vertical <- axis_position %in% c("left", "right")

  position_dim <- if (is_vertical) "y" else "x"
  non_position_dim <- if (is_vertical) "x" else "y"
  position_size <- if (is_vertical) "height" else "width"
  non_position_size <- if (is_vertical) "width" else "height"
  gtable_element <- if (is_vertical) {
    gtable::gtable_row
  } else {
    gtable::gtable_col
  }
  measure_gtable <- if (is_vertical) {
    gtable::gtable_width
  } else {
    gtable::gtable_height
  }
  measure_labels_non_pos <- if (is_vertical) {
    grid::grobWidth
  } else {
    grid::grobHeight
  }

  # Set horizontal labels
  is_second <- axis_position %in% c("right", "top")

  tick_direction <- if (is_second) 1 else -1
  non_position_panel <- if (is_second) unit(0, "npc") else unit(1, "npc")
  tick_coordinate_order <- if (is_second) c(2, 1) else c(1, 2)

  # Set gtable ordering
  labels_first_gtable <- axis_position %in% c("right", "bottom")

  # Set common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom",
                          "bottom" = "top",
                          "right" = "left",
                          "left" = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # Draw elements
  line_grob <- do.call(
    element_grob,
    setNames(
      list(
        line_element,
        unit(c(0, 1), "npc"),
        grid::unit.c(non_position_panel, non_position_panel)
      ),
      c("element", position_dim, non_position_dim)
    )
  )

  if (n_breaks < 1L) {
    return(
      gTree(
        gList(line_grob),
        width = grobWidth(line_grob),
        height = grobHeight(line_grob),
        xmin = NULL, ymin = NULL, vp = NULL, cl = "absoluteGrob"
      )
    )
  }

  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))) {
      break_labels <- do.call(expression, break_labels)
    } else {
      break_labels <- unlist(break_labels)
    }
  }

  # calculate rows/columns of labels
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)

  label_grobs <- lapply(dodge_indices, function(indices) {
    .int$draw_axis_labels(
      break_positions = break_positions[indices],
      break_labels = break_labels[indices],
      label_element = label_element,
      is_vertical = is_vertical,
      check.overlap = check.overlap
    )
  })

  dendro_grob <- grid::segmentsGrob(
    x0 = if (axis_position == "left") 1 - dendro$x else dendro$x,
    y0 = if (axis_position == "bottom") 1 - dendro$y else dendro$y,
    x1 = if (axis_position == "left") 1 - dendro$xend else dendro$xend,
    y1 = if (axis_position == "bottom") 1 - dendro$yend else dendro$yend,
    gp = grid::gpar(
      col = tick_element$colour, fill = tick_element$colour,
      lwd = if (length(tick_element$size) == 0) NULL
      else tick_element$size * .pt,
      lty = tick_element$linetype,
      lineend = tick_element$lineend
    )
  )

  # create gtable
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(grid::unit.c, lapply(label_grobs,
                                             measure_labels_non_pos))
  grobs <- c(list(dendro_grob), label_grobs)
  grob_dims <- grid::unit.c(unit(1, "cm"), label_dims)

  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- base::do.call(
    gtable_element,
    setNames(list("axis", grobs, grob_dims, unit(1, "npc")),
             c("name", "grobs", non_position_sizes, position_size))
  )

  # create viewport
  justvp <- base::do.call(
    grid::viewport,
    setNames(list(non_position_panel, measure_gtable(gt),
                  axis_position_opposite),
             c(non_position_dim, non_position_size, "just"))
  )

  gTree(children = gList(line_grob, gt),
        width = gtable::gtable_width(gt),
        height = gtable::gtable_height(gt),
        xmin = NULL, ymin = NULL, vp = justvp,
        cl = "absoluteGrob")
}
