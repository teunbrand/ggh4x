# Constructor -------------------------------------------------------------

# nocov start

#' Dendrogram guide
#'
#' `r lifecycle::badge("deprecated")`
#' Visual representation of a discrete variable with hierarchical relationships
#' between members, like those detailed in
#' [`scale_(x|y)_dendrogram)()`][scale_x_dendrogram]. This function is
#' deprecated in favour of `legendry::guide_axis_dendro()`.
#'
#' @inheritParams guide_axis_truncated
#' @param label A `logical(1)`. If `TRUE`, labels are drawn at the
#'   dendrogram leaves. If `FALSE`, labels are not drawn.
#' @param dendro Relevant plotting data for a dendrogram such as those returned
#'   by [ggdendro::dendro_data()].
#'
#' @details The dendrogram guide inherits graphical elements from the
#'   `axis.ticks` theme element. However, the size of the dendrogram is set
#'   to 10 times the `axis.ticks.length` theme element.
#'
#' @export
#' @keywords internal
#'
#' @return A *dendroguide* class object.
#'
#' @examples
#' clust <- hclust(dist(USArrests), "ave")
#'
#' # Melting USArrests
#' df <- data.frame(
#'   State = rownames(USArrests)[row(USArrests)],
#'   variable = colnames(USArrests)[col(USArrests)],
#'   value = unname(do.call(c, USArrests))
#' )
#'
#' # The guide function can be used to customise the axis
#' g <- ggplot(df, aes(variable, State, fill = value)) +
#'   geom_raster() +
#'   scale_y_dendrogram(hclust = clust,
#'                      guide = guide_dendro(n.dodge = 2))
#'
#' # The looks of the dendrogram are controlled through ticks
#' g + theme(axis.ticks = element_line(colour = "red"))
#'
#' # The size of the dendrogram is controlled through tick size * 10
#' g + theme(axis.ticks.length = unit(5, "pt"))
guide_dendro <- function(
  title = waiver(),
  check.overlap = FALSE,
  n.dodge = 1,
  order = 0,
  position = waiver(),
  label = TRUE,
  trunc_lower = NULL,
  trunc_upper = NULL,
  colour = NULL,
  color = NULL,
  dendro = waiver()
) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "ggh4x::guide_dendro()",
    "legendry::guide_axis_dendro()"
  )
  colour <- color %||% colour
  check_trunc_arg(trunc_lower, trunc_upper)
  structure(
    list(title = title,
         check.overlap = check.overlap,
         n.dodge = n.dodge,
         order = order,
         position = position,
         available_aes = c("x", "y"),
         label = label,
         trunc_lower = trunc_lower,
         trunc_upper = trunc_upper,
         colour = colour,
         dendro = dendro,
         name = "axis"),
    class = c("guide", "dendroguide", "axis_ggh4x", "axis")
  )
}


# Trainer -----------------------------------------------------------------

#' @method guide_train dendroguide
#' @export
#' @noRd
guide_train.dendroguide <- function(guide, scale, aesthetic = NULL) {
  guide <- NextMethod()
  if (!is.null(guide$key$.label) & guide$label) {
    i <- seq_len(NROW(guide$dendro$labels))
    guide$dendro$labels$label <- as.character(guide$dendro$labels$label)
    guide$dendro$labels$label[i] <- as.character(guide$key$.label[i])
  } else {
    guide$dendro$labels$label <- NULL
  }
  guide
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

    warn_for_guide_position(guide)
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
  guide$trunc <- transform_truncated(guide$trunc, coord, panel_params)
  guide
}

# Grob generator ----------------------------------------------------------

#' @method guide_gengrob dendroguide
#' @export
#' @noRd
guide_gengrob.dendroguide <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  key <- guide$key
  key$.label <- guide$dendro$labels$label

  draw_dendroguide(
    key = key,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    n.dodge = guide$n.dodge,
    dendro = guide$dendro$segments,
    trunc = guide$trunc,
    colour = guide$colour
  )
}

# Drawing -----------------------------------------------------------------

draw_dendroguide <- function(
  key, axis_position, theme,
  check.overlap = FALSE, n.dodge = 1, dendro = NULL,
  trunc, colour = NULL
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))

  elements <- build_axis_elements(axis_position, angle = NULL, theme, colour)

  params <- setup_axis_params(axis_position)
  params$labels_first <- !params$labels_first

  line_grob <- build_trunc_axis_line(elements$line, params, trunc)

  if ({n_breaks <- nrow(key)} == 0) {
    out <- grid::gTree(
      children = grid::gList(line_grob),
      width = grid::grobWidth(line_grob),
      height = grid::grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels(
    elements, key = key,
    dodge = n.dodge, check.overlap = check.overlap, params = params
  )

  dendro_grob <- grid::segmentsGrob(
    x0 = if (axis_position == "l") 1 - dendro$x else dendro$x,
    y0 = if (axis_position == "b") 1 - dendro$y else dendro$y,
    x1 = if (axis_position == "l") 1 - dendro$xend else dendro$xend,
    y1 = if (axis_position == "b") 1 - dendro$yend else dendro$yend,
    gp = element_grob(elements$ticks)$gp
  )

  elements$tick_length <- elements$tick_length * 10

  assemble_axis_grobs(
    ticks = dendro_grob, labels = label_grobs,
    lines = line_grob, elements = elements,
    params = params
  )
}

# nocov end
