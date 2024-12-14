# External facing functions -----------------------------------------------

#' @name scale_dendrogram
#' @title Dendrogram position scales
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' When discrete data has some inherent hierarchy to the relationship between
#' discrete categories, you can display a dendrogram instead of a tick axis.
#' These functions have been deprecated in favour of
#' `legendry::scale_{x/y}_dendro()`.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritDotParams ggplot2::discrete_scale -breaks
#' @param hclust An object of the type produced by the
#'   [stats::hclust()] function.
#'
#' @details The dendrogram type of scale does two things, first it reorders the
#'   values along the relevant direction such that they follow the order
#'   captured in the `hclust` argument. Secondly, it draws the dendrogram
#'   at the axis. The dendrogram visuals inherit from the ticks theme elements,
#'   so defining a linetype for the tick marks sets the linetype for the
#'   dendrogram.
#'
#' @export
#'
#' @return A *ScaleDendrogram* ggproto object.
#' @keywords internal
#'
#' @examples
#' # Hierarchically cluster USArrests
#' yclus <- hclust(dist(USArrests), "ave")
#' xclus <- hclust(dist(t(USArrests)), "ave")
#'
#' # Melting USArrests
#' df <- data.frame(
#'   State = rownames(USArrests)[row(USArrests)],
#'   variable = colnames(USArrests)[col(USArrests)],
#'   value = unname(do.call(c, USArrests))
#' )
#'
#' # Supply the clustering to the scales
#' ggplot(df, aes(variable, State, fill = value)) +
#'   geom_raster() +
#'   scale_y_dendrogram(hclust = yclus) +
#'   scale_x_dendrogram(hclust = xclus)
scale_x_dendrogram <- function(...,
                               hclust = waiver(),
                               expand = waiver(),
                               guide = waiver(),
                               position = "bottom") {
  lifecycle::deprecate_warn(
    "0.3.0",
    "ggh4x::scale_x_dendrogram()",
    "legendry::scale_x_dendro()"
  )
  # Do regular discrete axis if no hclust is provided
  if (!inherits(hclust, "hclust")) {
    return(scale_x_discrete(...,
                            expand = expand,
                            guide = "axis",
                            position = position))
  }

  # Set guide to dendrogram and fill dendrogram data
  if (inherits(guide, "waiver") || is.character(guide) && guide == "dendro") {
    guide <- guide_dendro()
  }
  if (inherits(guide, "guide") && inherits(guide, "dendroguide")) {
    if (inherits(guide$dendro, "waiver")) {
      check_installed("ggdendro", "for `scale_x_dendrogram()`.")
      guide$dendro <- ggdendro::dendro_data(hclust)
    }
  }

  # Build scale
  scale_name <- if (new_guide_system) missing_arg() else "position_d"
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"),
                       scale_name,
                       identity,
                       ...,
                       expand = expand,
                       guide = guide,
                       position = position,
                       super = ScaleDendrogram)
  sc$range_c <- ContinuousRange$new()
  sc$hclust <- hclust
  sc
}

#' @rdname scale_dendrogram
#' @export
scale_y_dendrogram <- function(...,
                               hclust = waiver(),
                               expand = waiver(),
                               guide = waiver(),
                               position = "left") {
  lifecycle::deprecate_warn(
    "0.3.0",
    "ggh4x::scale_y_dendrogram()",
    "legendry::scale_y_dendro()"
  )
  # Do regular discrete axis if no hclust is provided
  if (!inherits(hclust, "hclust")) {
    return(scale_y_discrete(...,
                            expand = expand,
                            guide = "axis",
                            position = position))
  }

  # Set guide to dendrogram and fill dendrogram data
  if (inherits(guide, "waiver") || is.character(guide) && guide == "dendro") {
    guide <- guide_dendro()
  }
  if (inherits(guide, "guide") && inherits(guide, "dendroguide")) {
    if (inherits(guide$dendro, "waiver")) {
      check_installed("ggdendro", "for `scale_y_dendrogram()`.")
      guide$dendro <- ggdendro::dendro_data(hclust)
    }
  }

  # Build scale
  scale_name <- if (new_guide_system) missing_arg() else "position_d"
  sc <- discrete_scale(c("y", "ymin", "ymax", "yend"),
                       scale_name,
                       identity,
                       ...,
                       expand = expand,
                       guide = guide,
                       position = position,
                       super = ScaleDendrogram)
  sc$range_c <- ContinuousRange$new()
  sc$hclust <- hclust
  sc
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
ScaleDendrogram <- ggproto(
  "ScaleDendrogram", ScaleDiscretePosition,
  hclust = waiver(),
  trans = list(), # To not trigger transformation skipping
  transform = function(self, x) {
    hclust <- self$hclust
    if (!inherits(hclust, "waiver") && inherits(hclust, "hclust")) {
      if (length(hclust$labels) != length(hclust$order)) {
        cli::cli_abort(c(
          "{.arg hclust} must have as many labels as there are items.",
          i = "There are {length(hclust$labels)} labels.",
          i = "There are {length(hclust$order)} items."
        ))
      }
      ref <- hclust$labels[hclust$order]
      if (is.factor(x)) {
        x <- as.character(x)
      }
      if (is.character(x)) {
        x <- factor(x, levels = ref)
      }
    }
    return(x)
  })
