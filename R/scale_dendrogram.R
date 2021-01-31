# External facing functions -----------------------------------------------

#' @name scale_dendrogram
#' @title Dendrogram position scales
#'
#' @description
#'   When discrete data has some inherent hierarchy to the relationship between
#'   discrete categories, you can display a dendrogram instead of a tick axis.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritDotParams ggplot2::discrete_scale -breaks
#' @param hclust An object of the type produced by the
#'   \code{\link[stats]{hclust}} function.
#'
#' @details The dendrogram type of scale does two things, first it reorders the
#'   values along the relevant direction such that they follow the order
#'   captured in the \code{hclust} argument. Secondly, it draws the dendrogram
#'   at the axis. The dendrogram visuals inherit from the ticks theme elements,
#'   so defining a linetype for the tick marks sets the linetype for the
#'   dendrogram.
#'
#' @export
#'
#' @return A \emph{ScaleDendrogram} ggproto object.
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
  # Do regular discrete axis if no hclust is provided
  if (!inherits(hclust, "hclust")) {
    message("`hclust` argument was not recognised. Switching to regular discrete scale")
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
      try_require("ggdendro", "scale_x_dendrogram")
      guide$dendro <- ggdendro::dendro_data(hclust)
    }
  }

  # Build scale
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"),
                       "position_d",
                       identity,
                       ...,
                       expand = expand,
                       guide = guide,
                       position = position,
                       super = ScaleDendrogram)
  sc$range_c <- .int$continuous_range()
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
      try_require("ggdendro", "scale_y_dendrogram")
      guide$dendro <- ggdendro::dendro_data(hclust)
    }
  }

  # Build scale
  sc <- discrete_scale(c("y", "ymin", "ymax", "yend"),
                       "position_d",
                       identity,
                       ...,
                       expand = expand,
                       guide = guide,
                       position = position,
                       super = ScaleDendrogram)
  sc$range_c <- .int$continuous_range()
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
  transform = function(self, x) {
    hclust <- self$hclust
    if (!inherits(hclust, "waiver") && inherits(hclust, "hclust")) {
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
