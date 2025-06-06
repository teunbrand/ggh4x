# Main function -----------------------------------------------------------

#' Add a list of scales for non-standard aesthetics
#'
#' Distributes a list of non-standard aesthetics scales to the plot,
#' substituting geom and scale settings as necessary to display the non-standard
#' aesthetics. Useful for mapping different geoms to different scales for
#' example.
#'
#' @param scalelist A `list` wherein elements are the results of calls to a
#'   scale function with a non-standard aesthetic set as the `aesthetic`
#'   argument.
#' @param replaces A `character` vector of the same length as- and parallel
#'   to- `scalelist`, indicating what standard aesthetic to replace with
#'   the non-standard aesthetic. Typically `"colour"` or `"fill"`.
#'
#' @description This function should only be called after all layers that the
#'   non-standard aesthetic scales affects have been added to the plot.
#'
#'   Inside a layer, the non-standard aesthetic should be part of the call to
#'   `aes` mapping.
#'
#'   May return a warning that the plot is ignoring unknown aesthetics.
#'
#' @return A `list` of which the elements are of the class
#'   `MultiScale`.
#' @export
#'
#' @examples
#' # Annotation of heatmap
#' iriscor <- cor(t(iris[, 1:4]))
#'
#' df <- data.frame(
#'   x = as.vector(row(iriscor)),
#'   y = as.vector(col(iriscor)),
#'   value = as.vector(iriscor)
#' )
#'
#' annotation <- data.frame(
#'   z = seq_len(nrow(iris)),
#'   Species = iris$Species,
#'   Leaves = ifelse(iris$Species == "setosa", "Short", "Long")
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_raster(aes(fill = value)) +
#'   geom_tile(data = annotation,
#'             aes(x = z, y = -5, spec = Species), height = 5) +
#'   geom_tile(data = annotation,
#'             aes(y = z, x = -5, leav = Leaves), width = 5) +
#'   scale_listed(
#'     list(scale_fill_brewer(palette = "Set1", aesthetics = "spec"),
#'          scale_fill_brewer(palette = "Dark2", aesthetics = "leav")),
#'     replaces = c("fill", "fill")
#'   )
scale_listed <- function(scalelist, replaces = NULL) {
  # Check replaces validity
  if (length(scalelist) != length(replaces)) {
    cli::cli_abort(
      "The {.arg replaces} argument must be parallel to and of the same \\
      length as the {.arg scalelist} argument."
    )
  }
  replaces <- standardise_aes_names(replaces)
  if (!(all(replaces %in% .all_aesthetics))) {
    cli::cli_abort(
      "The aesthetics in the {.arg replaces} argument must be valid aesthetics."
    )
  }

  # Check scalelist validity
  check <- vapply(scalelist, function(scale) {
    all(inherits(scale, "Scale"),
        inherits(scale, "ggproto"),
        inherits(scale, "gg"))
  }, logical(1))
  if (!all(check)) {
    cli::cli_abort(
      "The {.arg scalelist} argument must have valid {.cls Scale} objects as \\
      list-elements."
    )
  }

  # Check scale aesthetics
  aesthetics <- lapply(scalelist, `[[`, "aesthetics")
  if (any(lengths(aesthetics) > 1)) {
    cli::cli_abort(
      "{.fn scale_listed} can only accept 1 aesthetic per scale."
    )
  }
  aesthetics <- unlist(aesthetics)

  if (length(aesthetics) != length(replaces)) {
    cli::cli_abort(
      "Every scale in the {.arg scalelist} argument must have set \\
      valid aesthetics."
    )
  }

  # Interpret guides
  scalelist <- lapply(seq_along(scalelist), function(i){
    scale <- scalelist[[i]]
    guide <- scale$guide
    if (identical(guide, "none") || identical(guide, FALSE)) {
      return(scale)
    }
    if (!inherits(guide, c("guide", "Guide"))) {
      guide <- match.fun(paste0("guide_", guide))()
    }
    if (inherits(guide, "Guide")) {
      old <- guide
      guide <- ggproto(NULL, old)
    }
    if (!("any" %in% guide$available_aes)) {
      guide$available_aes <- scale$aesthetics
    } else {
      guide$available_aes <- c(guide$available_aes, scale$aesthetics)
    }
    scale$guide <- guide
    return(scale)
  })

  splitlist <- split(scalelist, replaces)
  splitaes  <- split(aesthetics, replaces)

  out <- lapply(names(splitlist), function(aes) {
    structure(list(scales = splitlist[[aes]],
                   aes = splitaes[[aes]],
                   replaced_aes = aes),
              class = "MultiScale")
  })
  return(out)
}

# S3 add method --------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @noRd
#' @export
#' @keywords internal
ggplot_add.MultiScale <- function(object, plot, ...){
  for (i in object$scales){
    plot$scales$add(i)
  }

  replaced_aes <- object$replaced_aes
  replaced_pattern <- paste0("^", replaced_aes, "$")

  plot$layers <- lapply(plot$layers, function(lay){
    if (!any(names(lay$mapping) %in% object$aes)) {
      return(lay)
    }
    new_aes  <- object$aes[object$aes %in% names(lay$mapping)]
    new_aes_pattern <- paste0("^", new_aes, "$")
    old_geom <- lay$geom
    old_geom_nahandle <- old_geom$handle_na
    new_geom_nahandle <- function(self, data, params){
      colnames(data)  <- eval(gsub(new_aes_pattern,
                                   replaced_aes,
                                   colnames(data)))
      old_geom_nahandle(data, params)
    }

    draw_key_old <- old_geom$draw_key
    draw_key_new <- function(data, params, size) {
      colnames(data) <- eval(gsub(new_aes_pattern,
                                  replaced_aes,
                                  colnames(data)))
      draw_key_old(data, params, size)
    }

    new_geom <-
      ggproto(
        paste0("New", new_aes, class(old_geom)[1]),
        old_geom,
        handle_na =
          new_geom_nahandle,
        default_aes =
          setNames(old_geom$default_aes,
                   gsub(replaced_pattern,
                        new_aes,
                        names(old_geom$default_aes))),
        non_missing_aes =
          gsub(replaced_pattern,
               new_aes,
               old_geom$non_missing_aes),
        optional_aes =
          gsub(replaced_pattern,
               new_aes,
               old_geom$optional_aes),
        required_aes =
          gsub(replaced_pattern,
               new_aes,
               old_geom$required_aes),
        draw_key = draw_key_new
      )

    lay$geom <- new_geom
    return(lay)
  })
  return(plot)
}

.all_aesthetics <- c(
  "adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", "fill",
  "group", "hjust", "label", "linetype", "lower", "lty", "lwd", "max", "middle",
  "min", "pch", "radius", "sample", "shape", "size", "srt", "upper", "vjust",
  "weight", "width", "x", "xend", "xmax", "xmin", "xintercept", "y", "yend",
  "ymax", "ymin", "yintercept", "z"
)
