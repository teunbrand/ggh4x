# Constructor -------------------------------------------------------------

#' Flexible rectangles
#'
#' The `geom_box()` function offers a more flexible variant of `geom_rect()` and
#' `geom_tile()`. Instead of exclusively working with the `(x/y)min`/`(x/y)max`
#' *or* `(x/y)`/`(width/height)` aesthetics, any two out of these four
#' aesthetics suffice to define a rectangle.
#'
#' @inheritParams ggplot2::geom_rect
#' @param radius A [`grid::unit`] object of length 1 or `numeric(1)` to set
#'  rounded corners. If `NULL` (default), no rounded corners are applied. If
#'  `numeric(1)`, it is interpreted as millimetres. Does not work under
#'  non-linear coordinates.
#'
#' @return A ggplot2 `Layer` object that can be added to a plot.
#' @export
#'
#' @examples
#' # Combine any two position aesthetics
#' df <- data.frame(
#'   x = c(1.5, 3.5), xmin = c(1, 2),
#'   y = c(1.5, 2.5), ymin = c(1, 2)
#' )
#' ggplot(df) +
#'   geom_box(aes(x = x, xmin = xmin, y = y, ymin = ymin))
#'
#' # Works with partial information for position, as long as two aesthetics
#' # are complete for any observation.
#' df <- data.frame(
#'   x = c(1.5, NA, 4),   xmin = c(1, 2, NA), width = c(NA, 3, 2),
#'   y = c(1.5, 2.5, NA), ymin = c(NA, 2, 3), height = c(1, NA, 3)
#' )
#' ggplot(df) +
#'   geom_box(aes(x = x, xmin = xmin, y = y, ymin = ymin,
#'                width = width, height = height))
#'
#' # Set radius for rounded corners
#' ggplot() +
#'   geom_box(
#'     aes(x = 1:3, width = rep(1, 3),
#'         y = 1:3, height = 3:1),
#'     radius = 5
#'   )
geom_box <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  linejoin = "mitre",
  na.rm    = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  radius   = NULL
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomBox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params   = list2(
      linejoin = linejoin,
      na.rm    = na.rm,
      radius   = radius,
      ...
    )
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomBox <- ggproto(
  "GeomBox", Geom,

  optional_aes = c("xmin", "xmax", "x", "width",
                   "ymin", "ymax", "y", "height"),

  default_aes = aes(
    colour    = NA,
    fill      = "grey35",
    linewidth = 0.5,
    linetype  = 1,
    alpha     = NA
  ),

  setup_data = function(data, params) {


    x <- resolve_box(
      data[["xmin"]], data[["xmax"]], data[["x"]],
      data[["width"]] %||% params$width
    )

    y <- resolve_box(
      data[["ymin"]], data[["ymax"]], data[["y"]],
      data[["height"]] %||% params$height
    )

    # Check for missing rows
    missing <- character()
    missing <- if (anyNA(x$min)) c(missing, 'xmin') else missing
    missing <- if (anyNA(x$max)) c(missing, 'xmax') else missing
    missing <- if (anyNA(y$min)) c(missing, 'ymin') else missing
    missing <- if (anyNA(y$max)) c(missing, 'ymax') else missing


    if (length(missing)) {
      tip <- character()
      if (any(grepl("^x", missing))) {
        xargs <- c("xmin", "xmax", "x", "width")
        tip <- c(tip, i = paste0(
          "Have you specified {.emph exactly} two of {.or {.field {xargs}}} for ",
          "every row?"
        ))
      }
      if (any(grepl("^y", missing))) {
        yargs <- c("ymin", "ymax", "y", "height")
        tip <- c(tip, i = paste0(
          "Have you specified {.emph exactly} two of {.or {.field {yargs}}} for ",
          "every row?"
        ))
      }
      cli::cli_warn(c(
        "Could not resolve the position of every {.arg {missing}} \\
        aesthetic{?s}.", tip))
    }

    data[c("xmin", "xmax", "ymin", "ymax")] <- list(x$min, x$max, y$min, y$max)
    data[c("x", "width", "y", "height")] <- NULL
    data
  },

  draw_panel = function(
    self, data, panel_params, coord,
    lineend = "butt", linejoin = "mitre", radius = NULL
  ) {

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )

      # Rectangle to polygon
      new_data <- data[rep(seq_nrow(data), each = 5), , drop = FALSE]
      new_data$x <- vec_interleave(
        data$xmin, data$xmax, data$xmax, data$xmin, data$xmin
      )
      new_data$y <- vec_interleave(
        data$ymax, data$ymax, data$ymin, data$ymin, data$ymin
      )
      new_data$xmin <- new_data$xmax <- new_data$ymin <- new_data$ymax <- NULL

      grob <- GeomPolygon$draw_panel(
        new_data, panel_params, coord, lineend = lineend, linejoin = linejoin
      )
      return(grob)
    }

    coords <- coord$transform(data, panel_params)
    coords$fill      <- alpha(coords$fill, coords$alpha)
    coords$linewidth <- coords$linewidth * .pt
    coords$width     <- coords$xmax - coords$xmin
    coords$height    <- coords$ymax - coords$ymin

    if (is.null(radius)) {
      ggname(
        "geom_box", rectGrob(
          coords$xmin, coords$ymax,
          width  = coords$width,
          height = coords$height,
          default.units = "native",
          just   = c("left", "top"),
          gp = gpar(
            col  = coords$colour,
            fill = coords$fill,
            lwd  = coords$linewidth,
            lty  = coords$linetype,
            linejoin = linejoin,
            lineend  = lineend
          )
        )
      )
    } else {
      if (is.numeric(radius)) {
        radius <- unit(radius, "mm")
      }
      if (!is.unit(radius)) {
        radius <- unit(0, "pt")
      }

      grobs <- lapply(seq_len(nrow(coords)), function(i) {
        roundrectGrob(
          coords$xmin[i], coords$ymax[i],
          width  = coords$width[i],
          height = coords$height[i],
          r      = radius,
          default.units = "native",
          just   = c("left", "top"),
          gp = gpar(
            col  = coords$colour[i],
            fill = coords$fill[i],
            lwd  = coords$linewidth[i],
            lty  = coords$linetype[i],
            linejoin = linejoin,
            lineend  = lineend
          )
        )
      })
      ggname("geom_box", do.call(grobTree, grobs))
    }

  },

  draw_key = draw_key_polygon
)

# Function for resolving the min, max given partial information
# Priority order is:
# - min/max verbatim
# - center +/- 0.5 * dim
# - opposite +/- dim
resolve_box <- function(min = NULL, max = NULL, center = NULL, dim = NULL) {
  n <- max(length(min), length(max), length(center), length(dim))
  if (n == 0) {
    return(NULL)
  }

  # Replace missing by NAs
  min <- min %||% rep(NA, n)
  max <- max %||% rep(NA, n)

  if (!anyNA(min) && !anyNA(max)) {
    ans <- list(min = pmin(min, max), max = pmax(min, max))
    return(ans)
  }

  center <- center %||% rep(NA, n)
  dim    <- dim    %||% rep(NA, n)

  if (anyNA(min)) {
    i <- which(is.na(min))
    min[i] <- center[i] - 0.5 * dim[i]
  }
  if (anyNA(max)) {
    i <- which(is.na(max))
    max[i] <- center[i] + 0.5 * dim[i]
  }
  if (!anyNA(min) && !anyNA(max)) {
    ans <- list(min = pmin(min, max), max = pmax(min, max))
    return(ans)
  }
  if (anyNA(dim)) {
    i <- which(is.na(dim))
    dim[i] <- (center[i] - min[i]) * 2
    if (anyNA(dim)) {
      i <- which(is.na(dim))
      dim[i] <- (max[i] - center[i]) * 2
    }
  }

  if (anyNA(min)) {
    i <- which(is.na(min))
    min[i] <- max[i] - dim[i]
  }
  if (anyNA(max)) {
    i <- which(is.na(max))
    max[i] <- min[i] + dim[i]
  }
  list(min = pmin(min, max), max = pmax(min, max))
}
