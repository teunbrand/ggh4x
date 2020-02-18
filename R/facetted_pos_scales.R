# User functions ----------------------------------------------------------

#' Set individual scales at facets
#'
#' Under default circumstances, using facets in a plot limits the freedom with
#' which the position scales (x and y) can be defined. Calling this function
#' after facets have been defined allows fine-tuning of the position scales.
#'
#' @param x A \code{list} wherein elements are either x position scales or
#'   \code{NULL}s.
#' @param y A \code{list} wherein elements are either y position scales or
#'   \code{NULL}s.
#'
#' @export
#'
#' @details It is intended that this function works with both
#'   \code{\link[ggplot2]{facet_wrap}} and \code{\link[ggplot2]{facet_grid}}.
#'   For \code{facet_wrap}, the scales are used for each individual panel. For
#'   \code{facet_grid}, the scales are used for the rows and columns. Note that
#'   these facets must be used with \code{scales = "free"} or \code{"free_x"} or
#'   \code{"free_y"}, depending on what scales are added.
#'
#'   Axis titles are derived from the first scale in the list (or the default
#'   position scale when the first list element is \code{NULL}).
#'
#'   It is allowed to use individual scale transformations for facets, but this
#'   functionality comes with the trade-off that the out of bounds (\code{oob})
#'   argument for individual scales is ignored. Values that are out of bounds
#'   will be clipped.
#'
#'   \code{NULL}s are valid list elements and signal that the default position
#'   scale should be used at the position in the list where the \code{NULL}
#'   occurs. Since transformations are applied before facet scales are
#'   initiated, it is not recommended to use a default position scale with a
#'   transformation other than \code{trans = "identity"} (the default).
#'
#' @seealso \code{\link[ggplot2]{scale_continuous}} and \code{scale_x_discrete}.
#'
#' @examples
#' # Reversing the y-axis in the second panel
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(colour = Species)) +
#'   facet_wrap(Species ~ ., scales = "free_y") +
#'   facetted_pos_scales(
#'     y = list(NULL, scale_y_continuous(trans = "reverse"))
#'   )
facetted_pos_scales <- function(x = NULL, y = NULL) {
  x_test <- vapply(x, function(xi){
    inherits(xi, "Scale") | is.null(xi)
  }, logical(1)) | is.null(x)
  y_test <- vapply(y, function(yi){
    inherits(yi, "Scale") | is.null(yi)
  }, logical(1)) | is.null(y)
  
  if (!(all(x_test) & all(y_test))) {
    stop("Invalid facetted scale specifications.")
  }
  
  structure(list(x = x, y = y), class = "facetted_pos_scales")
}

# S3 add method -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @noRd
#' @export
#' @keywords internal
ggplot_add.facetted_pos_scales <- function(object, plot, object_name) {
  
  if (is.null(object$x) & is.null(object$y)) {
    return(plot)
  }
  # Check if we can overide core functions
  valid_init <- identical(body(environment(Facet$init_scales)$f), 
                          body(environment(plot$facet$init_scales)$f))
  valid_train <- identical(body(environment(Facet$train_scales)$f),
                           body(environment(plot$facet$train_scales)$f))
  valid_finish <- identical(body(environment(Facet$finish_data)$f),
                            body(environment(plot$facet$finish_data)$f))
  
  if (!all(c(valid_init, valid_train, valid_finish))) {
    warning("Unknown facet, overriding facetted scales may be unstable.",
            call. = FALSE)
  }
  
  # Copy facet
  oldfacet <- plot$facet
  
  # Reconstitute new facet
  newfacet <- ggproto(
    paste0("FreeScaled", class(oldfacet)[[1]]),
    oldfacet,
    new_x_scales = object$x,
    new_y_scales = object$y,
    init_scales  = init_scales_individual,
    train_scales = train_scales_individual,
    finish_data  = finish_data_individual
  )
  
  plot$facet <- newfacet
  plot
}

# ggproto methods ---------------------------------------------------------

#' @keywords internal
init_scales_individual <- function(layout, 
                                   x_scale = NULL, y_scale = NULL, 
                                   params, self) {
  scales <- list()
  
  # Handle x
  if (!is.null(x_scale)) {
    new_x <- self$new_x_scales
    xidx <- seq_len(max(layout$SCALE_X))
    
    if (is.null(new_x)) {
      # Setup scales as per usual
      scales$x <- lapply(xidx, function(i) {
        x_scale$clone()
      })
      
    } else {
      # Substituting scales if needed
      scales$x <- lapply(xidx, function(i) {
        # Check i^th scale exists
        if (length(new_x) >= i) {
          if (!is.null(new_x[[i]])) {
            new <- new_x[[i]]$clone()
            # oob handling must be overridden
            new$oob <- function(x, ...) x
            return(new)
          }
        }
        return(x_scale$clone())
      })
    }
  }
  
  # Handle y
  if (!is.null(y_scale)) {
    new_y <- self$new_y_scales
    yidx <- seq_len(max(layout$SCALE_Y))
    
    if (is.null(new_y)) {
      # Setup scales as per usual
      scales$y <- lapply(yidx, function(i) {
        y_scale$clone()
      })
      
    } else {
      # Substitute scales if needed
      scales$y <- lapply(yidx, function(i) {
        # Check i^th scale exists
        if (length(new_y) >= i) {
          if (!is.null(new_y[[i]])) {
            new <- new_y[[i]]$clone()
            # oob handling must be overridden
            new$oob <- function(x, ...) x
            return(new)
          }
        }
        return(y_scale$clone())
      })
    }
  }
  scales
}

#' @keywords internal
train_scales_individual <- function(x_scales, y_scales, layout, data, params, self) {
  # Transform data first
  data <- lapply(data, function(layer_data) {
    self$finish_data(layer_data, layout, 
                     x_scales, y_scales, params)
  })
  
  # Then use parental method for scale training
  ggproto_parent(Facet, self)$train_scales(x_scales, y_scales, 
                                           layout, data, params)
}

#' @keywords internal
finish_data_individual <- function(data, layout, x_scales, y_scales, params) {
  # Divide data by panel
  panels <- split(data, data$PANEL, drop = FALSE)
  panels <- lapply(names(panels), function(i) {
    dat  <- panels[[i]]
    
    # Match panel to their scales
    panel_id <- match(as.numeric(i), layout$PANEL)
    xidx <- layout[panel_id, "SCALE_X"]
    yidx <- layout[panel_id, "SCALE_Y"]
    
    # Decide what variables need to be transformed
    y_vars <- intersect(y_scales[[yidx]]$aesthetics, names(dat))
    x_vars <- intersect(x_scales[[xidx]]$aesthetics, names(dat))
    
    # Transform variables by appropriate scale
    for (j in y_vars) {
      dat[, j] <- y_scales[[yidx]]$transform(dat[, j])
    }
    for (j in x_vars) {
      dat[, j] <- x_scales[[xidx]]$transform(dat[, j])
    }
    dat
  })
  
  # Recombine the data
  data <- unsplit(panels, data$PANEL)
  data
}