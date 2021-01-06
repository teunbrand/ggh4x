# User functions ----------------------------------------------------------

#' Set individual scales in facets
#'
#' This function allows the tweaking of the position scales (x and y) of
#' individual facets. You can use it to fine-tune limits, breaks and other scale
#' parameters for individual facets, provided the facet allows free scales.
#'
#' @param x,y A \code{list} wherein elements are either x/y position scales or
#'   \code{NULL}s. Alternatively, a list of formulae (see details).
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
#'   \subsection{Scale transformations}{It is allowed to use individual scale
#'   transformations for facets, but this functionality comes with the trade-off
#'   that the out of bounds (\code{oob}) argument for individual scales is
#'   ignored. Values that are out of bounds will be clipped. Whereas the
#'   \code{stat} part of a ggplot layer is typically calculated after scale
#'   transformations, the calculation of the \code{stat} happens before scale
#'   transformation with this function, which can lead to some awkward results.
#'   The suggested workaround is to pre-transform the data for layers with
#'   non-identity \code{stat} parts.}
#'
#'   \subsection{Scale list input}{\code{NULL}s are valid list elements and
#'   signal that the default position scale should be used at the position in
#'   the list where the \code{NULL} occurs. Since transformations are applied
#'   before facet scales are initiated, it is not recommended to use a default
#'   position (either the first in the list, or defined outside
#'   \code{facetted_pos_scales()}) scale with a transformation other than
#'   \code{trans = "identity"} (the default).}
#'
#'   \subsection{Formula list input}{The \code{x} and \code{y} arguments also
#'   accept a list of two-sided formulas. The left hand side of a formula should
#'   evaluate to a \code{logical} vector. The right hand side of the formula
#'   should evaluate to a position scale, wherein the \code{x} argument accepts
#'   x-position scales and the \code{y} argument accepts y-position scales.
#'   Notably, the left hand side of the formula is evaluated using the tidy
#'   evaluation framework, whereby the \code{data.frame} with the plot's layout
#'   is given priority over the environment in which the formula was created. As
#'   a consequence, variables (columns) that define faceting groups can be
#'   references directly.}
#'
#' @seealso \code{\link[ggplot2]{scale_continuous}} and \code{scale_x_discrete}.
#'
#' @return A \emph{facetted_pos_scales} object, instructing a ggplot how to
#'   adjust the scales per facet.
#'
#' @examples
#' plot <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(colour = Species)) +
#'   facet_wrap(Species ~ ., scales = "free_y")
#'
#' # Reversing the y-axis in the second panel. When providing a list of scales,
#' # NULL indicates to use the default, global scale
#' plot +
#'   facetted_pos_scales(
#'     y = list(NULL, scale_y_continuous(trans = "reverse"))
#'   )
#'
#' # Alternative for specifying scales with formula lists. The LHS can access
#' # columns in the plot's layout.
#' plot +
#'   facetted_pos_scales(
#'     y = list(
#'       Species == "virginica" ~ scale_y_continuous(breaks = c(6, 7)),
#'       Species == "versicolor" ~ scale_y_reverse()
#'     )
#'   )
facetted_pos_scales <- function(x = NULL, y = NULL) {
  # Safeguard against non-list input
  if (!is.list(x)) {
    x <- list(x)
  }
  if (!is.list(y)) {
    y <- list(y)
  }

  # Check input
  x_test <- check_facetted_scale(x, "x")
  y_test <- check_facetted_scale(y, "y")
  if (!(x_test & y_test)) {
    stop("Invalid facetted scale specifications.")
  }

  x <- validate_facetted_scale(x, "x")
  y <- validate_facetted_scale(y, "y")

  structure(list(x = x, y = y), class = "facetted_pos_scales")
}

#' @keywords internal
check_facetted_scale <- function(x, aes = "x", allow_null = TRUE) {
  if (is.null(x)) {
    return(TRUE)
  }

  # Basic class checks for all list elements
  is_scale <- vapply(x, inherits, logical(1), "Scale")
  is_null  <- vapply(x, is.null,  logical(1))
  is_form  <- vapply(x, inherits, logical(1), "formula")

  if (all(is_form)) {
    return(TRUE)
  }

  appropriate_aes <- vapply(x[is_scale], function(y) {
    any(y[["aesthetics"]] == aes)
  }, logical(1))
  is_scale[is_scale] <- is_scale[is_scale] & appropriate_aes

  if (allow_null) {
    if (all(is_scale | is_null)) {
      return(TRUE)
    }
  } else {
    if (all(is_scale)) {
      return(TRUE)
    }
  }

  return(FALSE)
}


#' @keywords internal
#' @importFrom rlang as_quosure f_env f_lhs f_rhs
validate_facetted_scale <- function(x, aes = "x") {
  # Checked earlier for formula, so should be the only is.language case
  if (!is.language(x[[1]])) {
    # Should be good to go
    return(x)
  }

  # Otherwise, interpret language
  # Keep left hand side as call
  lhs <- lapply(x, function(f) {
    as_quosure(f_lhs(f), env = f_env(f))
  })
  # Evaluate right hand side now
  rhs <- lapply(x, function(f) {
    eval(f_rhs(f), envir = f_env(f))
  })

  # Double check for appropriate scales
  check <- check_facetted_scale(rhs, aes = aes, allow_null = FALSE)
  if (!check) {
    stop("RHS of formula does not result in appropriate scale.")
  }

  return(
    structure(rhs, lhs = lhs, class = "list")
  )
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
init_scale <- function(old, new, layout, aes = "x") {

  if (is.null(old)) {
    return(NULL)
  }

  scalename <- paste0("SCALE_", toupper(aes))
  idx <- seq_len(max(layout[[scalename]]))

  out <- lapply(idx, function(i) old$clone())

  if (!("lhs" %in% names(attributes(new)))) {

    # Do regular stuff
    i <- which(lengths(new) > 0)
    if (length(i)) {
      out[i] <- lapply(new[i], function(x) {
        x <- x$clone()
        x$oob <- oob_keep
        x
      })
    }

  } else {
    lhs <- attr(new, "lhs")
    n <- NROW(layout)

    # Evaluate LHS expressions in context of layout
    m <- vapply(lhs, function(x) {
      rep_len(as.logical(rlang::eval_tidy(x, layout)), n)
    }, logical(n))
    if (is.null(dim(m))) {
      dim(m) <- c(n, 1)
    }

    for (i in rev(seq_len(NCOL(m)))) {
      # Match up scale IDs
      j <- which(m[, i])
      # j <- which(layout[[scalename]] %in% layout[[scalename]][j])
      j <- unique(layout[j, scalename])
      # Replace appropriate IDs
      out[j] <- lapply(seq_along(out[j]), function(x) {
        x <- new[[i]]$clone()
        x$oob <- oob_keep
        x
      })
    }
  }

  return(out)
}

#' @keywords internal
init_scales_individual <- function(layout,
                                   x_scale = NULL, y_scale = NULL,
                                   params, self) {
  scales <- list()

  # Handle x
  scales$x <- init_scale(x_scale, self$new_x_scales, layout, aes = "x")
  scales$y <- init_scale(y_scale, self$new_y_scales, layout, aes = "y")
  scales <- scales[lengths(scales) > 0]

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
