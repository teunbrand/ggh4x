# Main function ----------------------------------------------------------------

#' Linearly transform coordinates
#'
#' Transforms coordinates in two dimensions in a linear manner for layers that
#' have an \code{x} and \code{y} parametrisation.
#'
#' @param scale A \code{numeric} of length two describing relative units with
#'   which to multiply the \code{x} and \code{y} coordinates respectively.
#' @param shear A \code{numeric} of length two giving relavite units by which to
#'   shear the output. The first number is for vertical shearing whereas the
#'   second is for horizontal shearing.
#' @param angle A \code{numeric} noting an angle in degrees by which to rotate
#'   the input clockwise.
#' @param M A \code{2} x \code{2} real \code{matrix}: the transformation matrix
#'   for linear mapping. Overrides other arguments if provided.
#'
#' @details Linear transformation matrices are \code{2} x \code{2} real
#'   matrices. The '\code{scale}', '\code{shear}' and '\code{rotation}'
#'   arguments are convenience arguments to construct a transformation matrix.
#'   These operations occur in the order: scaling - shearing - rotating. To
#'   apply the transformations in another order, build a custom '\code{M}'
#'   argument.
#'
#'   For some common transformations, you can find appropriate matrices for the
#'   '\code{M}' argument below.
#'
#' @section Common transformations: \subsection{Identity transformations}{An
#'   identity transformation, or returning the original coordinates, can be
#'   performed by using the following transformation matrix: \cr\cr\code{| 1 0
#'   |}\cr\code{| 0 1 |}\cr\cr or \cr\cr\code{M <- matrix(c(1, 0, 0, 1), 2)}}
#'   \subsection{Scaling}{A scaling transformation multiplies the dimension of
#'   an object by some amount. An example transformation matrix for scaling
#'   everything by a factor 2: \cr\cr\code{| 2 0 |}\cr\code{| 0 2 |}\cr\cr or
#'   \cr\cr\code{M <- matrix(c(2, 0, 0, 2), 2)}} \subsection{Squeezing}{Similar
#'   to scaling, squeezing multiplies the dimensions by some amount that is
#'   unequal for the \code{x} and \code{y} coordinates. For example, squeezing
#'   \code{y} by half and expanding \code{x} by two: \tabular{cclclcc}{ |
#'   \tab\tab \code{2} \tab\tab \code{0} \tab\tab | \cr | \tab\tab \code{0}
#'   \tab\tab \code{0.5} \tab\tab | \cr } or \cr\cr \code{M <- matrix(c(2, 0, 0,
#'   0.5), 2)}} \subsection{Reflection}{Mirroring the coordinates around one of
#'   the axes. Reflecting around the x-axis: \tabular{cclcrcc}{ | \tab\tab
#'   \code{1} \tab\tab \code{0} \tab\tab | \cr | \tab\tab \code{0} \tab\tab
#'   \code{-1} \tab\tab | \cr } or \cr\cr \code{M <- matrix(c(1, 0, 0, -1), 2)}
#'   \cr\cr Reflecting around the y-axis: \tabular{ccrclcc}{ | \tab\tab
#'   \code{-1} \tab\tab \code{0} \tab\tab | \cr | \tab\tab \code{0} \tab\tab
#'   \code{1} \tab\tab | \cr } or \cr\cr \code{M <- matrix(c(-1, 0, 0, 1), 2)} }
#'   \subsection{Projection}{For projecting the coordinates on one of the axes,
#'   while collapsing everything from the other axis. Projecting onto the
#'   \code{y}-axis: \tabular{cclclcc}{ | \tab\tab \code{0} \tab\tab \code{0}
#'   \tab\tab | \cr | \tab\tab \code{0} \tab\tab \code{1} \tab\tab | \cr } or
#'   \cr\cr \code{M <- matrix(c(0, 0, 0, 1), 2)} \cr\cr Projecting onto the
#'   \code{x}-axis: \tabular{cclclcc}{ | \tab\tab \code{1} \tab\tab \code{0}
#'   \tab\tab | \cr | \tab\tab \code{0} \tab\tab \code{0} \tab\tab | \cr } or
#'   \cr\cr \code{M <- matrix(c(1, 0, 0, 0), 2)} } \subsection{Shearing}{Tilting
#'   the coordinates horizontally or vertically. Shearing vertically by 10\%:
#'   \tabular{cclclcc}{ | \tab\tab \code{1} \tab\tab \code{0} \tab\tab | \cr |
#'   \tab\tab \code{0.1} \tab\tab \code{1} \tab\tab | \cr } or \cr\cr \code{M <-
#'   matrix(c(1, 0.1, 0, 1), 2)} \cr\cr Shearing horizontally by 200\%:
#'   \tabular{cclclcc}{ | \tab\tab \code{1} \tab\tab \code{2} \tab\tab | \cr |
#'   \tab\tab \code{0} \tab\tab \code{1} \tab\tab | \cr } or \cr\cr \code{M <-
#'   matrix(c(1, 0, 2, 1), 2)} } \subsection{Rotation}{A rotation performs a
#'   motion around a fixed point, typically the origin the coordinate system. To
#'   rotate the coordinates by 90 degrees counterclockwise: \tabular{cclcrcc}{ |
#'   \tab\tab \code{0} \tab\tab \code{-1} \tab\tab | \cr | \tab\tab \code{1}
#'   \tab\tab \code{0} \tab\tab | \cr } or \cr\cr \code{M <- matrix(c(0, 1, -1,
#'   0), 2)} \cr\cr For a rotation around any angle \eqn{\theta} :
#'   \tabular{cclcrcc}{ | \tab\tab \eqn{cos\theta} \tab\tab \eqn{-sin\theta}
#'   \tab\tab | \cr | \tab\tab \eqn{sin\theta} \tab\tab \eqn{cos\theta} \tab\tab
#'   | \cr } or \cr\cr \code{M <- matrix(c(cos(theta), sin(theta), -sin(theta),
#'   cos(theta)), 2)} \cr with '\code{theta}' defined in radians. }
#'
#' @return A \emph{PositionLinearTrans} ggproto object.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = c(0, 1, 1, 0),
#'                  y = c(0, 0, 1, 1))
#' ggplot(df, aes(x, y)) +
#'   geom_polygon(position = position_lineartrans(angle = 30))
#'
#' # Custom transformation matrices
#' # Rotation
#' theta <- -30 * pi / 180
#' rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2)
#' # Shear
#' shear <- matrix(c(1, 0, 1, 1), 2)
#'
#' # Shear and then rotate
#' M <- rot %*% shear
#' ggplot(df, aes(x, y)) +
#'   geom_polygon(position = position_lineartrans(M = M))
#' # Alternative shear and then rotate
#' ggplot(df, aes(x, y)) +
#'   geom_polygon(position = position_lineartrans(shear = c(0, 1), angle = 30))
#'
#' # Rotate and then shear
#' M <- shear %*% rot
#' ggplot(df, aes(x, y)) +
#'   geom_polygon(position = position_lineartrans(M = M))
position_lineartrans <- function(scale = c(1,1), shear = c(0,0), angle = 0,
                                 M = NULL) {
  ggproto(NULL, PositionLinearTrans,
          scale = scale, shear = shear,
          angle = angle, M = M)
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
PositionLinearTrans <- ggproto(
  "PositionLinearTrans", Position,
  compute_layer = function(self, data, params, layout) {
    coord <- as.matrix(data[,c("x", "y")])
    coord <- t(params$M %*% t(coord))
    data$x <- coord[, 1]
    data$y <- coord[, 2]
    data
  },
  setup_data = function(self, data, params) {
    data
  },
  setup_params = function(self, data) {
    if (!is.null(self$M)) {
      return(list(M = self$M))
    }
    M <- diag(2)
    # Apply scale
    if (!is.null(self$scale)) {
      M <- M * self$scale
    }
    # Apply shear
    if (!is.null(self$shear) && length(self$shear) == 2) {
      M <- M %*% matrix(c(1, self$shear, 1), ncol = 2)
    }
    # Apply rotation
    if (!is.null(self$angle)) {
      angle <- -self$angle * pi / 180
      angle <- matrix(
        c(cos(angle),  sin(angle),
          -sin(angle), cos(angle)),
        ncol = 2
      )
      M <- angle %*% M
    }
    list(M = M)
  }
)
