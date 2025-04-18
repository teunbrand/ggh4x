% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_dendrogram.R
\name{scale_dendrogram}
\alias{scale_dendrogram}
\alias{scale_x_dendrogram}
\alias{scale_y_dendrogram}
\title{Dendrogram position scales}
\usage{
scale_x_dendrogram(
  ...,
  hclust = waiver(),
  expand = waiver(),
  guide = waiver(),
  position = "bottom"
)

scale_y_dendrogram(
  ...,
  hclust = waiver(),
  expand = waiver(),
  guide = waiver(),
  position = "left"
)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale}}
  \describe{
    \item{\code{aesthetics}}{The names of the aesthetics that this scale works with.}
    \item{\code{scale_name}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} The name of the scale
that should be used for error messages associated with this scale.}
    \item{\code{palette}}{A palette function that when called with a single integer
argument (the number of levels in the scale) returns the values that
they should take (e.g., \code{\link[scales:pal_hue]{scales::pal_hue()}}).}
    \item{\code{name}}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}
    \item{\code{labels}}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{limits}}{One of:
\itemize{
\item \code{NULL} to use the default scale values
\item A character vector that defines possible values of the scale and their
order
\item A function that accepts the existing (automatic) values and returns
new ones. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{na.translate}}{Unlike continuous scales, discrete scales can easily show
missing values, and do so by default. If you want to remove missing values
from a discrete scale, specify \code{na.translate = FALSE}.}
    \item{\code{na.value}}{If \code{na.translate = TRUE}, what aesthetic value should the
missing values be displayed as? Does not apply to position scales
where \code{NA} is always placed at the far right.}
    \item{\code{drop}}{Should unused factor levels be omitted from the scale?
The default, \code{TRUE}, uses the levels that appear in the data;
\code{FALSE} includes the levels in the factor. Please note that to display
every level in a legend, the layer should use \code{show.legend = TRUE}.}
    \item{\code{call}}{The \code{call} used to construct the scale for reporting messages.}
    \item{\code{super}}{The super class to use for the constructed scale}
  }}

\item{hclust}{An object of the type produced by the
\code{\link[stats:hclust]{stats::hclust()}} function.}

\item{expand}{For position scales, a vector of range expansion constants used to add some
padding around the data to ensure that they are placed some distance
away from the axes. Use the convenience function \code{\link[ggplot2:expansion]{expansion()}}
to generate the values for the \code{expand} argument. The defaults are to
expand the scale by 5\% on each side for continuous variables, and by
0.6 units on each side for discrete variables.}

\item{guide}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}

\item{position}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}
}
\value{
A \emph{ScaleDendrogram} ggproto object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
When discrete data has some inherent hierarchy to the relationship between
discrete categories, you can display a dendrogram instead of a tick axis.
These functions have been deprecated in favour of
\verb{legendry::scale_\{x/y\}_dendro()}.
}
\details{
The dendrogram type of scale does two things, first it reorders the
values along the relevant direction such that they follow the order
captured in the \code{hclust} argument. Secondly, it draws the dendrogram
at the axis. The dendrogram visuals inherit from the ticks theme elements,
so defining a linetype for the tick marks sets the linetype for the
dendrogram.
}
\examples{
# Hierarchically cluster USArrests
yclus <- hclust(dist(USArrests), "ave")
xclus <- hclust(dist(t(USArrests)), "ave")

# Melting USArrests
df <- data.frame(
  State = rownames(USArrests)[row(USArrests)],
  variable = colnames(USArrests)[col(USArrests)],
  value = unname(do.call(c, USArrests))
)

# Supply the clustering to the scales
ggplot(df, aes(variable, State, fill = value)) +
  geom_raster() +
  scale_y_dendrogram(hclust = yclus) +
  scale_x_dendrogram(hclust = xclus)
}
\keyword{internal}
