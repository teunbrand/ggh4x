% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_roll.R
\name{stat_rollingkernel}
\alias{stat_rollingkernel}
\title{Rolling Kernel}
\usage{
stat_rollingkernel(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  ...,
  bw = "nrd",
  kernel = "gaussian",
  n = 256,
  expand = 0.1,
  na.rm = FALSE,
  orientation = "x",
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{Use to override the default geom (\code{"line"}).}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{bw}{A bandwidth, which can be one of the following: \itemize{ \item A
\code{numeric} of length one indicating a measure of kernel width, in data
units. \item A \code{rel} object of length one constructed for setting a
bandwidth relative to the group data range. Can be constructed with the
\code{rel()} function. \item A \code{character} of length one, naming one
of the functions documented in \code{\link[stats:bandwidth]{bw.nrd()}}. }}

\item{kernel}{One of the following: \itemize{ \item A \code{function} that
takes a vector of distances as first argument, a numeric bandwidth as
second argument and returns relative weights. \item A \code{character} of
length one that can take one of the following values: \describe{
\item{\code{"gaussian"} or \code{"norm"}}{A kernel that follows a normal
distribution with 0 mean and bandwidth as standard deviation.}
\item{\code{"mean"} or \code{"unif"}}{A kernel that follows a uniform
distribution with \eqn{bandwidth * -0.5} and \eqn{bandwidth * 0.5} as
minimum and maximum. This is similar to a simple, unweighted moving
average.} \item{\code{"cauchy"}}{A kernel that follows a Cauchy
distribution with 0 as location and bandwidth as scale parameters. The
Cauchy distribution has fatter tails than the normal distribution.} } }}

\item{n}{An \code{integer} of length one: how many points to return per
group.}

\item{expand}{A \code{numeric} of length one: how much to expand the range
for which the rolling kernel is calculated beyond the most extreme
datapoints.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{orientation}{A \code{character} of length one, either \code{"x"}
(default) or \code{"y"}, setting the axis along which the rolling should
occur.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \emph{Layer} ggproto object.
}
\description{
A rolling kernel moves along one of the axes and assigns weights to
datapoints depending on the distance to the kernel's location. It then
calculates a weighted average on the y-values of the datapoints, creating a
trendline. In contrast to (weighted) rolling averages, the interval between
datapoints do not need to be constant.
}
\section{Aesthetics}{
 \code{stat_rollingkernel()} understands the following
aesthetics (required aesthetics are in bold)
\itemize{
\item \strong{x}
\item \strong{y}
\item group
}
}

\section{Computed variables}{
 \describe{
\item{\code{x}}{A sequence of ordered x positions.}
\item{\code{y}}{The weighted value of the rolling kernel.}
\item{\code{weight}}{The sum of weight strengths at a position.}
\item{\code{scaled}}{The fraction of weight strengths at a position. This is
the same as \code{weight / sum(weight)} by group.}
}
}

\examples{
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  stat_rollingkernel()

# The (scaled) weights can be used to emphasise data-dense areas
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  stat_rollingkernel(aes(alpha = after_stat(scaled)))
}
