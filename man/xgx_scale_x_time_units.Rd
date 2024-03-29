% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgx_scale_x_time_units.R
\name{xgx_scale_x_time_units}
\alias{xgx_scale_x_time_units}
\alias{xgx_scale_y_time_units}
\title{Convert time units for plotting}
\usage{
xgx_scale_x_time_units(
  units_dataset,
  units_plot = NULL,
  breaks = NULL,
  labels = NULL,
  ...
)

xgx_scale_y_time_units(
  units_dataset,
  units_plot = NULL,
  breaks = NULL,
  labels = NULL,
  ...
)
}
\arguments{
\item{units_dataset}{units of the input dataset, must be specified by user
as "h", "d", "w", "m", or "y"}

\item{units_plot}{units of the plot, will be units of the dataset if empty}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks computed by the
\link[scales:trans_new]{transformation object}
\item A numeric vector of positions
\item A function that takes the limits as input and returns breaks
as output (e.g., a function returned by \code{\link[scales:breaks_extended]{scales::extended_breaks()}}).
Also accepts rlang \link[rlang:as_function]{lambda} function notation.
}}

\item{labels}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}

\item{...}{other parameters for
\code{\link[ggplot2:scale_continuous]{scale_x_continuous}}}
}
\value{
ggplot2 compatible scale object
}
\description{
\code{xgx_scale_x_time_units} converts x axis scale from one time unit
to another.
Supported units include hours, days, weeks, months, and years, which
can also be called using just the first letter (h, d, w, m, y).
}
\details{
Note: \code{xgx_scale_x_time_units} only scales the plot axis, all other
specifications must be on the original scale of the dataset (e.g. breaks,
position, width)
}
\examples{
data <- data.frame(x = 1:1000, y = rnorm(1000))
ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point() +
  xgx_scale_x_time_units(units_dataset = "hours", units_plot = "weeks")
}
