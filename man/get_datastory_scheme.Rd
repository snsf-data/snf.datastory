% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{get_datastory_scheme}
\alias{get_datastory_scheme}
\title{Get your datastory SNSF scheme}
\usage{
get_datastory_scheme(palette = "default", n_col = NULL)
}
\arguments{
\item{palette}{Color palette to choose. Available: "default" (qualitative),
"qualitative", "blue_seq", "green_seq", "yellow_seq", "gray_seq"}

\item{n_col}{The number of colors to return. If it exceeds the number of
colors in the chosen palette, new colors are interpolated. Do not set this
argument if you want to get the original number of colors in the chosen
palette.}
}
\description{
palettes : default (qualitative), qualitative, blue_seq,
green_seq, yellow_seq, gray_seq.
}
\examples{
 # TODO
}
