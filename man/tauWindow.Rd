% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tauWindow.R
\name{tauWindow}
\alias{tauWindow}
\title{Smart windowing function for time series}
\usage{
tauWindow(x, from, to)
}
\arguments{
\item{x}{An xts matrix}

\item{from}{Either a Date, a number of obserations,
or NULL (for all data since start)}

\item{to}{Either a Date
or NULL (for all data to end)}
}
\value{
An xts matrix
}
\description{
This function is a lot like \link{window.zoo},
but smarter because \code{from} can be a number,
meaning that many data points.
}
\details{
If either \code{from} or \code{to} is NULL,
then data is not chopped from the respective end.
}
