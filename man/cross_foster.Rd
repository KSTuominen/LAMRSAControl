% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixing.R
\name{cross_foster}
\alias{cross_foster}
\title{cross_foster}
\usage{
cross_foster(result, residual = NULL, events = NULL, prop = 0.1, day = 34)
}
\arguments{
\item{result}{the result object}

\item{residual}{the residual object}

\item{events}{the events}

\item{prop}{the proportion to mix}

\item{day}{The day for the crossfostering. This is the day fo the
countdown timer in the farrowing room. ie 34 is the first day
in the farrowing room and 33 is the second day. If used in
conjunction with mortality events then the mixing and
mortality should not be scheduled on the same day.}
}
\value{
events
}
\description{
cross_foster
}
