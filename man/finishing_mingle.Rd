% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixing.R
\name{finishing_mingle}
\alias{finishing_mingle}
\title{finishing_mingle}
\usage{
finishing_mingle(
  result,
  residual = NULL,
  events = NULL,
  prop = 1,
  day = 98,
  biweekly = FALSE
)
}
\arguments{
\item{result}{the result object}

\item{residual}{the residual object}

\item{events}{the events}

\item{prop}{the proportion to mix}

\item{day}{the day the mixing should occur. This is in the scale
of the countdown timer in the finisher. The first day in the
finisher is 98, which is the default argument.}

\item{biweekly}{Should the events we biweekly. Biweekly events
means that sows only farrow every second week and that at the
beginning of the model only 10 groups of gilts are added to
the model in 14 day increments.}
}
\value{
events
}
\description{
finishing_mingle
}
