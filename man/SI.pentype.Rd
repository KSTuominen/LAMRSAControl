% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weaning.R
\name{SI.pentype}
\alias{SI.pentype}
\title{SI.pentype}
\usage{
SI.pentype(
  x,
  SI.comp = c("Sows", "Gilts", "Piglets", "Growers", "Finish"),
  pentype = c("Sow breeding", "Gilt breeding", "Sow breeding buffer",
    "Gilt breeding buffer", "Sow gestation", "Gilt gestation", "Farrowing", "Growing",
    "Growing buffer", "Finishing", "Gilt growing")
)
}
\arguments{
\item{x}{A dataframe which is the output from the trajectory of
the MRSA model from 1 day.}

\item{SI.comp}{A string}

\item{pentype}{A string'}
}
\value{
an integer, sum of animals
}
\description{
function for returning back the sum of of animals both susceptible and infected compartments
from all pens with given pentype
instead of having to type then separately
}
