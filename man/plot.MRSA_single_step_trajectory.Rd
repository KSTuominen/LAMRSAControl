% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farm_image.R
\name{plot.MRSA_single_step_trajectory}
\alias{plot.MRSA_single_step_trajectory}
\title{plot.MRSA_single_step_trajectory}
\usage{
\method{plot}{MRSA_single_step_trajectory}(
  x,
  xlim = c(-8, 100),
  col = c("#FF000000", "#FF000019", "#FF000032", "#FF00004b", "#FF000064", "#FF00007d",
    "#FF000096", "#FF0000af", "#FF0000c8", "#FF0000e1", "#FF0000fe"),
  breaks = c(0, 6.310501, 18.707384, 31.389292, 44.337289, 58.628841, 74.981242,
    93.845223, 119.361628, 175.595168),
  ...
)
}
\arguments{
\item{x}{a trajectory for one step}

\item{xlim}{The limits of the x axis of the plot}

\item{col}{The colours of the background of the nodes}

\item{breaks}{The breaks in phi to select a colour for a node}

\item{...}{Other arguments}
}
\value{
A plot
}
\description{
plot.MRSA_single_step_trajectory
}
