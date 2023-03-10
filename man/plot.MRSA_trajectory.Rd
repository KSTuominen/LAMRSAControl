% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farm_image.R
\name{plot.MRSA_trajectory}
\alias{plot.MRSA_trajectory}
\title{plot.MRSA_trajectory}
\usage{
\method{plot}{MRSA_trajectory}(x, tspan = 1:5, path = tempdir(), ...)
}
\arguments{
\item{x}{a cleaned model trajectory. Something like the result of 'clean_trajectory(trajectory(run(model)))'}

\item{tspan}{the days you want to plot}

\item{path}{The path to write the files to}

\item{...}{Other arguments}
}
\value{
A series of plots
}
\description{
plot.MRSA_trajectory
}
\examples{
\dontrun{
library(LAMRSAControl)
model <- MRSA_model_4_parameter()
data(events)
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- clean_trajectory(trajectory(run(model)))
dir.create("movie")
plot(result, tspan = 600:605, path = "movie")

## You may now take these files and create a movie. If you have
## ffmpeg on your machine you can do that like this:
##     ## Combining a series of generated images into a video:
system("ffmpeg -start_number 600 -i movie/plots/plot\%05d.png -r 10 -c:v libx264 -strict -2 -preset veryslow -pix_fmt yuv420p -vf scale=trunc\\\\(iw/2\\\\)*2:trunc\\\\(ih/2\\\\)*2 -f mp4 farm-indirect.mp4")
}
}
