##' infect_pigs
##'
##' A function to infect pigs using events
##'
##' @param x result dataframe from the herd
##' @param time time when events are scheduled
##' @param pentype A string of the pentype. This could be any valid
##'     pen type the default is "Gilt growing"
##' @param proportion proportion of animals in each node to be
##'     infected
##' @export
##' @return dataframe of scheduled events
infect_pigs <- function(x, time, pentype = "Gilt growing", proportion = 0.2) {
    stopifnot(length(time) == 1L)
    pens <- x[x$pentype == pentype & x$npigs > 0, ]
    nodes <- pens$node
    event(type = 23,
          time = time,
          node = nodes,
          dest = 0,
          n = 0,
          proportion = proportion)
}
