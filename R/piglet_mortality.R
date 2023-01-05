##' piglet_mortality
##'
##' function to handle the piglet mortality outside siminf prob is the
##' probability of death for each piglet per day, 0.006 when piglets
##' spend 33 d in the unit (total 35 d - 1 day for weaning - 1 day to
##' ignore the first day of life because of piglets mixing) total
##' mortality between birth and weaning according to 2019 statistics is
##' 17.7
##'
##' @param result A result from a model trajectory
##' @param residual The residual
##' @param events The events
##' @param prob The fraction of piglet to die
##' @param skip_days A vector of days to skip for mortality. This is
##'     used to prevent mortality on the same days as mixing since it
##'     interferes.
##' @importFrom stats rbinom
##' @return An events dataframe
##' @export
piglet_mortality <- function(result,
                             residual = NULL,
                             events = NULL,
                             prob = 0.006,
                             skip_days = 34) {

  ## excluding the first day of life because we don't want deaths happen the same day as mixing
  index <- (result$pentype == "Farrowing") & (result$npigs !=0) & (result$countdown > 0) & (!(result$countdown %in% skip_days))

  if (sum(index) == 0) {
    return(list(result = result,
                residual = residual,
                events = events))
  }

  ## Calculate the number of piglets to remove from each pen
  nodes <- result[index, "node"]
  n.pigs <- rowSums(result[index, c("Spiglets", "Ipiglets1", "Ipiglets2", "Ipiglets3")])
  n.remove <- sapply(n.pigs, function(x) {
    rbinom(1, x, prob)
  })

  ## No reason to create events to remove 0 animals so drop those nodes
  nodes <- nodes[n.remove > 0]
  n.remove <- n.remove[n.remove > 0]
  if (length(nodes) == 0) {
    return(list(result = result,
                residual = residual,
                events = events))
  }

  ## now create events if needed
  event1 <- event(type = 12,
                  time = result$time[1],
                  node = nodes,
                  dest = 0,
                  n = n.remove,
                  proportion = 0)

  ## And concatenate these events to the old events and return the list
  events <- rbind(events, event1)
  return(list(result = result,
              residual = residual,
              events = events))
}
