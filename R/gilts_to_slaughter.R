##' gilts_to_slaughter
##'
##' @param result The result of a trajectory
##' @param residual the remaining nodes to process
##' @param events an event dataframe
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @return An object to pass to the next event generator
##' @export
gilts_to_slaughter <- function(result,
                               residual = NULL,
                               events = NULL,
                               biweekly = FALSE) {

    ## Index of growing pens that have pigs
    index <- residual$pentype == "Gilt growing" & (residual$npigs != 0)

    ## If there is nothing to do then just get out now
    if(sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    dep <- residual$node[index]
    event1 <- event(type = 15,
                    time = result$time[1],
                    node = dep,
                    dest = 0,
                    n = 0,
                    proportion = 1)

    events <- rbind(events, event1)

    ## timer for the pen to be empty before new animals.
    result$countdown[result$node %in% dep] <- timers("Gilt growing dt", biweekly = biweekly)
    residual <- residual[!index, ]

    return(list(result = result,
                residual = residual,
                events = events))
}
