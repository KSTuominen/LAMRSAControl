##' buffer_culling
##'
##' A function to inject culling events for 100% of the sows and gilts
##' in breeding pens that are in the .
##'
##' @param result The result object
##' @param residual The remaining rows to process for the day
##' @param events Potential events to pas through
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @export
##' @return An object with the result dataframe, events and residual
buffer_culling <- function(result,
                           residual = NULL,
                           events = NULL,
                           biweekly = FALSE) {

    index <- (residual$pentype == "Sow breeding buffer" | residual$pentype == "Gilt breeding buffer")

    ## If there is nothing to do exit
    if (sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    pens.to.remove <- residual[index, ]
    pen.id <- pens.to.remove$node

    ## define the type for the exit event based on the pentype
    type <- rep(14, length(pen.id))
    rem.type <- pens.to.remove$pentype[match(pen.id,
                                             pens.to.remove$node)] == "Gilt breeding buffer"
    type[rem.type] <- 15

    ## Remove all animals from these pens
    event1 <- event(type = type,
                    time = result$time[1],
                    node = pen.id,
                    dest = 0,
                    n = 0,
                    proportion = 1)
    events <- rbind(events, event1)

    result$countdown[result$node %in% pen.id] <- timers("Breeding dt", biweekly = biweekly)

    ## Drop rows from residual
    residual <- residual[!index, ]

    ## return result
    return(list(result = result,
                residual = residual,
                events = events))
}
