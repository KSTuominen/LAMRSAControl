##' finishing_slaughter
##'
##' @param result The result object
##' @param residual The remaining rows to process for the day
##' @param events Potential events to pas through
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @return An object with the result dataframe, events and residual
##' @export
finishing_slaughter <- function(result,
                                residual = NULL,
                                events = NULL,
                                biweekly = FALSE) {
    ## gather pens that are "eligible for slaughter
    new_in <- result[(result$pentype == "Finishing") & (result$countdown %in% c(0, 7, 14)), ]

    ## residual only has lines with countdown 0, but these pens will
    ## need to be removed after creating events
    index <- residual$pentype == "Finishing" & residual$countdown == 0

    if (nrow(new_in) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    ## Here we want to schedule slaughter events for the whole room
    ## not just the pens in new_in since there can be problems with
    ## individual pens that stochasticcaly have 0 animals now but
    ## later when we rerun the events in the model outside the daily
    ## loop we will get a bug if there are a non-zero number of
    ## animals since no event will have been generated to slughter
    ## these animals that perviously didn't exist.

    getpens <- function(day) {
        result$node[result$section %in%
                    unique(new_in$section[new_in$countdown == day]) &
                    result$pentype == "Finishing"]
    }

    pens <- rbind(data.frame(node = getpens(0),
                             proportion = rep(1, length(getpens(0)))),
                  data.frame(node = getpens(7),
                             proportion = rep(1/2, length(getpens(7)))),
                  data.frame(node = getpens(14),
                             proportion = rep(1/3, length(getpens(14)))))

    ## events to slaughter proportion from each pen in the whole room
    event1 <- event(type = 10,
                    time = result$time[1],
                    node = pens$node,
                    dest = 0,
                    n = 0,
                    proportion = pens$proportion)
    events <- rbind(events, event1)

    ## set timers when the pens are empty
    result$countdown[result$node %in% pens$node[pens$proportion == 1]] <- timers("Finishing dt", biweekly = biweekly)

    ## remove the rows with countdown 0 from residual
    residual <- residual[!index, ]
    return(list(result = result,
                residual = residual,
                events = events))
}
