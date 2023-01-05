##' growing_to_finishing
##'
##' In this function we move pigs from the grower to the finisher
##' section and to growing buffer
##'
##' @param result The result from the previous day or modified result
##'     from the current day.
##' @param residual The rows with timer == 0 that still need to be
##'     processed. The default is NULL if there is nothing to be done
##' @param events The events that have already been generated. The
##'     default is NULL in the case that no events have yet been
##'     generated for this day
##' @param fraction_to_buffer proportion of animals to be moved to buffer pens
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @export
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
growing_to_finishing <- function(result,
                                 residual = NULL,
                                 events = NULL,
                                 fraction_to_buffer = 0.1,
                                 biweekly = FALSE) {

    ## Index of growing pens that have pigs
    index <- residual$pentype == "Growing"

    ## If there is nothing to do then just get out now
    if(sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    ## how many animals we have to move in total
    pigs.per.pen <- residual[index, "npigs"]
    total.piglets.to.move <- sum(pigs.per.pen)

    ## expected number to buffer
    weaner.to.buffer <- round(total.piglets.to.move * fraction_to_buffer)

    ## Number of buffer pens required given the capacity
    total.buffer.pens <- ceiling(weaner.to.buffer / pen_capacity("Growing buffer"))

    ## Finding empty buffer pens
    empty.growing.buffer.pens <- empty.pens(result,
                                            "Growing buffer",
                                            countdown_check = FALSE)[seq_len(total.buffer.pens)]

    ## check that you don't index too many pens in the freepens vector
    stopifnot(all(!is.na(empty.growing.buffer.pens)))

    ## generating events for moving pigs from growing to finishing
    ## finding empty finishing pens from one section
    empty.finishing.pens <- sectioning(result, "Finishing", 0)[seq_len(sum(index))]

    ## check that you don't index too many pens in the
    ## sectioning_multi vector
    stopifnot(all(!is.na(empty.finishing.pens)))

    ## pen ids of the departure pens
    growing.pen.id <- residual[index, "node"]

    for(i in seq_len(sum(index))) {

        ## A single source pen
        node <- growing.pen.id[i]

        ## n buffer pens and one finisher pen:
        dest <- c(empty.growing.buffer.pens,
                  empty.finishing.pens[i])

        ## split the fraction to buffer over the buffer pens and the
        ## rest in the finisher pen:
        p <- c(rep(fraction_to_buffer / total.buffer.pens, total.buffer.pens),
               1 - fraction_to_buffer)

        ## Scale the p to the order to the dest pen ID
        scaled_p <- scale_p(dest, p)

        ## set the type for the events:
        type <- c(rep(18, total.buffer.pens), 9)

        ## create the events:
        event1 <- event(type = type,
                        time = result$time[1],
                        node = node,
                        dest = dest,
                        n = 0,
                        proportion = scaled_p)
        events <- rbind(events, event1)

        ## set the emptied pen timer
        result$countdown[result$node %in% node] <- timers("Growing dt", biweekly = biweekly)
    }

    ## Set the timers for the destination pens
    result$countdown[result$node %in% empty.growing.buffer.pens] <- timers("Growing buffer", biweekly = biweekly)
    result$countdown[result$node %in% empty.finishing.pens] <- timers("Finishing", biweekly = biweekly)

    ## Drop processed rows
    residual <- residual[!index, ]

    return(list(result = result,
                residual = residual,
                events = events))
}
