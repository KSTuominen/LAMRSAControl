#' Growing buffer to finishing
#'
#'In this function we merge the pigs in buffer pens. It prioritizes merging with
#' a finishing section that already has pigs and the section that has the youngest
#' finishing pigs.
#' @param result The result from the previous day or modified result
#'     from the current day.
#' @param residual The rows with timer == 0 that still need to be
#'     processed. The default is NULL if there is nothing to be done
#' @param events The events that have already been generated. The
#'     default is  NULL in the case that no events have yet been
#'     generated for this day
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
#' @export
#' @return list of the (modified) result, the new residual and the
#'     new events appended to the events you fed to the function
growing_buffer_to_finishing <- function(result,
                                        residual = NULL,
                                        events = NULL,
                                        biweekly = FALSE) {
    ## Index of growing pens that have pigs
    index <- residual$pentype == "Growing buffer"

    ## If there is nothing to do then just get out now
    if(sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    source_pens <- residual[index, "node"]

    ## generating events for moving pigs from growing to finishing
    ## finding empty finishing pens from one section
    empty.finishing.pens <- sectioning(result, "Finishing", sum(index))

    if (identical(empty.finishing.pens, integer(0))) {
        stop(paste("There are no finisher sections with enough",
                   "space to move the animals from the growing buffer"))
    }

    ## what sections are these
    efp <- result[result$node %in% empty.finishing.pens, "section"]

    ## what pens are in those sections
    efp <- result[result$pentype == "Finishing" &
                      result$section %in% efp, ]

    unempty <- efp[efp$npigs > 0, ]

    ## If there are no pigs in any finisher pens then just use the
    ## first pens available and set the contdown of the destination
    ## pens to the normal finisher time.
    if (nrow(unempty) == 0L) {
        dest.pens.vec <- empty.finishing.pens[seq_len(length(source_pens))]

        event1 <- event(type = 9,
                        time = result$time[1],
                        node = source_pens,
                        dest = dest.pens.vec,
                        n = 0,
                        proportion = 1)

        events <- rbind(events, event1)

        ## timers for destination pens to same as their new section
        result$countdown[result$node %in% dest.pens.vec] <- timers("Finishing", biweekly = biweekly)

        ## timer for the pen to be empty before new animals.
        result$countdown[result$node %in% source_pens] <- timers("Growing buffer dt", biweekly = biweekly)
        residual <- residual[!index, ]

        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    ## What are the timers in these sections?
    cdown <- tapply(unempty$countdown,
                    unempty$section,
                    function(x) {
                        max(x[x != 99999], na.rm = TRUE)
                    })

    ## which is the highest ie. the section that last received pigs?
    maxdown_index <- which.max(cdown)
    cdown_section <- names(cdown)[maxdown_index]
    cdown_timer <- cdown[maxdown_index]

    ## The nodes in this max downtime section:
    efp <- efp[efp$section == cdown_section, "node"]

    ## Just keep those in the empty vector that were in this section:
    empty.finishing.pens <- empty.finishing.pens[empty.finishing.pens %in% efp]

    ##sanity check to make sure we are not trying to move more
    stopifnot(sum(index) <= length(empty.finishing.pens))

    ## Move to finisher
    dest.pens.vec <- empty.finishing.pens[seq_len(length(source_pens))]

    ## create the events
    event1 <- event(type = 9,
                    time = result$time[1],
                    node = source_pens,
                    dest = dest.pens.vec,
                    n = 0,
                    proportion = 1)

    events <- rbind(events, event1)

    ## timers for destination pens to same as their new section
    result$countdown[result$node %in% dest.pens.vec] <- cdown_timer

    ## timer for the pen to be empty before new animals.
    result$countdown[result$node %in% source_pens] <- timers("Growing buffer dt", biweekly = biweekly)
    residual <- residual[!index, ]

    return(list(result = result,
                residual = residual,
                events = events))
}
