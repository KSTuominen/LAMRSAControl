##' Abortion
##'
##' In this function we move bred animals from the gestation section
##' to a breeding buffer section or cull them.
##'
##' @param result The result from the previous day or modified result
##'     from the current day.
##' @param residual The rows with timer == 0 that still need to be
##'     processed. The default is NULL if there is nothing to be done
##' @param events The events that have already been generated. The
##'     default is NULL in the case that no events have yet been
##'     generated for this day
##' @param abortion.chance The probability of abortion per day
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
abortion <- function(result,
                     residual = NULL,
                     events = NULL,
                     abortion.chance = 0.0029,
                     biweekly = FALSE) {

    gest.timer <- timers("Gestation", biweekly = biweekly)

    ## collecting the rows that have sows in the first 28 days of their gestation
    early.gestation <- NULL
    early.gestation <- result[(result$pentype == "Sow gestation"| result$pentype == "Gilt gestation") &
                                  (result$countdown <= gest.timer & result$countdown >= (gest.timer-28)), ]

    if (nrow(early.gestation) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }
    for (i in seq_len(nrow(early.gestation))) {
        ## go through each row and have a chance of it being pregnant
        row <- early.gestation[i, ]
        type <- ifelse(row$pentype=="Sow gestation", "Sows", "Gilts")
        animal.sum <- row$npigs
        not.pregnant <- sum(rbinom(animal.sum, 1, abortion.chance))
        ## part of the non-pregnant animals go back to breeding and part are removed from herd
        removed <- sum(rbinom(not.pregnant, 1, 0.5))
        back <- not.pregnant - removed
        ## generating events for the animals that are going to be removed
        if (removed > 0) {
            if (type == "Sows") {
                event1 <- event(type = 14,
                                time = result$time[1],
                                node = row$node,
                                dest = 0,
                                n = removed,
                                proportion = 0)

                events <- rbind(events, event1)
            }
            if (type == "Gilts") {
                event1 <- event(type = 15,
                                time = result$time[1],
                                node = row$node,
                                dest = 0,
                                n = removed,
                                proportion = 0)

                events <- rbind(events, event1)
            }
        }
        ##  rest will returnn back from gestation to breeding (type 1 for sows, 5 for gilts)
        if (back > 0){
            timer.set <- FALSE
            ## attempt to merge the animals with highest timer breeding buffer pen if there is any with animals
            if (type == "Sows"){
                ## collecting buffer pens with animals in them but are not full
                buffer.pens <- result[result$pentype == "Sow breeding buffer" &
                                          result$npigs != 0 &
                                          result$npigs < result$capacity,]
                if (nrow(buffer.pens) > 0) {
                    ## order the pens with highest countdown first
                    buffer.pens <- buffer.pens[order(buffer.pens$countdown, decreasing = TRUE),]
                    ## which pens will have enough space for the sows which aborted
                    space <- (pen_capacity("Sow breeding buffer") - (buffer.pens$npigs + back)) >= 0
                    ## nodes of the pens with space
                    space.pen.id <- buffer.pens[space, "node"]
                    ## take the first pen
                    dest <- space.pen.id[1]
                }
                ## if there are no breeding buffer pens with animals in them, take an empty pen
                else{
                    total.buffer.pens <- ceiling(back / pen_capacity("Sow breeding buffer"))
                    empty.sow.buffer.pens <- empty.pens(result, "Sow breeding buffer", countdown_check = FALSE)
                    ## Check that there are enough pens in the buffer and throw error if not
                    stopifnot(length(empty.sow.buffer.pens) >= total.buffer.pens)
                    dest <- empty.sow.buffer.pens[seq_len(total.buffer.pens)]
                    timer.set <- TRUE
                }
                ## schedule events
                event1 <- event(type = 1,
                                time = result$time[1],
                                node = row$node,
                                dest = dest,
                                n = back,
                                proportion = 0)

                events <- rbind(events, event1)
                ## only set timer for pens that were previously empty
                if(timer.set){
                    result$countdown[result$node %in% dest] <- timers("Breeding buffer", biweekly = biweekly)
                }

            }
            ## hangle the gilts in similar way: try to merge ~half of the aborted gilts with gilts in breeding buffer with highest timer
            ## if there are no animals within than pentype, find a new pen
            if (type == "Gilts"){
                buffer.pens <- result[result$pentype == "Gilt breeding buffer" &
                                          result$npigs != 0 &
                                          result$npigs < result$capacity,]
                if (nrow(buffer.pens) > 0) {
                    ## collecting buffer pens with animals in them
                    buffer.pens <- result[result$pentype == "Gilt breeding buffer" & (result$npigs!= 0),]
                    ## order the pens with highest countdown first
                    buffer.pens <- buffer.pens[order(buffer.pens$countdown, decreasing = TRUE),]
                    ## which pens will have enough space for the sows which aborted
                    space <- (pen_capacity("Gilt breeding buffer") - (buffer.pens$npigs + back)) >= 0
                    ## nodes of the pens with space
                    space.pen.id <- buffer.pens[space, "node"]
                    ## take the first pen
                    dest <- space.pen.id[1]
                }
                else{
                    total.buffer.pens <- ceiling(back / pen_capacity("Gilt breeding buffer"))
                    empty.gilt.buffer.pens <- empty.pens(result, "Gilt breeding buffer", countdown_check = FALSE)
                    ## Check that there are enough pens in the buffer and throw error if not
                    stopifnot(length(empty.gilt.buffer.pens) >= total.buffer.pens)
                    dest <- empty.gilt.buffer.pens[seq_len(total.buffer.pens)]
                    timer.set <- TRUE
                }
                ## schedule the events
                event1 <- event(type = 5,
                                time = result$time[1],
                                node = row$node,
                                dest = dest,
                                n = back,
                                proportion = 0)

                events <- rbind(events, event1)

                if(timer.set){
                    result$countdown[result$node %in% dest] <- timers("Breeding buffer")
                }
            }
        }
    }

    return(list(result = result,
                residual = residual,
                events = events))
}
