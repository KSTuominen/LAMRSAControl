##' cross_foster
##'
##' @param result the result object
##' @param residual the residual object
##' @param events the events
##' @param prop the proportion to mix
##' @param day The day for the crossfostering. This is the day fo the
##'     countdown timer in the farrowing room. ie 34 is the first day
##'     in the farrowing room and 33 is the second day. If used in
##'     conjunction with mortality events then the mixing and
##'     mortality should not be scheduled on the same day.
##' @importFrom stats rpois
##' @return events
##' @export
cross_foster <- function(result,
                         residual = NULL,
                         events = NULL,
                         prop = 0.1,
                         day = 34) {

    ## gather pens that have "newborn" piglets
    newborn <- result[(result$pentype == "Farrowing") &
                      (result$Spiglets !=0 | (result$Ipiglets1 + result$Ipiglets2 + result$Ipiglets3) !=0) &
                      (result$countdown == day),]

    if (nrow(newborn) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    sections <- unique(newborn$section)

    event1 <- do.call("rbind", lapply(sections, function(x) {

        piglets <- newborn[newborn$section == x, c("Spiglets", "Ipiglets1", "Ipiglets2", "Ipiglets3")]
        pigs.per.pen <- rowSums(piglets)
        pens <- newborn$node[newborn$section == x]
        total.pigs <- sum(pigs.per.pen)
        total.mingle <- rbinom(1, total.pigs, prop)

        ## don't try to move more pigs than the total
        if(total.mingle > total.pigs) total.mingle <- total.pigs

        ## if none then skip to next section
        if(total.mingle == 0) return(NULL)

        ## generate the source pens
        source_pens <- sample(unlist(mapply(rep,
                                            times = pigs.per.pen,
                                            x = names(pigs.per.pen))),
                              total.mingle,
                              replace = FALSE)

        ## Sample the dest pens one at a time to avoid a movement from a
        ## source pen to the same pen
        dest <- sapply(source_pens, function(x) {
            sample(pens[pens != x], 1)
        })

        ## Generate the events
        event(type = 21,
              time = result$time[1],
              node = source_pens,
              dest = dest,
              n = 1,
              proportion = 0)
    }))

    events <- rbind(events, event1)

    return(list(result = result,
                residual = residual,
                events = events))
}

##' finishing_mingle
##'
##' @param result the result object
##' @param residual the residual object
##' @param events the events
##' @param prop the proportion to mix
##' @param day the day the mixing should occur. This is in the scale
##'     of the countdown timer in the finisher. The first day in the
##'     finisher is 98, which is the default argument.
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @return events
##' @export
finishing_mingle <- function(result,
                             residual = NULL,
                             events = NULL,
                             prop = 1.0,
                             day = 98,
                             biweekly = FALSE) {

    ## gather pens that have "new" finishing pigs
    new_in <- result[(result$pentype == "Finishing") & result$npigs > 0 & (result$countdown == day), ]

    if (nrow(new_in) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }
    sections <- unique(new_in$section)

    ## make a vector of each animal in a pen and shuffle that?
    event1 <- do.call("rbind", lapply(sections, function(x) {

        pigs.per.pen <- new_in[new_in$section == x, "npigs"]
        total_finishers <- sum(pigs.per.pen)
        pens <- new_in$node[new_in$section == x]
        total_pens <- length(pens)
        events_section <- NULL

        ## because the capacity is smaller, add some extra pens
        ## how many new pens we need
        needed_pens <- total_finishers/pen_capacity("Finishing")
        balance <- needed_pens - total_pens

        if(balance > 0){
            ## pens that are still empty in the section
            empty <- result[result$pentype == "Finishing" & result$section == x & result$npigs == 0, "node"]
            ## do nothing if there are no free pens (means that we are overfilling pens)
            if(length(empty) > 0){
                ##  take only the empty pens we need, if not, take what is available
                if(balance <= length(empty)){
                    empty <- empty[1:length(balance)]
                }
            }
            ## append the extra pens to pens-variable to get more destination pens
            dest_pens <- c(pens, empty)

        }

        ## if we don't add pens, use the departure pens also as destination pens
        else{
            dest_pens <- pens
        }

        ## Note: we assume pens are in order of their index already. This is
        ## important since the events in SimInf are processed in order of
        ## node then destination we are taking care of the scaling of
        ## proportion of pigs to move out of each pen correctly along the
        ## destination pens but not adjusting for the fact that the pens
        ## are also receiving pigs. These received pigs should not be
        ## among those that are moved out when we come to that pen. For
        ## this to work we must assume a uniform population size in each
        ## node before the operation starts. This may not truely be the
        ## case but on average may be true since everything is based on
        ## littersize which is sampled from a Poisson. Below, the line
        ## marked with 'Note' will gradually reduce the proportion of pigs
        ## moved out of the pen as we move along the source pens
        ## vector. If there are 10 pens to move out of then the first pen
        ## will have prop_i == prop the next pen will have prop_i =
        ## prop/(1 + ((1/10) * 1)) the third prop/(1 + ((1/10) * 2)) and
        ## so on.

        ## generate the events
        for(i in seq_len(total_pens)) {

            prop_i <- prop / (1 + ((1 / length(dest_pens)) * (i - 1))) ## Note

            ## split the proportion over destination pens
            p <- prop_i / length(dest_pens)

            ## Scale the p to the order to the dest pen ID
            scaled_p <- scale_p(dest_pens, rep(p, length(dest_pens)))

            ## create the events:
            event_pen <- event(type = 22,
                               time = result$time[1],
                               node = pens[i],
                               dest = dest_pens,
                               n = 0,
                               proportion = scaled_p)

            events_section <- rbind(events_section, event_pen)
        }
        events_section

    }))
    events <- rbind(events, event1)

    ## set the timer for the newly occupied pens to same as with previously occupied pens
    result$countdown[result$node %in% event1$dest] <- timers("Finishing", biweekly = biweekly) - 1

    return(list(result = result,
                residual = residual,
                events = events))
}
