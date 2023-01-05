##' farrowing
##'
##' In this function we move the sows into the farrowing room and
##' generate piglet (birth events).
##'
##' @param result The result from the previous day or modified result
##'     from the current day.
##' @param residual The rows with timer == 0 that still need to be
##'     processed. The default is NULL if there is nothing to be done
##' @param events The events that have already been generated. The
##'     default is NULL in the case that no events have yet been
##'     generated for this day
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @export
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
farrowing <- function(result,
                      residual = NULL,
                      events = NULL,
                      biweekly = FALSE) {

    ## collecting all the gestation pens with countdown == 0 that have animals
    index <- (residual$pentype == "Sow gestation" | residual$pentype == "Gilt gestation")

    if (sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    pens.to.farrowing <- residual[index,]

    ## how many animals to farrowing in total
    total.to.farrow <- sum(pens.to.farrowing$npigs)

    gestation.pen.id <- unlist(mapply(rep,
                                      times = pens.to.farrowing$npigs,
                                      x = pens.to.farrowing$node))

    ## finding an empty section and return the node list
    empty.farrow.pens <- sectioning(result, "Farrowing", 0, events)

    ## stop if there are not enough pens in the farrowing room. There
    ## are several checks that determine what to do with extra sows
    ## and log them. Let's just throw an error for now.
    if(length(empty.farrow.pens) < total.to.farrow){
        excess <- total.to.farrow - length(empty.farrow.pens)
        culled <- paste("D", residual$time[1], ":", excess, "animal(s) culled because we ran out of farrowing pens in a section")
        warning(culled)
        ## sample which animals we are going to cull
        pen.id <- pens.to.farrowing$node
        remove <- sample(pen.id, excess, replace = TRUE)
        culled.row <- residual[residual$node %in% remove,]

        ## define the type for the exit event based on the pentype
        type <- rep(14, length(remove))
        rem.type <- pens.to.farrowing$pentype[match(remove,
                                                    pens.to.farrowing$node)] == "Gilt gestation"
        type[rem.type] <- 15

        event1 <- event(type = type,
                        time = result$time[1],
                        node = remove,
                        dest = 0,
                        n = 1,
                        proportion = 0)
        events <- rbind(events, event1)

        ## remove the culled animals from the gestation.pen.id vector
        gestation.pen.id <- unlist(sapply(union(gestation.pen.id, remove), function(i)
            rep(i, each = length(gestation.pen.id[gestation.pen.id == i]) - length(remove[remove == i]))))
        total.to.farrow <- length(gestation.pen.id)
    }

    ## Crop the vector of empty pens to the length of pens we need if
    ## we need less pens than the section capacity
    empty.farrow.pens <- empty.farrow.pens[seq_len(total.to.farrow)]

    ## Sort out what event type we want for each movement
    type <- rep(2, length(gestation.pen.id))
    gt <- pens.to.farrowing$pentype[match(gestation.pen.id,
                                          pens.to.farrowing$node)] == "Gilt gestation"
    type[gt] <- 6

    event1 <- event(type = type,
                    time = result$time[1],
                    node = gestation.pen.id,
                    dest = empty.farrow.pens,
                    n = 1,
                    proportion = 0)

    ## Piglets are born, currently on a same day as sows arrive
    event2<- event(type = 7,
                   time = result$time[1],
                   node = empty.farrow.pens,
                   dest = 0,
                   n = littersize(total.to.farrow),
                   proportion = 0)

    events <- rbind(events, event1, event2)

    ## setting timers for the destination pens
    result$countdown[result$node %in% empty.farrow.pens] <- timers("Farrowing")
    ## timer for the pens to be empty before new animals
    result$countdown[result$node %in% gestation.pen.id] <- timers("Gestation dt")

    residual <- residual[!index, ]

    return(list(result = result,
                residual = residual,
                events = events))
}

##' littersize
##'
##' sampling the littersizes with poisson distribution
##' poisson is used instead of binomial distribution to better reflect the chance of getting littersizes outsife of SD
##'
##' @param litters an integer for the amount of litters needed
##' @param size an integer lambda for poisson distribution (size of litter)
##' @export
##' @return a vector of integers
littersize <- function(litters,
                       size = 14.8) {
    rpois(litters, size)
}
