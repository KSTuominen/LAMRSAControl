##' initialize_herd
##'
##' In this function we add gilts to the breeding at weekly intervals
##' in the beginning of the study to startup to herd.
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
##' @param gilt_introduction Change how many times gilts are introduced
##' (based on gilt_interval). Affects the end herd size.
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
##' @export
initialize_herd <- function(result,
                            residual = NULL,
                            events = NULL,
                            biweekly = FALSE,
                            gilt_introduction = 21) {
    gilt_introduction <- gilt_introduction
    gilt_interval <- 7
    if (biweekly) {
        gilt_introduction <- 10
        gilt_interval <- 14
    }

    gilts.in <- (seq_len(gilt_introduction) * gilt_interval + 1)

    if(result$time[1] %in% gilts.in) {
        ## finding empty destination pen
        empty.gilt.breeding <- empty.pens(result, "Gilt breeding", events)[1]

        ## if there is none break out
        if(is.na(empty.gilt.breeding)) {
            return(list(result = result,
                        residual = residual,
                        events = events))
        }

        ## type 16 generates susceptible gilts and 20 infected
        event1 <- event(type = 16,
                        time = result$time[1],
                        node = empty.gilt.breeding,
                        dest = 0,
                        n = 22,
                        proportion = 0)

        result$countdown[result$node %in% empty.gilt.breeding] <- timers("Breeding", biweekly = biweekly)

        events <- rbind(events, event1)
    }

    return(list(result = result,
                residual = residual,
                events = events))
}

##' empty.pens
##'
##' A function that identifies empty pens of a given class.  main
##' functionality has been moved to the sectioning-functions below,
##' which has the same functionality but takes the sections into
##' account.  Currently used for moving animals from farrowing to
##' breeding because we group up animals from one pen to one pen.
##'
##' @param x A dataframe which is the output from the trajectory of
##'     the MRSA model from 1 day.
##' @param pentype A string
##' @param events The queued events for this timestep
##' @param countdown_check If TRUE check the countdown timer is 99999
##' @return a vector of pen ids
##' @export
empty.pens <- function(x,
                       pentype = c("Sow breeding",
                                   "Gilt breeding",
                                   "Sow breeding buffer",
                                   "Gilt breeding buffer",
                                   "Sow gestation",
                                   "Gilt gestation",
                                   "Farrowing",
                                   "Growing",
                                   "Growing buffer",
                                   "Finishing",
                                   "Gilt growing"),
                       events = NULL,
                       countdown_check = TRUE){
    ## Check arguments
    pentype <- match.arg(pentype)
    stopifnot(identical(class(x), c("MRSA_single_step_trajectory", "data.frame")))

    ## Which pens are the class you are interested in:
    pens_of_class <- x[, "pentype"] == pentype

    ## Which pens are empty:
    emptypens <- x$npigs == 0

    ## which pens have timer 99999 meaning that they are truly available
    counter.check <- x[, "countdown"] == "99999"

    ## Which node ids are both empty and your class of interest.
    if (countdown_check) {
        x <- x[ ,"node"][pens_of_class & emptypens & counter.check]
    } else {
        x <- x[ ,"node"][pens_of_class & emptypens]
    }

    ## Remove those ids that are in the booked list how to return
    ## those that are in the same section? Should we assume there are
    ## enough pens?
    blocked <- c(events$dest, events$node[events$event == "enter"])
    x[!(x %in% blocked)]
}
