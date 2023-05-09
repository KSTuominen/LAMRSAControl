##' daily
##'
##' This function runs once per day to generate the day's events
##'
##' @param result The result object
##' @param model The model object
##' @param tspan The time vector
##' @param crossfostering_day The day to perform crossfostering. The
##'     default is day 34. This both prevents mortality on that day
##'     and does the crossfostering on that day.
##' @param crossfostering_proportion The proportion of piglets to be crossfostered
##' @param finisher_mixing_proportion The proportion of finisher pigs to be mixed
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @param gilt_introduction Change how many times gilts are introduced
##' (based on gilt_interval). Affects the end herd size.
##' @export
##' @importFrom SimInf SimInf_events
##' @importFrom SimInf trajectory
##' @importFrom SimInf run
##' @return An object for the day
daily <- function(result,
                  model,
                  tspan,
                  crossfostering_day = 34,
                  crossfostering_proportion = 0.1,
                  finisher_mixing_proportion = 1,
                  biweekly = FALSE,
                  gilt_introduction = 21) {
    ## step the counter
    stopifnot(gilt_introduction <= 21)
    result$countdown <- as.integer(ifelse(result$countdown == 0 | result$countdown == 99999,
                                          result$countdown,
                                          result$countdown - 1))
    ## check that we don't have timer 99999 and animals in a pen at the same time
    null_timer <- result$countdown == 99999
    non_empty_pen <- result[, "npigs"] != 0
    stopifnot(!any(null_timer & non_empty_pen))

    ## Remove the events from the model object and set the tspan
    model@events <- SimInf_events(model@events@E, model@events@N, NULL)
    model@tspan <- as.numeric(tspan)

    ## Set the countdown to 99999 if there are no animals but the
    ## timer reached zero. This happens when the downtime in a pen
    ## expires.
    result[result$countdown == 0 & nanimals(result) == 0, "countdown"] <- 99999

    ## Get the rows that we need to work with 'today'
    residual <- result[result$countdown == 0,]

    ## Initialize the herd with gilts to breeding
    ob <- initialize_herd(result,
                          residual,
                          biweekly = biweekly,
                          gilt_introduction = gilt_introduction)

    ob <- abortion(ob$result,
                   ob$residual,
                   ob$events,
                   biweekly = biweekly)

    ob <- piglet_mortality(ob$result,
                           ob$residual,
                           ob$events,
                           skip_days = crossfostering_day)

    ob <- cross_foster(ob$result,
                       ob$residual,
                       ob$events,
                       day = crossfostering_day,
                       prop = crossfostering_proportion)

    ob <- finishing_mingle(ob$result,
                           ob$residual,
                           ob$events,
                           prop = finisher_mixing_proportion)

    ob <- finishing_slaughter(ob$result,
                              ob$residual,
                              ob$events,
                              biweekly = biweekly)

    ## Start a series of events the while control flow just allows you
    ## to break out anytime you don't have anything left to do or you
    ## get to the end of the control flow
    while(TRUE) {
        if(nrow(residual) == 0) break

        ob <- weaning(ob$result,
                      ob$residual,
                      ob$events,
                      biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- growing_to_finishing(ob$result,
                                   ob$residual,
                                   ob$events,
                                   biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- growing_buffer_to_finishing(ob$result,
                                          ob$residual,
                                          ob$events,
                                          biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- farrowing(ob$result,
                        ob$residual,
                        ob$events,
                        biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- gilt_to_gestation(ob$result,
                                ob$residual,
                                ob$events,
                                biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- sow_to_gestation(ob$result,
                               ob$residual,
                               ob$events,
                               biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- gilts_to_slaughter(ob$result,
                                 ob$residual,
                                 ob$events,
                                 biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ob <- buffer_culling(ob$result,
                             ob$residual,
                             ob$events,
                             biweekly = biweekly)
        if(nrow(ob$residual) == 0) break

        ## If we still have rows left in the residual that we have not
        ## taken care of, we have a problem.
        stopifnot(nrow(ob$residual) == 0)
    }

    ## Finally run the trajectory and return the result
    model@u0 <- as_u_matrix(ob$result)
    model@v0 <- as_v_matrix(ob$result)

    model@events <- SimInf_events(model@events@E, model@events@N, ob$events)
    result <- trajectory(run(model))
    return(list(result = clean_trajectory(result), events = ob$events))
}
