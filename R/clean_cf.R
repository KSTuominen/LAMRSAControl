#' clean_cf
#'
#' Generates the events for cleaning for continuous flow pens
#' based on weaning (happens currectly every 7 days)
#'
#' @param result dataframe of the herd structure (from one day)
#' @param events dataframe of pre-generated base events
#' @param time integer, time when we want the cleaning events to start
#'
#' @return dataframe, merged from base events and added cleaning events
#' @export

clean_cf <- function(result,
                   events,
                   time = 1) {

    time_range <- range(events$time)
    stopifnot(time %in% time_range[1]:time_range[2])

    events <- events[events$time >= time, ]

    ## collect time points when weaning happens
    wean <- unique(events[events$event == "extTrans" & events$select == 4 & events$shift == 2, "time"])
    ## do the cleaning one day earlier than weaning
    wean <- wean-1

    ## collect pens we want to clean
    clean_pens <- result[result$pentype %in% c(1:6, 11), "node"]

    clean_events <- NULL

    for (i in 1:length(wean)){
        time <- wean[i]
        event1 <- event(type = 26,
                        time = time,
                        node = clean_pens,
                        dest = 0,
                        n = 1,
                        proportion = 0)

        clean_events <- rbind(clean_events, event1)
    }

    ## add the generated events to the original events and sort them by time

    merge_events <- rbind(events, clean_events)
    merge_events <- merge_events[order(merge_events$time),]
    merge_events

}
