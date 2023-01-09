##' gen_tests
##'
##' Generate events for disease testing of sows or growing gilts.
##' Sows are tested before they are moved from farrowing to breeding.
##' Growing gilts are tested before they are moved from gilt growing
##' unit to gilt breeding.
##'
##' @param events dataframe, consists of generated movement events
##' @param weaning boolean, generate testing events for sows at weaning
##' @param gilts boolean, generate testing events for growing gilts before moving to breeding
##' @param time integer, timepoint when we want the testing events to start
##'
##' @return An events dataframe where testing events are merged with old events.
##' @export
##'
gen_tests <- function(events,
                      weaning = TRUE,
                      gilts = TRUE,
                      time = 1) {

    if (!weaning && !gilts) {
        stop("Expect at least one of weaning or gilts to be TRUE")
    }

    time_range <- range(events$time)
    stopifnot(time %in% seq(time_range[1], time_range[2]))

    stopifnot(length(time) == 1L)

    ## subset events based on start time
    events <- events[events$time >= time, ]

    test_events <- NULL

    if (weaning) {
        ## sow test events are scheduled based on the time of weaning
        ## collect events when piglets are born
        farr <- events[events$event == "enter" & events$select == 3,]

        # keep only necessary columns for scheduling new events
        # also change the time 2 days before weaning (timer for farrowing unit is total 35 days)
        farr_pens <- farr[, 2:3]
        farr_pens$time <- farr_pens$time + 33

        event1 <- event(type = 24,
                        time = farr_pens$time,
                        node = farr_pens$node,
                        dest = 0,
                        n = 1,
                        proportion = 0)
        event2 <- event(type = 25,
                        time = farr_pens$time + 2,
                        node = farr_pens$node,
                        dest = 0,
                        n = 0,
                        proportion = 1)

        test_events <- rbind(event1, event2)
    }

    if (gilts) {
        ## the nodes of gilt growing pens
        gpens <- which(create_u0()$pentype == "Gilt growing")

        ## pick the events where gilts are moved to breeding
        gg <- events[events$event == "extTrans" & events$select == "2" & events$shift == "0" & events$node %in% gpens, ]
        ## testing whole pen two days before the movement
        gg$time <- gg$time - 2

        ## for each timepoint, choose unique pens to produce only one testing event per pen
        gg_sub <- gg[!duplicated(gg), ]

        gtest_events <- NULL

        ## generate the testing events
        event1 <- event(type = 24,
                        time = gg_sub$time,
                        node = gg_sub$node,
                        dest = 0,
                        n = 1,
                        proportion = 0)

        event2 <- event(type = 25,
                        time = gg_sub$time + 2,
                        node = gg_sub$node,
                        dest = 0,
                        n = 0,
                        proportion = 1)

        test_events <- rbind(event1, event2, test_events)

        ## add the generated events to the original events and sort them by time
        return(rbind(events, test_events))
    }

    rbind(events, test_events)
}
