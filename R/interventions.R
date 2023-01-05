#' Interventions
#'
#' @param intervention string, which intervention model to prepare
#' @param events pre-generated events
#' @param time timeseries for model run
#' @param proportion proportion of gilts to be infected
#' @param clean_AIAO alternative infectious pressure after cleaning, all-in-all-out pens
#' @param clean_CF alternative infectious pressure after cleaning, continuous flow pens
#' @param test_weaning if TRUE, test sows at the time of weaning
#' @param test_gilts if TRUE, test new gilts in the gilt unit before moving them to breeding
#' @param Se The sensitivity of the test applied in the sows and gilts
#' @param room_scale between_pen spread within a room
#' @param farm_scale between room spread within the herd
#'
#' @return a model object
#' @export

interventions <- function(intervention = "basemodel",
                          events,
                          proportion = 0.2,
                          time = 731:3000,
                          clean_AIAO = 1.0,
                          clean_CF = 1.0,
                          test_weaning = FALSE,
                          test_gilts = FALSE,
                          Se = 0.7,
                          room_scale = 0.1,
                          farm_scale = 0.1) {
    stopifnot(intervention %in% c("basemodel", "mixing", "cleaning", "testing", "biweekly"))
    model <- MRSA_model_4_parameter(Se = Se,
                                    room_scale = room_scale,
                                    farm_scale = farm_scale)

    inf_time <- time[1]
    model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < inf_time, ])
    model@tspan <- as.double(1:(inf_time-1))
    result <- trajectory(run(model))

    ## uncleaned trajectory of one day (needed for generating cleaning events)
    result_uncleaned <- result[result$time == inf_time-1, ]

    ## new result df to work as u0 for infecting animals
    result_clean <- clean_trajectory(result_uncleaned)

    #events starting from day 731 and infected gilts
    inf_events <- infect_pigs(result_clean, pentype = "Gilt growing", time = inf_time, proportion = proportion)
    events_I <- rbind(inf_events, events[events$time >= inf_time,])

    model@events <- SimInf_events(model@events@E, model@events@N, events = events_I)
    model@tspan <- as.double(time)
    model@u0 <- as_u_matrix(result_clean)

    ## the base model, biweekly model and different mixing scenarios only require different events to be run
    ## therefore only "base" model object with infected animals is returned
    if (intervention %in% c("basemodel", "biweekly", "mixing")) {
        return(model)
    }

    if (intervention == "cleaning") {
        model_cleaning <- cleaning(model = model,
                                   result = result_uncleaned,
                                   time = time,
                                   inf_time = inf_time,
                                   events = events_I,
                                   clean_AIAO = clean_AIAO,
                                   clean_CF = clean_CF)
        return(model_cleaning)
    }

    if (intervention == "testing") {
        model_testing <- disease_testing(model = model,
                                         events = events_I,
                                         time = inf_time,
                                         test_weaning = test_weaning,
                                         test_gilts = test_gilts)
        return(model_testing)
    }


}

#' Cleaning
#'
#' run the model with premade events with different cleaning regimes
#'
#' @param model model object
#' @param result dataframe. "non-cleaned" trajectory of the model
#' @param time A time series
#' @param inf_time Time when gilts were infected
#' @param events pregenerated events with infected animals
#' @param clean_AIAO alternative infectious pressure after cleaning,  all-in-all-out pens
#' @param clean_CF alternative infectious pressure after cleaning, continuous flow pens
#'
#' @return  model object
#' @export
cleaning <- function(model, result, time, inf_time, events, clean_AIAO, clean_CF) {
    ## check that we have changed at least one of the default values to do some cleaning
    stopifnot((clean_AIAO != 1.0 | clean_CF != 1.0))

    ## generate the continuous flow events based on given events
    if (clean_CF != 1.0) {
        events <- clean_cf(result = result, events = events, time = inf_time)
    }

    model@events <- SimInf_events(model@events@E, model@events@N, events = events)
    model@tspan <- as.double(time)
    model@u0 <- as_u_matrix(clean_trajectory(result))
    model@gdata["cleaning_effect_AIAO"] <- clean_AIAO
    model@gdata["cleaning_effect_CF"] <- clean_CF
    model
}

#' Disease testing
#'
#' @param model model object
#' @param events dataframe of base events with infected animals
#' @param time timepoint (single value) when we want testing events to start
#' @param test_weaning if TRUE, test sows at the time of weaning
#' @param test_gilts if TRUE, test new gilts in the gilt unit before moving them to breeding
#'
#' @return list of result dataframes for different testing scenarios
#' @export
disease_testing <- function(model, events, time, test_weaning, test_gilts) {
    testing <- gen_tests(events, weaning = test_weaning, gilts = test_gilts, time = time)
    model@events <- SimInf_events(model@events@E, model@events@N, events = testing)
    model
}
