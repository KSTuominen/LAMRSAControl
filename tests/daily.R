library(LAMRSAControl)
model <- MRSA_model()
result <- create_u0(node = TRUE,
             time = TRUE,
             phi = TRUE,
             s_phi = TRUE,
             f_phi = TRUE,
             test_indicator = TRUE,
             result_indicator = TRUE,
             cleaning_indicator = TRUE,
             Icum = TRUE,
             npigs = TRUE)
result <- clean_trajectory(result)
events <- NULL
result <- list(result=result, events = events)
for(tspan in 1:100) {
    result <- daily(result$result,
                    model,
                    tspan)
    events <- rbind(events,
                    result$events)
    cat(tspan, "\n")
}

## Are some events generated?
stopifnot(length(unique(events$time)) > 10)
