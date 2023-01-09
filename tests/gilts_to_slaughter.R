library(LAMRSAControl)

## Timers set to 14
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Gilt growing", "countdown"][1] <- 0
result[result$pentype == "Gilt growing", "Sgilts"][1] <- 8
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
residual <- result[result$pentype == "Gilt growing" & result$countdown == 0, ]


## Make sure an event is scheduled
ob <- gilts_to_slaughter(result, residual)
stopifnot(nrow(ob$events) == 1)
stopifnot(ob$events$proportion == 1)
stopifnot(ob$events$select == 2)
stopifnot(ob$events$shift == 0)

## Make sure the timer is set
stopifnot(identical(ob$result[1081, "countdown"], 6))
