library(LAMRSAControl)

## Timers set to 14
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Sow breeding", "countdown"][1] <- 0
result[result$pentype == "Sow breeding", "Ssows"][1] <- 8
result$npigs <- result$Ssows
result <- clean_trajectory(result)
residual <- result[result$pentype == "Sow breeding" & result$countdown == 0, ]

## Two possibilities here. Either gilts all go to gestation
set.seed(6)
ob <- sow_to_gestation(result, residual)$events
stopifnot(identical(length(table(ob$dest)), 1L))
## Or some also go to the buffer because they were nonpregnant
set.seed(7)
ob <- sow_to_gestation(result, residual)$events
stopifnot(identical(length(table(ob$dest)), 2L))
