library(LAMRSAControl)

## Timers set to 14
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Gilt breeding", "countdown"][1] <- 0
result[result$pentype == "Gilt breeding", "Sgilts"][1] <- 8
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
residual <- result[result$pentype == "Gilt breeding" & result$countdown == 0, ]

## Two possibilities here. Either gilts all go to gestation
set.seed(6)
ob <- gilt_to_gestation(result, residual)$events
stopifnot(identical(length(table(ob$dest)), 1L))
## Or some also go to the buffer because they were nonpregnant
set.seed(7)
ob <- gilt_to_gestation(result, residual)$events
stopifnot(identical(length(table(ob$dest)), 2L))

## Check that the sampling fix works:
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$node == 16, "Sgilts"] <- 1
result[result$node == 16, "countdown"] <- 0
result[result$node == 17, "Sgilts"] <- 1
result[result$node == 17, "countdown"] <- 0
result[result$node == 18, "Sgilts"] <- 1
result[result$node == 18, "countdown"] <- 1
result[result$node == 19, "Sgilts"] <- 1
result[result$node == 19, "countdown"] <- 1
result[result$node == 20, "Sgilts"] <- 10
result[result$node == 20, "countdown"] <- 0
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
residual <- result[result$pentype == "Gilt breeding" & result$countdown == 0, ]
set.seed(20)
gilt_to_gestation(result, residual)
