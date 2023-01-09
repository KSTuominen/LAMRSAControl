library(LAMRSAControl)

result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

## add some grower pigs
result[result$pentype == "Growing buffer", "countdown"][1:3] <- 0
result[result$pentype == "Growing buffer", "Sgrowers"][1:3] <- 3
result$npigs <- result$Sgrowers
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]

## Expect the no events when running growing to finisher.
ob <- growing_to_finishing(result, residual, fraction_to_buffer = 0)
stopifnot(is.null(ob$events))

## Expect events to move animals to finisher pens when no finishers
## are present.
ob <- growing_buffer_to_finishing(result, residual)
stopifnot(nrow(ob$events) == 3L)

## check the countdown was set appropriately
stopifnot(all(ob$result[ob$result$pentype == "Growing buffer", "countdown"][1:3] == timers("Growing buffer dt")))
stopifnot(all(ob$result[ob$result$pentype == "Finishing", "countdown"][1:3] == timers("Finishing")))

## add some finishers and make sure it still works.
result[result$pentype == "Finishing", "countdown"][1:3] <- 45
result[result$pentype == "Finishing", "Sfinish"][1:3] <- 3
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect events to move animals to finisher pens when finishers are
## present.
ob2 <- growing_buffer_to_finishing(result, residual)
stopifnot(nrow(ob2$events) == 3L)
## none of these destination pens should be the same as the first
## because we filled them with animals
stopifnot(all(!(ob2$events$dest %in% ob$events$dest)))

## check the countdown was set appropriately
stopifnot(all(ob2$result[ob2$result$pentype == "Growing buffer", "countdown"][1:3] == timers("Growing buffer dt")))
stopifnot(all(ob2$result[ob2$result$pentype == "Finishing", "countdown"][4:6] == 45))
