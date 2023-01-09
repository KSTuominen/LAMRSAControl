library(LAMRSAControl)

## Timers set to 8 and add some pigs
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Farrowing", "countdown"][1:22] <- 8
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- 13
result$npigs <- result$Spiglets
result <- clean_trajectory(result)
events <- NULL

## Expect to scheduel events to remove 1% of the pigs
set.seed(2)
stopifnot(identical(round(sum(piglet_mortality(result, prob = 0.01)$events$n)/
     sum(result$Spiglets), 2), 0.01))

## Expect to scheduel events to remove 10% of the pigs
set.seed(9000)
stopifnot(identical(round(sum(piglet_mortality(result, prob = 0.1)$events$n)/
     sum(result$Spiglets), 2), 0.1))

## Expect to scheduel events to remove 90% of the pigs
set.seed(9000)
stopifnot(identical(round(sum(piglet_mortality(result, prob = 0.9)$events$n)/
     sum(result$Spiglets), 2), 0.9))

## Check that not more pigs than are in the pen are removed. We had
## the problem that only the first probability for used.
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Farrowing", "countdown"][1:22] <- 8
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- c(100, 1)
result$npigs <- result$Spiglets
result <- clean_trajectory(result)
set.seed(1)
stopifnot(!(100 %in% piglet_mortality(result, prob = 0.1)$events$node))


## Try to avoid event on a certain day
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$pentype == "Farrowing", "countdown"][1:22] <- 34
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- c(100, 1)
result$npigs <- result$Spiglets
result <- clean_trajectory(result)
set.seed(1)

stopifnot(is.null(piglet_mortality(result, prob = 0.1)$events))

result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$pentype == "Farrowing", "countdown"][1:22] <- 32
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- c(100, 1)
result$npigs <- result$Spiglets
result <- clean_trajectory(result)
set.seed(1)

stopifnot(is.null(piglet_mortality(result, prob = 0.1, skip_days = 32)$events))

result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Farrowing", "countdown"][1:22] <- c(33, 34)
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- c(100, 1)
result$npigs <- result$Spiglets
result <- clean_trajectory(result)
set.seed(1)

stopifnot(is.null(piglet_mortality(result, prob = 0.1, skip_days = c(33, 34))$events))
