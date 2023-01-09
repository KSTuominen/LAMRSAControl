library(LAMRSAControl)

## Timers set to 34 and add some pigs
result <- create_u0(node = TRUE, time = TRUE)
result[result$pentype == "Farrowing", "countdown"][1:22] <- 34
result[result$pentype == "Farrowing", "Spiglets"][1:22] <- 13

result <- clean_trajectory(result)
events <- NULL

## Expect to move all the pigs
stopifnot(sum(cross_foster(result, prop = 1)$events$n) == sum(result$Spiglets))

## expect to move 10% of the pigs (has been tested with 10000 samples
## to make sure it gives the right prop)
set.seed(4)
ob <- round(mean(unlist(lapply(1:100, function(x) {sum(cross_foster(result, prop = 0.1)$events$n)}))))
ex <- round(0.1 * sum(result$Spiglets))
stopifnot(identical(ob, ex))

## expect to have no events
set.seed(4)
ob <- cross_foster(result, prop = 0)$events
stopifnot(is.null(ob))

## expect to have no events
set.seed(4)
result[result$pentype == "Farrowing", "countdown"][1:22] <- 30
ob <- cross_foster(result, prop = 0.1)$events
stopifnot(is.null(ob))
