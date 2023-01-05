library(LAMRSAControl)

result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
result$npigs <- result$Sgrowers
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]

## Expect the events to only move animals to the finisher, not the buffer
ob <- growing_to_finishing(result, residual, fraction_to_buffer = 0)
stopifnot(nrow(ob$events) == 26)
## Check that the timers are set
stopifnot(all(ob$result[ob$result$pentype == "Growing", "countdown"][1:26] == timers("Growing dt")))
stopifnot(all(ob$result[ob$result$pentype == "Finishing", "countdown"][1:26] == timers("Finishing")))

## When we also move to the buffer then expect the numer of events to
## be 4 times as many and more timers should be set in the growing buffer
ob <- growing_to_finishing(result, residual, fraction_to_buffer = 0.1)
stopifnot(nrow(ob$events) == 26*4)
stopifnot(all(ob$result[ob$result$pentype == "Growing", "countdown"][1:26] == timers("Growing dt")))
stopifnot(all(ob$result[ob$result$pentype == "Growing buffer", "countdown"][1:3] == timers("Growing buffer")))
stopifnot(all(ob$result[ob$result$pentype == "Finishing", "countdown"][1:26] == timers("Finishing")))

## What happens when the finisher barn is full?
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
## add finishers
for (i in 1:18) {
    result[result$pentype == "Finishing" & result$section == i, "countdown"][1:26] <- 1 + 7*i
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][1:26] <- 12
}
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect an error because there is no space left
ob <- tools::assertError(growing_to_finishing(result, residual, fraction_to_buffer = 0))[[1]]$message
ex <- "all\\(!is.na\\(empty.finishing.pens\\)\\) is not TRUE"
stopifnot(length(grep(ex, ob)) == 1L)

## But it should work if there is 1 section free
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
## add finishers
for (i in 1:17) {
    result[result$pentype == "Finishing" & result$section == i, "countdown"][1:26] <- 1 + 7*i
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][1:26] <- 12
}
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect an error because there is no space left
ob <- growing_to_finishing(result, residual, fraction_to_buffer = 0)
stopifnot(nrow(ob$events) == 26)

## What happens if all the finishing sections are occupied by less
## pens then are needed? We expect them not to be available since we
## want a completly empty room:
## But it should work if there is 1 section free
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
## add finishers
for (i in 1:18) {
    result[result$pentype == "Finishing" & result$section == i, "countdown"][1] <- 1 + 7*i
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][1] <- 12
}
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect an error because there is no empty room left
ob <- tools::assertError(growing_to_finishing(result, residual, fraction_to_buffer = 0))[[1]]$message
ex <- "all\\(!is.na\\(empty.finishing.pens\\)\\) is not TRUE"
stopifnot(length(grep(ex, ob)) == 1L)

## A corner case is when we slaughter animals from the finisher we get
## the countdown timer set to 99999 on a few pens with no animals and
## the remainer of the room emptied with a downtime timer of a few
## days because we don't know that those pens were empty. This causes
## the risk of a room being interpreted as empty when it needs
## downtime before being available.
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
## add timers that should render the rooms unavailable
for (i in 1:18) {
    result[result$pentype == "Finishing" & result$section == i, "countdown"][1:26] <- 1 + i
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][1:26] <- 0
    result[result$pentype == "Finishing" & result$section == i, "countdown"][27:30] <- 99999
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][27:30] <- 0

}
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect an error because there is should be no available room
ob <- tools::assertError(growing_to_finishing(result, residual, fraction_to_buffer = 0))[[1]]$message
ex <- "all\\(!is.na\\(empty.finishing.pens\\)\\) is not TRUE"
stopifnot(length(grep(ex, ob)) == 1L)
