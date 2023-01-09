library(LAMRSAControl)
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Farrowing", "countdown"][1:2] <- 0
result[result$pentype == "Farrowing", "Ssows"][1:2] <- 1
result[result$pentype == "Farrowing", "Spiglets"][1:2] <- 15
result$npigs <- result$Ssows + result$Spiglets
result <- clean_trajectory(result)
residual <- result[result$pentype == "Farrowing" & result$countdown == 0, ]

set.seed(2)
ob <- weaning(result, residual, events = NULL, fraction_to_gilt = 0.2)$events
ex <- structure(list(event = structure(c(1L, 1L, 1L, 1L, 2L, 1L),
                                       .Label = c("extTrans", "exit"),
                                       class = "factor"),
                     time = c(1L, 1L, 1L, 1L, 1L, 1L),
                     node = c(99L, 99L, 100L, 100L, 99L, 100L),
                     dest = c(255L, 1081L, 256L, 1081L, 0L, 1L),
                     n = c(12L, 3L, 12L, 3L, 1L, 1L),
                     proportion = c(0, 0, 0, 0, 0, 0),
                     select = c(4L, 4L, 4L, 4L, 1L, 1L),
                     shift = c(2L, 4L, 2L, 4L, 0L, 0L)),
                row.names = c("3", "1", "4", "2", "11", "12"),
                class = "data.frame")
stopifnot(identical(ob, ex))

## This is broken
## set.seed(7)
## weaning(result, residual, events = NULL, fraction_to_gilt = 0)$events

## With 24 sowss
result <- create_u0(node = TRUE, time = TRUE)

result[result$pentype == "Farrowing", "countdown"][1:24] <- 0
result[result$pentype == "Farrowing", "Ssows"][1:24] <- 1
result[result$pentype == "Farrowing", "Spiglets"][1:24] <- 15
result$npigs <- result$Ssows + result$Spiglets
result <- clean_trajectory(result)
residual <- result[result$pentype == "Farrowing" & result$countdown == 0, ]

set.seed(2)
ob <- weaning(result, residual, events = NULL, fraction_to_gilt = 0.05)$events
## Expect to get piglet movement to gilt grower
ex <- sum(ob$event == "extTrans" & ob$select == 4 & ob$shift == 4) > 0
stopifnot(ex)
## Expect piglets to go to grower
ex <- sum(ob$event == "extTrans" & ob$select == 4 & ob$shift == 2) > 0
stopifnot(ex)
## Expect to get sows to breeding
ex <- sum(ob$event == "extTrans" & ob$select == 1 & ob$shift == 0) > 0
stopifnot(ex)
## expect to get some sows culled
ex <- sum(ob$event == "exit" & ob$select == 1 & ob$shift == 0) > 0
stopifnot(ex)

## ## We can't do this:
## ## With 48 sows in 2 sections
## result <- create_u0(node = TRUE, time = TRUE)
## result[result$pentype == "Farrowing", "countdown"][c(1:24, 27:52)] <- 0
## result[result$pentype == "Farrowing", "Ssows"][c(1:24, 27:52)] <- 1
## result[result$pentype == "Farrowing", "Spiglets"][c(1:24, 27:52)] <- 15
## result <- clean_trajectory(result)
## residual <- result[result$pentype == "Farrowing" & result$countdown == 0, ]

## set.seed(2)
## ob <- weaning(result, residual, events = NULL, fraction_to_gilt = 0.05)$events
## ## Expect to get piglet movement to gilt grower
## ex <- sum(ob$event == "extTrans" & ob$select == 4 & ob$shift == 4) > 0
## stopifnot(ex)
## ## Expect piglets to go to grower
## ex <- sum(ob$event == "extTrans" & ob$select == 4 & ob$shift == 2) > 0
## stopifnot(ex)
## ## Expect to get sows to breeding
## ex <- sum(ob$event == "extTrans" & ob$select == 1 & ob$shift == 0) > 0
## stopifnot(ex)
## ## expect to get some sows culled
## ex <- sum(ob$event == "exit" & ob$select == 1 & ob$shift == 0) > 0
## stopifnot(ex)
