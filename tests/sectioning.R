library(LAMRSAControl)

## Timers set to 14
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Gilt breeding", "countdown"][1] <- 0
result[result$pentype == "Gilt breeding", "Sgilts"][1] <- 8
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
residual <- result[result$pentype == "Gilt breeding" & result$countdown == 0, ]

## Don't find enough pens return none
stopifnot(identical(sectioning(result, "Gilt breeding", 8), integer(0)))

## Don't find emtpy section return none
stopifnot(identical(sectioning(result, "Gilt breeding", 0), integer(0)))

## Find enough pens return them and more
stopifnot(length(sectioning(result, "Gilt breeding", 3)) == 7)

## Find empty section and return it and more. We return all the pens
## from sections that have enough space. Do we only want to return the
## first section?
stopifnot(length(sectioning(result, "Farrowing", 5)) == 156)

## Find empty section and return it. We should get none because no
## singel section have 75 pens.
stopifnot(identical(sectioning(result, "Farrowing", 75), integer(0)))

## expect to get just all sections length
stopifnot(length(sectioning(result, "Farrowing", 0)) == 156)

## What happens when the finisher barn is full?
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
## add some grower pigs
result[result$pentype == "Growing", "countdown"][1:26] <- 0
result[result$pentype == "Growing", "Sgrowers"][1:26] <- 12
## add finishers
for (i in 1:18) {
    result[result$pentype == "Finishing" & result$section == i, "countdown"][1:26] <- 1 + i
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][1:26] <- 0
    result[result$pentype == "Finishing" & result$section == i, "countdown"][27:30] <- 99999
    result[result$pentype == "Finishing" & result$section == i, "Sfinish"][27:30] <- 0
}
result$npigs <- result$Sgrowers + result$Sfinish
result <- clean_trajectory(result)
residual <- result[result$countdown == 0, ]
## Expect no pens because there is should be no available rooms
stopifnot(identical(sectioning(result, "Finishing", 0), integer(0)))

## Expected rooms with 4 pens
sectioning(result, "Finishing", 4)

## But not 5
stopifnot(identical(sectioning(result, "Finishing", 5), integer(0)))
