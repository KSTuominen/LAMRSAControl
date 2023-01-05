library(LAMRSAControl)

## Timers set to 14
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Finishing", "countdown"][c(1,2)] <- 14
result[result$pentype == "Finishing", "Sfinish"][1] <- 8
result[result$pentype == "Finishing", "Ifinish"][1] <- 1
result[result$pentype == "Finishing", "Sfinish"][2] <- 10
result$npigs <- result$Sfinish + result$Ifinish

result <- clean_trajectory(result)
ob <- finishing_slaughter(result)$events
## we now expect to get events for the whole room
stopifnot(nrow(ob) == 30L)
stopifnot(ob$proportion == 1/3)

## Timers set to 7
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Finishing", "countdown"][c(1,2)] <- 7
result[result$pentype == "Finishing", "Sfinish"][1] <- 8
result[result$pentype == "Finishing", "Ifinish"][1] <- 1
result[result$pentype == "Finishing", "Sfinish"][2] <- 10
result$npigs <- result$Sfinish + result$Ifinish

result <- clean_trajectory(result)

ob <- finishing_slaughter(result)$events
stopifnot(nrow(ob) == 30L)
stopifnot(ob$proportion == 1/2)

## Timers set to 0
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Finishing", "countdown"][c(1,2)] <- 0
result[result$pentype == "Finishing", "Sfinish"][1] <- 8
result[result$pentype == "Finishing", "Ifinish"][1] <- 1
result[result$pentype == "Finishing", "Sfinish"][2] <- 10
result$npigs <- result$Sfinish + result$Ifinish
result <- clean_trajectory(result)

ob <- finishing_slaughter(result)$result$countdown[541:542]
ex <- c(6, 6)
stopifnot(identical(ob, ex))

ob <- finishing_slaughter(result)$events
stopifnot(nrow(ob) == 30L)
stopifnot(ob$proportion == 1)

## See that animals are not slaughtered when timer is not 14, 7, or  0
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

result[result$pentype == "Finishing", "countdown"][c(1,2)] <- 1
result[result$pentype == "Finishing", "Sfinish"][1] <- 8
result[result$pentype == "Finishing", "Ifinish"][1] <- 1
result[result$pentype == "Finishing", "Sfinish"][2] <- 10
result$npigs <- result$Sfinish + result$Ifinish
result <- clean_trajectory(result)

ob <- finishing_slaughter(result = result)$events
stopifnot(is.null(ob))

## Pens have countdown 0, and they are empty. We still want to
## schedule slaughter events because we can't know the absolute number
## of animals in these pens
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$pentype == "Finishing", "countdown"][c(1,2)] <- 0
result <- clean_trajectory(result)
ob <- finishing_slaughter(result = result)$events
stopifnot(nrow(ob) == 30L)
stopifnot(ob$proportion == 1)

## Check what happens if we traverse the time from 15 days to 0 days
result <- u0(node = TRUE, time = TRUE, npigs = TRUE, test_indicator = TRUE, result_indicator = TRUE, cleaning_indicator = TRUE, Icum = TRUE)
result <- clean_trajectory(result)
index <- which(result$pentype == "Finishing")[c(1, 2)]
result[index, "countdown"] <- 15
result[index, "Sfinish"][1] <- 50000
result[index, "Ifinish1"][1] <- 50000
result[index, "Sfinish"][2] <- 100000
result$npigs <- result$Sfinish + result$Ifinish1

ob <- finishing_slaughter(result)
stopifnot(is.null(ob$events))

## Set timer to 14 We should see that about 1/3 of animals are removed
ob$result[index, "countdown"] <- 14

ob <- finishing_slaughter(ob$result,
                          ob$residual,
                          ob$events)
model <- MRSA_model()
model@u0 <- as_u_matrix(ob$result)
model@events <- SimInf_events(model@events@E, model@events@N, ob$events)
model@tspan <- 1
result <- clean_trajectory(trajectory(run(model)))

## We see that the number of pigs in the pens has reduced (by about a 1/3)
ob1 <- result[index, "npigs"]
stopifnot(all(ob1 < 100000))

## Set timer to 7, we should see about half of the pigs removed
result[index, "countdown"] <- 7

ob <- finishing_slaughter(result,
                          NULL,
                          NULL)

## we want evnets for all the pens at 7 days regardless of how many
## animals are left. This is because when we run the events on the fly
## we don't know which pens could be empty. We therefore expect 2 events:
stopifnot(identical(nrow(ob$events), 30L))

model <- MRSA_model()
model@u0 <- as_u_matrix(ob$result)
model@events <- SimInf_events(model@events@E, model@events@N, ob$events)
model@tspan <- 1
result <- clean_trajectory(trajectory(run(model)))

## We expect that the number of pigs has been further reduced
ob <- result[index, "npigs"]
stopifnot(all(ob < ob1))

## Set timer to 0, we should see all the pigs removed and the
## countdown timers set to the appropriate value
result[index, "countdown"] <- 0

ob <- finishing_slaughter(result,
                          NULL,
                          NULL)

## we want events for all the pens at 0 days regardless of how many
## animals are left. This is because when we run the events on the fly
## we don't know which pens could be empty. We therefore expect 2 events:
stopifnot(identical(nrow(ob$events), 30L))

model <- MRSA_model()
model@u0 <- as_u_matrix(ob$result)
model@events <- SimInf_events(model@events@E, model@events@N, ob$events)
model@tspan <- 1
set.seed(1)
result <- clean_trajectory(trajectory(run(model)))

## We expect that the number of pigs is now 0
stopifnot(identical(result[index, "npigs"], c(0, 0)))

## We also expect all the pen countdowntimers to be set to 6
stopifnot(identical(result[index, "countdown"], c(6L, 6L)))
