library(LAMRSAControl)
result <- create_u0(node = TRUE, time = TRUE, npigs = TRUE, test_indicator = TRUE, result_indicator = TRUE, cleaning_indicator = TRUE, Icum = TRUE)

## set up a finisher section that will be mixed
result[result$pentype == "Finishing", "countdown"][1:20] <- 98
result[result$pentype == "Finishing", "Sfinish"][1:5] <- 9
result[result$pentype == "Finishing", "Sfinish"][6:10] <- 10
result[result$pentype == "Finishing", "Sfinish"][11:15] <- 11
result[result$pentype == "Finishing", "Sfinish"][16:20] <- 12
result$npigs <- result$Sfinish
result <- clean_trajectory(result)
residual <- NULL

event1 <- finishing_mingle(result, residual, prop = 1)$events
## Expect the number of events equal to the total number of pens
## squared minus the events from and to the same pen which are not
## included:
stopifnot(identical(nrow(event1), 420L))

## test these events will apply:
model <- MRSA_model()
model@u0 <- as_u_matrix(result)

model@events <- SimInf_events(model@events@E, model@events@N, event1)
model@tspan <- 1
set.seed(1)
result_new <- clean_trajectory(trajectory(run(model)))

before <- result[result$pentype == "Finishing" & result$section == 1 & result$time == 1, "Sfinish"]
after <- result_new[result$pentype == "Finishing" & result$section == 1 & result$time == 1, "Sfinish"]

## Expect the same number of animals before and after
stopifnot(sum(before) == sum(after))

## expect the pen numbers to be different
stopifnot(mean(abs(before - after)) > 0)

## but the sum of the differences to be zero
stopifnot(sum(before - after) == 0)

## expect no events on other days
result[result$pentype == "Finishing", "countdown"][1:20] <- 70
event1 <- finishing_mingle(result, residual, prop = 1)$events
stopifnot(is.null(event1))
