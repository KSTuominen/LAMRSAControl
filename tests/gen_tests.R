library(LAMRSAControl)
model <- MRSA_model()

## use existing events to split and add testing events
data(events)
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- trajectory(run(model))

timepoint <- 731
test_weaning <- gen_tests(events, weaning = TRUE, gilts = FALSE, time = timepoint)
test_gilts <- gen_tests(events, weaning = FALSE, gilts = TRUE, time = timepoint)
test_both <- gen_tests(events, weaning = TRUE, gilts = TRUE, time = timepoint)

## check if test events are created to correct nodes
farr_pens <- result[result$pentype == 7, "node"]
gg_pens <- result[result$pentype == 11, "node"]
test_events <- test_both[test_both$event == "enter" & test_both$select == 10, ]
result_events <- test_both[test_both$event == "exit" & test_both$select == 11, ]
## can we find all the scheduled events from nodes that exist in the correct pentype categories
stopifnot(any(!(test_events$node %in% farr_pens | test_events$node %in% gg_pens)) == FALSE)
stopifnot(any(!(result_events$node %in% farr_pens | result_events$node %in% gg_pens)) == FALSE)

## test nothing
ob <- tools::assertError(gen_tests(events, weaning = FALSE, gilts = FALSE, time = timepoint))[[1]]$message
ex <-  "Expect at least one of weaning or gilts to be TRUE"
stopifnot(length(grep(ex, ob)) == 1)

## try scheduling tests to non-existing day
ob <- tools::assertError(gen_tests(events, weaning = TRUE, gilts = FALSE, time = 5))[[1]]$message
ex <- "time %in% seq\\(time_range\\[1\\], time_range\\[2\\]\\) is not TRUE"
stopifnot(length(grep(ex, ob)) == 1)

## check that we didn't get tests to unwanted days
time_range <- range(test_both$time)
stopifnot(timepoint %in% time_range[1]:time_range[2])

## does the testing trigger change correct column in result df?
result <- clean_trajectory(result[result$time == 730, ])
model <- MRSA_model()
model@events <- SimInf_events(model@events@E, model@events@N, events = test_both)
model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result)
df <- trajectory(run(model))
pos_ind <- df[df$test_indicator > 0, ]
## are these positive test indicators all in correct pens?
stopifnot(any(!(pos_ind$node %in% farr_pens | pos_ind$node %in% gg_pens)) == FALSE)
