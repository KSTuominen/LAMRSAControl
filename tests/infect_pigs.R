library(LAMRSAControl)
model <- MRSA_model()
## Use saved evenets to save time
data(events)
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- trajectory(run(model))
result <- clean_trajectory(result[result$time == 730, ])

## feed in the result of the last day
inf_events <- infect_pigs(result, pentype = "Gilt growing", time = 731, proportion = 0.2)

## make sure that we are not trying to move animals when there are none
nodes <- inf_events$node
pens <- result[result$node %in% nodes,]
stopifnot(any(pens$npigs > 0))

## test events in the model
model <- MRSA_model()
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(events[events$time == 731,], inf_events))
model@tspan <- as.double(731)
model@u0 <- as_u_matrix(result)

df <- trajectory(run(model))
stopifnot(any(df$Igilts1 > 1))
