library(LAMRSAControl)

model <- MRSA_model()
data(events)
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- trajectory(run(model))
result <- clean_trajectory(result[result$time == 730, ])
inf_events <- infect_pigs(result, pentype = "Gilt growing", time = 731, proportion = 0.2)

model <- MRSA_model()
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(events[events$time >= 731,], inf_events))

model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result)

result <- trajectory(run(model))
result <- clean_trajectory(result[result$time == 1200, ])
plot(result)
