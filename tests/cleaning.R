library(LAMRSAControl)
model <- MRSA_model()

## use existing events
data(events)
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- trajectory(run(model))
result730 <-  result[result$time == 730, ]

## infect gilts at d730 (has both added and old events)
result730clean <- clean_trajectory(result730)
inf_events <- infect_pigs(result730clean, pentype = "Gilt growing", time = 731, proportion = 0.2)

## events after d730 without cf cleaning events
events731 <-  events[events$time >= 731,]

## run the model with events with cleaning_effect_AIAO = 0 (no added events)
model <- MRSA_model(cleaning_effect_AIAO = 0.0)
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(inf_events, events731))
model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result730clean)
resultAIAO <- trajectory(run(model))

## run the model with the events with cleaning_effect_CF = 0
## generate continuous flow cleaning events at day 731
cf <- clean_cf(result730, events, time = 731)
model <- MRSA_model(cleaning_effect_CF = 0.0)
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(inf_events, cf))
model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result730clean)
resultCF<- trajectory(run(model))

## run the model with the cleaning_effect_AIAO = 0 and cleaning_effect_CF = 0
model <- MRSA_model(cleaning_effect_AIAO = 0.0, cleaning_effect_CF = 0.0)
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(inf_events, cf))
model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result730clean)
resultCF_AIAO<- trajectory(run(model))

## check that the phi in a AIAO pen is 0 when there are no animals in the pen
AIAO_pens <- resultCF_AIAO[resultCF_AIAO$pentype %in% 7:10 & resultCF_AIAO$npigs == 0, ]
stopifnot(!(any(AIAO_pens$phi > 0)))

## test if cleaning indicator gets triggered
clean_time <- unique(events[events$event == "extTrans" & events$select == 4 & events$shift == 2, "time"])-1
clean_ind <- resultCF_AIAO[resultCF_AIAO$pentype %in% c(1:6, 11) & resultCF_AIAO$time %in% clean_time, "cleaning_indicator"]
stopifnot(!(any(clean_ind != 1)))

## test if phi in continuous flow was reduced
pre_clean <- clean_time-1
pre_cf <- resultCF_AIAO[resultCF_AIAO$pentype %in% c(1:6, 11) & resultCF_AIAO$time %in% pre_clean, "phi"]
post_cf <- resultCF_AIAO[resultCF_AIAO$pentype %in% c(1:6, 11) & resultCF_AIAO$time %in% clean_time, "phi"]
df <- data.frame(pre_cf = pre_cf, post_cf = post_cf)
compare_phi <- pre_cf >= post_cf
stopifnot(any(compare_phi == TRUE))
