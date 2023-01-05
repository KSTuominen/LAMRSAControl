library(LAMRSAControl)
model <- MRSA_model()
data(events)
expected <- MRSA_expected()
model@events <- SimInf_events(model@events@E, model@events@N, events = events[events$time < 731,])
model@tspan <- as.double(1:730)
result <- trajectory(run(model))
result <- clean_trajectory(result[result$time == 730, ])

## infect 20% of gilts, however we infected all at d730 in previous paper
inf_events <- infect_pigs(result, pentype = "Gilt growing", time = 731, proportion = 0.2)

## model with infected animals from day 730
model <- MRSA_model()
model@events <- SimInf_events(model@events@E, model@events@N, events = rbind(events[events$time >= 731,], inf_events))
model@tspan <- as.double(731:3000)
model@u0 <- as_u_matrix(result)

(beta_phi <- 0.0016)
beta_phi <- as.double(beta_phi)
model@gdata["beta_phi"] <- beta_phi
result <- run(model)
MRSA_distance(result, expected)
## You could also plot this
## herd_prev <- prevalence(result, Isows1+Isows2+Isows3+
##                                  Igilts1+Igilts2+Igilts3+
##                                  Ipiglets1+Ipiglets2+Ipiglets3+
##                                  Igrowers1+Igrowers2+Igrowers3+
##                                  Ifinish1+Ifinish2+Ifinish3~
##                                      Ssows+Sgilts+Spiglets+Sgrowers+Sfinish+
##                                      Isows1+Isows2+Isows3+
##                                      Igilts1+Igilts2+Igilts3+
##                                      Ipiglets1+Ipiglets2+Ipiglets3+
##                                      Igrowers1+Igrowers2+Igrowers3+
##                                      Ifinish1+Ifinish2+Ifinish3)

## sows <- prevalence(result, Isows1+Isows2+Isows3~Ssows+Isows1+Isows2+Isows3)
## gilts <- prevalence(result, Igilts1+Igilts2+Igilts3~Sgilts+Igilts1+Igilts2+Igilts3)
## piglets <- prevalence(result, Ipiglets1+Ipiglets2+Ipiglets3~Spiglets+Ipiglets1+Ipiglets2+Ipiglets3)
## growers <- prevalence(result, Igrowers1+Igrowers2+Igrowers3~Sgrowers+Igrowers1+Igrowers2+Igrowers3)
## finishers <- prevalence(result, Ifinish1+Ifinish2+Ifinish3~Sfinish+Ifinish1+Ifinish2+Ifinish3)

## plot(herd_prev, type = "l", col = "black", ylim = c(0, 1))
## lines(sows, col = "green")
## lines(gilts, col = "red")
## lines(piglets, col = "blue")
## lines(growers, col = "grey")
## lines(finishers, col = "purple")
