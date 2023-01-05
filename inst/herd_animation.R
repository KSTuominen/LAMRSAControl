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

times <- 731:3000
for(time in times) {
    png(sprintf("plots/map%05d.png", time), width = 1200, height = 500)
    plot(x = clean_trajectory(result[result$time == time, ]))
    dev.off()
}

## Combining a series of generated images into a video:
## system("ffmpeg -start_number 731 -i plots/map%05d.png -r 10 -c:v libx264 -strict -2 -preset veryslow -pix_fmt yuv420p -vf scale=trunc(iw/2)*2:trunc(ih/2)*2 -f mp4 farm-indirect.mp4")
