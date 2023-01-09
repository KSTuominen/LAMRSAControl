library(LAMRSAControl)

result <- create_u0(node = TRUE, time = TRUE)
result[result$pentype == "Sow breeding buffer", "countdown"][c(1,2)] <- 0
residual <- result[result$pentype == "Sow breeding buffer" & result$countdown == 0, ]

ob <- buffer_culling(result, residual)$events
ex <- data.frame(event = factor(c(1L, 1L), labels = c("exit")),
                 time = c(1L, 1L),
                 node = c(11L, 12L),
                 dest = c(0L, 0L),
                 n = c(0L, 0L),
                 proportion = c(1, 1),
                 select = c(1L, 1L),
                 shift = c(0L, 0L))
stopifnot(identical(ob, ex))

## try with gilts
result <- create_u0(node = TRUE, time = TRUE)
result[result$pentype == "Gilt breeding buffer", "countdown"][c(1,2)] <- 0
residual <- result[result$pentype == "Gilt breeding buffer" & result$countdown == 0, ]

ob <- buffer_culling(result, residual)$events
ex <- data.frame(event = factor(c(1L, 1L), labels = c("exit")),
                 time = c(1L, 1L),
                 node = c(24L, 25L),
                 dest = c(0L, 0L),
                 n = c(0L, 0L),
                 proportion = c(1, 1),
                 select = c(2L, 2L),
                 shift = c(0L, 0L))
stopifnot(identical(ob, ex))
