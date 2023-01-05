library(LAMRSAControl)

result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$pentype == "Sow gestation", "countdown"][1:2] <- 0
result[result$pentype == "Sow gestation", "Ssows"][1:2] <- 10
result[result$pentype == "Sow gestation", "npigs"][1:2] <- 10
result <- clean_trajectory(result)
residual <- result[result$pentype == "Sow gestation" & result$countdown == 0, ]

## Sows farrow
set.seed(6)
ob <- farrowing(result, residual)$events
ex <- data.frame(event = factor(c(rep(1L, 20), rep(2L, 20)), labels = c("extTrans", "enter")),
                 time = 1L,
                 node = c(34L, 34L, 34L, 34L, 34L, 34L, 34L, 34L, 34L,
                          34L, 35L, 35L, 35L, 35L, 35L, 35L, 35L, 35L,
                          35L, 35L, 99L, 100L, 101L, 102L, 103L, 104L,
                          105L, 106L, 107L, 108L, 109L, 110L, 111L,
                          112L, 113L, 114L, 115L, 116L, 117L, 118L),
                 dest = c(99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L,
                          109L, 110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 0L,
                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                          0L, 0L, 0L),
                 n = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                       1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 15L, 12L, 22L,
                       17L, 8L, 9L, 12L, 10L, 18L, 12L, 13L, 12L, 10L,
                       10L, 8L, 16L, 9L, 18L, 14L, 23L),
                 proportion = 0,
                 select = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L,
                            3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                 shift = 0L)
stopifnot(identical(ob, ex))

## gilts farrow
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
result[result$pentype == "Gilt gestation", "countdown"][1:2] <- 0
result[result$pentype == "Gilt gestation", "Ssows"][1:2] <- 10
result[result$pentype == "Gilt gestation", "npigs"][1:2] <- 10
result <- clean_trajectory(result)
residual <- result[result$pentype == "Gilt gestation" & result$countdown == 0, ]

set.seed(6)
ob <- farrowing(result, residual)$events
ex <- data.frame(event = factor(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                  1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                  2L, 2L, 2L, 2L), labels = c("extTrans", "enter")),
                 time = 1L, node = c(69L, 69L, 69L, 69L, 69L, 69L, 69L, 69L, 69L, 69L, 70L, 70L,
                                     70L, 70L, 70L, 70L, 70L, 70L, 70L, 70L, 99L, 100L, 101L, 102L,
                                     103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L,
                                     114L, 115L, 116L, 117L, 118L),
                 dest = c(99L, 100L, 101L, 102L, 103L, 104L, 105L,
                          106L, 107L, 108L, 109L, 110L, 111L, 112L,
                          113L, 114L, 115L, 116L, 117L, 118L, 0L, 0L,
                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                          0L, 0L, 0L, 0L, 0L, 0L, 0L),
                 n = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                       1L, 1L, 1L, 1L, 1L, 15L, 12L, 22L, 17L, 8L, 9L, 12L, 10L, 18L,
                       12L, 13L, 12L, 10L, 10L, 8L, 16L, 9L, 18L, 14L, 23L),
                 proportion = 0,
                 select = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                            2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                            3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                 shift = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L,
                           0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                           0L, 0L, 0L, 0L, 0L, 0L, 0L))
stopifnot(identical(ob, ex))
