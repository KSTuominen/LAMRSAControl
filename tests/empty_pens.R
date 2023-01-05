library(LAMRSAControl)
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)
result <- clean_trajectory(result)

## Make sure we get some empty pens
stopifnot(identical(length(empty.pens(result, "Finishing")), 540L))

## Make sure we get less empty pens
result$Sfinish[541:580] <- 20
result$countdown[541:580] <- 0

result <- clean_trajectory(result)
stopifnot(identical(length(empty.pens(result, "Finishing")), 500L))

## Make sure an event blocks a pen from being 'empty'
event <- data.frame(event = factor(1L, labels = "extTrans"),
                    time = 1L,
                    node = 350L,
                    dest = 581L,
                    n = 10L,
                    proportion = 0,
                    select = 6L,
                    shift = 0L)

stopifnot(identical(length(empty.pens(result, "Finishing", events = event)), 499L))

## Make sure an enter event blocks a pen from being 'empty'
event <- data.frame(event = factor(1L, labels = "enter"),
                    time = 1L,
                    node = 581L,
                    dest = 0L,
                    n = 10L,
                    proportion = 0,
                    select = 6L,
                    shift = 0L)

stopifnot(identical(length(empty.pens(result, "Finishing", events = event)), 499L))
