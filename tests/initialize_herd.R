library(LAMRSAControl)
result <- u0(node = TRUE, time = TRUE, npigs = TRUE)

## Test 1
## Check to make sure gilts are added the day is 8
result$time <- 8
result <- clean_trajectory(result)
ob <- initialize_herd(result = result)$events
ex <- data.frame(event = factor(1L, labels = c("enter")),
                 time = 8L,
                 node = 16L,
                 dest = 0L,
                 n = 22L,
                 proportion = 0,
                 select = 7L,
                 shift = 0L)
stopifnot(identical(ob, ex))

## Test 2
## Check to make sure gilts are not added when day is 13
result$time <- 13
ob <- initialize_herd(result = result)$events
stopifnot(is.null(ob))

## Test 3
## Check to make sure gilts are not added when day is 8 but there is
## no space in the breeding section
result$time <- 8
result$Sgilts[result$pentype == "Gilt breeding"] <- 10
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
ob <- initialize_herd(result = result)$events
stopifnot(is.null(ob))

## Test 4
## Check to make sure gilts are not added to a later pen if the first
## three are occupied
result$time <- 8
result$Sgilts[result$pentype == "Gilt breeding"] <- 0
result$Sgilts[result$pentype == "Gilt breeding"][1:3] <- 10
result$npigs <- result$Sgilts
result <- clean_trajectory(result)
ob <- initialize_herd(result = result)$events
ex <- data.frame(event = factor(1L, labels = c("enter")),
                 time = 8L,
                 node = 19L,
                 dest = 0L,
                 n = 22L,
                 proportion = 0,
                 select = 7L,
                 shift = 0L)
stopifnot(identical(ob, ex))
