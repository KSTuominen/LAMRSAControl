library(LAMRSAControl)

## You want to move pigs from 1 pen to 7 others distributed
## evenly. The destination pens are named:
dest <- c(1, 2, 3, 4, 5, 6, 7)
## to proportion going to each pen is equal:
p <- c(1, 1, 1, 1, 1, 1, 1)
## Then you need to set p in the event function to:
ob <- scale_p(dest, p)
ex <- c(1/7, 1/6, 1/5, 1/4, 1/3, 1/2, 1)
stopifnot(identical(round(ob, 3), round(ex, 3)))

## If the pens are not ordered then the result should be in the same
## order:
dest <- c(5, 2, 3, 8, 1, 3, 5)
## to proportion going to each pen is equal:
p <- c(1, 1, 1, 1, 1, 1, 1)
## Then you need to set p in the event function to:
ob <- scale_p(dest, p)
ex <- c(1/3, 1/6, 1/5, 1, 1/7, 1/4, 1/2)
stopifnot(identical(round(ob, 3), round(ex, 3)))

## An example of these events being processed by "SimInf" the source
## pen has 100 animals and we want to distribute them over 4 dest pens
## with unordered pen IDS
npigs <- 100
dest <- c(5, 2, 3, 8)
p <- c(0.3, 0.4, 0.2, 0.1)

## This is the p that we supply to the event p
ob <- scale_p(dest, p)
ex <- c(0.75, 0.4, 1/3, 1)
stopifnot(identical(round(ob, 3), round(ex, 3)))

## Then we should get the same if we use proportions in a different scale:
p <- c(3, 4, 2, 1)
ob <- scale_p(dest, p)
stopifnot(identical(round(ob, 3), round(ex, 3)))
