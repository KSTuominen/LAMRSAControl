##' MRSA_expected
##'
##' The expected prevalence at various time points used to
##' parameterize the model. There are 8 target observation points in
##' the herd that are included in the data. These are:
##'
##' * q_m1s : is the expected prevalence in sows when they enter the
##'     farrowing unit.
##'
##' * q_m2s : the expected prevalence in the sows three days after
##'     entering the farrowing unit.
##'
##' * q_m2p : the expected prevalence in the piglets three days after
##'     entering the farrowing unit.
##'
##' * q_m3s : the expected prevalence in the sows three weeks after
##'     entering the farrowing unit.
##'
##' * q_m3p : the expected prevalence in the piglets three weeks after
##'     entering the farrowing unit.
##'
##' * q_m4p : the expected prevalance in the pigs 7 days after weaning
##'
##' * q_m5p : the expected prevalance in the pigs 35 days after weaning
##'
##' * q_m6p : the expected prevalence in the pigs 84 days after moving
##'     the the finishing unit
##'
##' For each of the observation there was variation in the data from
##' the Broens paper. We therefore supply the 10th 50th and 90th
##' percentiles of those observations to mimic what you might see in a
##' low prevalence, 'normal' and high prevalence herd repectively.
##'
##' @importFrom stats setNames
##' @importFrom stats density
##' @importFrom stats quantile
##' @export
##' @return A dataframe containing the expected values to parameterize
##'     the model.
MRSA_expected <- function() {
    ## calculating the median, 10 % and 90 % percentiles prevalence
    ## from Broens et al. 2012 mX = sampling moment s = sows, p =
    ## piglets

    p <- c(0.1, 0.5, 0.9)
    ## Observations from Broens:
    pos_m1s <- c(0.063, 0.333, 0.500, 0.000, 0.000, 1.000, 0.700)
    d_m1s <- density(pos_m1s, from = 0, to = 1)
    d_m1s <- sample(d_m1s$x,
                    size = 1000000,
                    prob = d_m1s$y,
                    replace = TRUE)
    q_m1s <- quantile(d_m1s, probs = p)

    ## Observations from Broens:
    pos_m2s <- c(0.250, 1.000, 0.625, 0.000, 0.000, 1.000, 0.929)
    d_m2s <- density(pos_m2s, from = 0, to = 1)
    d_m2s <- sample(d_m2s$x,
                    size = 1000000,
                    prob = d_m2s$y,
                    replace = TRUE)
    q_m2s <- quantile(d_m2s, probs = p)

    ## Observations from Broens:
    pos_m3s <-c(0.938, 1.000, 1.000, 0.000, 0.000, 1.000, 0.714)
    d_m3s <- density(pos_m3s, from = 0, to = 1)
    d_m3s <- sample(d_m3s$x,
                    size = 1000000,
                    prob = d_m3s$y,
                    replace = TRUE)
    q_m3s <- quantile(d_m3s, probs = p)

    ## Observations from Broens:
    pos_m2p <- c(0.427, 1.000, 0.956, 0.044, 0.013, 1.000, 0.683)
    d_m2p <- density(pos_m2p, from = 0, to = 1)
    d_m2p <- sample(d_m2p$x,
                    size = 1000000,
                    prob = d_m2p$y,
                    replace = TRUE)
    q_m2p <- quantile(d_m2p, probs = p)

    ## Observations from Broens:
    pos_m3p <- c(0.930, 0.993, 0.989, 0.031, 0.000, 1.000, 0.872)
    d_m3p <- density(pos_m3p, from = 0, to = 1)
    d_m3p <- sample(d_m3p$x,
                    size = 1000000,
                    prob = d_m3p$y,
                    replace = TRUE)
    q_m3p <- quantile(d_m3p, probs = p)

    ## Observations from Broens:
    pos_m4p <- c(1.000, 0.559, 1.000, 0.523, 0.209, 1.000, 1.000)
    d_m4p <- density(pos_m4p, from = 0, to = 1)
    d_m4p <- sample(d_m4p$x,
                    size = 1000000,
                    prob = d_m4p$y,
                    replace = TRUE)
    q_m4p <- quantile(d_m4p, probs = p)

    ## Observations from Broens:
    pos_m5p <- c(NA, 1.000, 1.000, 0.891, 0.162, 1.000, 1.000)
    d_m5p <- density(pos_m5p, from = 0, to = 1, na.rm = TRUE)
    d_m5p <- sample(d_m5p$x,
                    size = 1000000,
                    prob = d_m5p$y,
                    replace = TRUE)
    q_m5p <- quantile(d_m5p, probs = p)

    ## Observations from Broens:
    pos_m6p <- c(NA, 0.862, 0.634, 0.645, 0.132, 1.000, 0.985)
    d_m6p <- density(pos_m6p, from = 0, to = 1, na.rm = TRUE)
    d_m6p <- sample(d_m6p$x,
                    size = 1000000,
                    prob = d_m6p$y,
                    replace = TRUE)
    q_m6p <- quantile(d_m6p, probs = p)

    ## Aggregate the expected values
    prev_percentiles <- setNames(data.frame(rbind(q_m1s, q_m2s, q_m2p,
                                                  q_m3s, q_m3p, q_m4p,
                                                  q_m5p, q_m6p)),
                                 c("10%", "50%", "90%"))

    ## rotate to another form
    prev_percentiles <- data.frame("percentile" = c("10%", "50%", "90%"))
    cbind(prev_percentiles, q_m1s, q_m2s, q_m2p, q_m3s, q_m3p, q_m4p,
          q_m5p, q_m6p)
}

##' MRSA_distance
##'
##' calculate the distance between the trajectory and the expected
##' data.
##'
##' @param result The ruesult of a run trajectory of the model
##' @param expected The expected results
##' @param ... Other arguments
##' @export
##' @importFrom SimInf prevalence
##' @return A number
MRSA_distance <- function(result, expected, ...) {
    events <- as.data.frame(result@events)

    ## collect nodes
    farr_pens <- which(u0()$pentype == "Farrowing")
    growing_pens <- which(u0()$pentype == "Growing") ## buffer pens excluded
    finishing_pens <- which(u0()$pentype == "Finishing")
    gilt_pens <- which(u0()$pentype == "Gilt growing")

    farrowing <- events[events$event == "extTrans" &
                        (events$select == 1 | events$select == 2) &
                        events$dest %in% farr_pens &
                        events$time > 2600 &
                        events$time <= 3000, c("time", "dest")]

    ## weaned piglets
    weaning <- events[events$event == "extTrans" &
                      events$dest %in% growing_pens &
                      events$time > 2600 &
                      events$time <= 3000, c("time", "dest")]

    ##  growers to finishing
    finishing <- events[events$event == "extTrans" &
                        events$dest %in% finishing_pens &
                        events$time > 2600 &
                        events$time <= 3000, c("time", "dest")]

    ## sows when they enter farrowing room
    q_m1s <- farrowing

    ## sows 3 days post farrowing
    q_m2s <- farrowing
    q_m2s$time <- q_m2s$time + 3

    ## piglets 3 days post farrowing
    q_m2p <- q_m2s

    ## sows 3 weeks post farrowing
    q_m3s <- farrowing
    q_m3s$time <- q_m3s$time + 21

    ## piglets 3 weeks post farrowing
    q_m3p <- q_m3s

    ## pigs 7 days post weaning
    q_m4p <- weaning
    q_m4p$time <- q_m4p$time + 7

    ## pigs 35 days post weaning
    q_m5p <- weaning
    q_m5p$time <- q_m5p$time + 35

    ## pigs 84 days post moving to finisher barn
    q_m6p <- finishing
    q_m6p$time <- q_m6p$time + 84

    ## Prevalence in the sows in the farrowing rooms
    q_ms <- prevalence(result, Isows1 + Isows2 + Isows3 ~ Ssows + Isows1 + Isows2 + Isows3, index = unique(q_m1s$dest))
    q_m1s <- q_ms[q_ms$time %in% q_m1s$time, "prevalence"]
    q_m2s <- q_ms[q_ms$time %in% q_m2s$time, "prevalence"]
    q_m3s <- q_ms[q_ms$time %in% q_m3s$time, "prevalence"]

    ## Prevalence of the piglets in the farrowing rooms
    q_mp <- prevalence(result, Ipiglets1 + Ipiglets2 + Ipiglets3 ~ Spiglets + Ipiglets1 + Ipiglets2 + Ipiglets3, index = unique(q_m2p$dest))
    q_m2p <- q_mp[q_mp$time %in% q_m2p$time, "prevalence"]
    q_m3p <- q_mp[q_mp$time %in% q_m3p$time, "prevalence"]

    ## Prevalence of pigs in the weaning rooms
    q_mp <- prevalence(result, Igrowers1 + Igrowers2 + Igrowers3 ~ Sgrowers + Igrowers1 + Igrowers2 + Igrowers3, index = unique(q_m4p$dest))
    q_m4p <- q_mp[q_mp$time %in% q_m4p$time, "prevalence"]
    q_m5p <- q_mp[q_mp$time %in% q_m5p$time, "prevalence"]

    ## Prevalence of pigs in the finisher
    q_mp <- prevalence(result, Ifinish1 + Ifinish2 + Ifinish3 ~ Sfinish + Ifinish1 + Ifinish2 + Ifinish3, index = unique(q_m6p$dest))
    q_m6p <- q_mp[q_mp$time %in% q_m6p$time, "prevalence"]

    ## The proportional distance to the expected values
    error <- list((expected["50%", "q_m1s"] - q_m1s) / expected["50%", "q_m1s"],
                  (expected["50%", "q_m2s"] - q_m2s) / expected["50%", "q_m2s"],
                  (expected["50%", "q_m3s"] - q_m3s) / expected["50%", "q_m3s"],
                  (expected["50%", "q_m2p"] - q_m2p) / expected["50%", "q_m2p"],
                  (expected["50%", "q_m3p"] - q_m3p) / expected["50%", "q_m3p"],
                  (expected["50%", "q_m4p"] - q_m4p) / expected["50%", "q_m4p"],
                  (expected["50%", "q_m5p"] - q_m5p) / expected["50%", "q_m5p"],
                  (expected["50%", "q_m6p"] - q_m6p) / expected["50%", "q_m6p"])

    ## The square root of the mean squared proportional error
    distance <- vapply(error, function(x) {
        sqrt(mean(x^2))
    }, numeric(1))

    sum(distance)
}
