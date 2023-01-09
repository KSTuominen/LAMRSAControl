##' MRSA_model
##'
##' @importFrom SimInf mparse
##' @importFrom utils data
##' @param Nt The daily decay rate (The proporation of phi that is
##'     left after one day's decay)
##' @param beta_phi The indirect transmission rate for all animal categories
##' @param duration The mean duration of carriage. This is estimated
##'     as a erlang distribution with shape parameter = 3.
##' @param mortality_grower The daily mortality rate in growers
##' @param mortality_finish The daily mortality rate in finishers
##' @param room_scale The relative effect of phi at the room level compared to phi at the pen level.
##' @param farm_scale The relative effect of phi at the farm level compared to phi at the room level.
##' @param cleaning_effect_AIAO The proportion of phi left after cleaning AIAO pens
##' @param cleaning_effect_CF The proportion of phi left after cleaning any pens
##' @param Se The diagnostic sensitivity of the test
##' @return A SimInf model object
##' @export
MRSA_model <- function(Nt=0.8705506,
                       beta_phi = 0.00285,
                       duration = 17.4,
                       mortality_grower = 0.0004,
                       mortality_finish = 0.0002,
                       room_scale = 0.1,
                       farm_scale = 0.1,
                       cleaning_effect_AIAO = 1.0,
                       cleaning_effect_CF = 1.0,
                       Se = 0.70) {

    alpha <- 10000
    gdata <- c(Nt = Nt,
               beta_phi = beta_phi,
               duration = duration,
               mortality_grower = mortality_grower,
               mortality_finish = mortality_finish,
               room_scale = room_scale,
               farm_scale = farm_scale,
               cleaning_effect_AIAO = cleaning_effect_AIAO,
               cleaning_effect_CF = cleaning_effect_CF,
               Se = Se,
               alpha = alpha)

    ## shape of erlang distribution (used in generating compartments and matrices)
    shape <- 3

    sows <- list(transitions = c("Isows1 -> (1/(3*duration)) * Isows1 -> Isows2",
                                 "Isows2 -> (1/(3*duration)) * Isows2 -> Isows3",
                                 "Isows3 -> (1/(3*duration)) * Isows3 -> Ssows"),
                 compartments = c("Isows1", "Isows2", "Isows3", "Ssows"))
    gilts <- list(transitions = c("Igilts1 -> (1/(3*duration)) * Igilts1 -> Igilts2",
                                  "Igilts2 -> (1/(3*duration)) * Igilts2 -> Igilts3",
                                  "Igilts3 -> (1/(3*duration)) * Igilts3 -> Sgilts"),
                  compartments = c("Igilts1", "Igilts2", "Igilts3", "Sgilts"))
    piglets <- list(transitions = c("Ipiglets1 -> (1/(3*duration)) * Ipiglets1 -> Ipiglets2",
                                    "Ipiglets2 -> (1/(3*duration)) * Ipiglets2 -> Ipiglets3",
                                    "Ipiglets3 -> (1/(3*duration)) * Ipiglets3 -> Spiglets"),
                    compartments = c("Ipiglets1", "Ipiglets2", "Ipiglets3", "Spiglets"))
    growers <- list(transitions = c("Igrowers1 -> (1/(3*duration)) * Igrowers1 -> Igrowers2",
                                    "Igrowers2 -> (1/(3*duration)) * Igrowers2 -> Igrowers3",
                                    "Igrowers3 -> (1/(3*duration)) * Igrowers3 -> Sgrowers"),
                    compartments = c("Igrowers1", "Igrowers2", "Igrowers3", "Sgrowers"))
    finish <- list(transitions = c("Ifinish1 -> (1/(3*duration)) * Ifinish1 -> Ifinish2",
                                   "Ifinish2 -> (1/(3*duration)) * Ifinish2 -> Ifinish3",
                                   "Ifinish3 -> (1/(3*duration)) * Ifinish3 -> Sfinish"),
                   compartments = c("Ifinish1", "Ifinish2", "Ifinish3", "Sfinish"))


    transitions <-  c( ## Transitions for pooled testing and the effect of that:
        "test_indicator -> ((Isows1 + Isows2 + Isows3) || (Igilts1 + Igilts2 + Igilts3)) > 0 ? Se * test_indicator * alpha : 0 -> result_indicator",
        "test_indicator -> ((Isows1 + Isows2 + Isows3) || (Igilts1 + Igilts2 + Igilts3)) > 0 ? (1-Se) * test_indicator * alpha : test_indicator * alpha -> @",
        ## cleaning CF pens
        "cleaning_indicator -> alpha * cleaning_indicator -> @",
        ## replace infected animals with susceptible when test is positive
        "Isows1 -> alpha * result_indicator * Isows1 -> Ssows",
        "Isows2 -> alpha * result_indicator * Isows2 -> Ssows",
        "Isows3 -> alpha * result_indicator * Isows3 -> Ssows",
        "Ipiglets1 -> alpha * result_indicator * Ipiglets1 -> Spiglets",
        "Ipiglets2 -> alpha * result_indicator * Ipiglets2 -> Spiglets",
        "Ipiglets3 -> alpha * result_indicator * Ipiglets3 -> Spiglets",
        "Igilts1 -> alpha * result_indicator * Igilts1 -> Sgilts",
        "Igilts2 -> alpha * result_indicator * Igilts2 -> Sgilts",
        "Igilts3 -> alpha * result_indicator * Igilts3 -> Sgilts",
        "Ifinish1 -> alpha * result_indicator * Ifinish1 -> Sfinish",
        "Ifinish2 -> alpha * result_indicator * Ifinish2 -> Sfinish",
        "Ifinish3 -> alpha * result_indicator * Ifinish3 -> Sfinish",
        "Igrowers1 -> alpha * result_indicator * Igrowers1 -> Sgrowers",
        "Igrowers2 -> alpha * result_indicator * Igrowers2 -> Sgrowers",
        "Igrowers3 -> alpha * result_indicator * Igrowers3 -> Sgrowers",
        ## Transitions for disease spread
        "Ssows -> (1 - result_indicator) * Ssows * beta_phi * (phi + s_phi * room_scale + f_phi * room_scale * farm_scale) -> Isows1 + Icum",
        "Sgilts -> (1 - result_indicator) * Sgilts * beta_phi * (phi + s_phi * room_scale + f_phi * room_scale * farm_scale) -> Igilts1 + Icum",
        "Spiglets -> (1 - result_indicator) * Spiglets * beta_phi * (phi + s_phi * room_scale + f_phi * room_scale * farm_scale) -> Ipiglets1 + Icum",
        "Sgrowers -> (1 - result_indicator) * Sgrowers * beta_phi * (phi + s_phi * room_scale + f_phi * room_scale * farm_scale) -> Igrowers1 + Icum",
        "Sfinish -> (1 - result_indicator) * Sfinish * beta_phi * (phi + s_phi * room_scale + f_phi * room_scale * farm_scale) -> Ifinish1 + Icum",
        sows$transitions,
        gilts$transitions,
        piglets$transitions,
        growers$transitions,
        finish$transitions,
        "Sgrowers -> Sgrowers * mortality_grower -> @",
        sapply(seq_len(shape), function(i) {
            gsub("%replace%", i, "Igrowers%replace% -> Igrowers%replace% * mortality_grower -> @")
        }),
        "Sfinish -> Sfinish * mortality_finish-> @",
        sapply(seq_len(shape), function(i) {
            gsub("%replace%", i, "Ifinish%replace% -> Ifinish%replace% * mortality_finish -> @")
        }))

    compartments <- c("Ssows",
                      sows$compartments[-length(sows$compartments)],
                      "Sgilts",
                      gilts$compartments[-length(gilts$compartments)],
                      "Spiglets",
                      piglets$compartments[-length(piglets$compartments)],
                      "Sgrowers",
                      growers$compartments[-length(growers$compartments)],
                      "Sfinish",
                      finish$compartments[-length(finish$compartments)],
                      "pentype",
                      "section",
                      "countdown",
                      "test_indicator",
                      "result_indicator",
                      "cleaning_indicator",
                      "Icum")

    E <- select_matrix(compartments)
    N <- shift_matrix(compartments)
    u0 <- create_u0(node = TRUE,
             test_indicator = TRUE,
             result_indicator = TRUE,
             cleaning_indicator = TRUE,
             Icum = TRUE)
    v0 <- data.frame(phi = rep(0, nrow(u0)),
                     s_phi = rep(0, nrow(u0)),
                     f_phi = rep(0, nrow(u0)),
                     npigs = rep(0, nrow(u0)))

    pts_fun <- readLines(system.file("ptsfun.c", package = "LAMRSAControl"))

    mparse(transitions = transitions,
           compartments = compartments,
           ldata = ldata(),
           gdata = gdata,
           u0 = u0,
           v0 = v0,
           E = E,
           N = N,
           tspan = 1:2000,
           pts_fun = pts_fun)
}

#' pen_capacity
#'
#' pensize of a pentype
#' @param pentype A character vector of pentypes
#' @return an numeric vector of the capacities of these pentypes
#' @export
pen_capacity <- function(pentype) {
    ## Check arguments
    capacity <- data.frame(pentype = c("Sow breeding",
                                       "Gilt breeding",
                                       "Sow breeding buffer",
                                       "Gilt breeding buffer",
                                       "Sow gestation",
                                       "Gilt gestation",
                                       "Farrowing",
                                       "Growing",
                                       "Growing buffer",
                                       "Finishing",
                                       "Finishing buffer",
                                       "Gilt growing"),
                           capacity = c(22,
                                        22,
                                        22,
                                        22,
                                        11,
                                        11,
                                        100,
                                        12,
                                        12,
                                        10,
                                        10,
                                        10))
    stopifnot(all(pentype %in% capacity$pentype))
    capacity$capacity[match(pentype, capacity$pentype)]
}

#' nanimals
#'
#' Number of animals in a pen
#' @param x A result or residual object
#' @return A numeric vector
#' @export
nanimals <- function(x) {

    index <- names(x) %in% c("Ssows", "Isows1", "Isows2",  "Isows3",
                             "Sgilts", "Igilts1", "Igilts2",  "Igilts3",
                             "Spiglets", "Ipiglets1", "Ipiglets2",  "Ipiglets3",
                             "Sgrowers", "Igrowers1", "Igrowers2",  "Igrowers3",
                             "Sfinish", "Ifinish1", "Ifinish2",  "Ifinish3")

        return(rowSums(x[ ,index]))
}


#' clean trajectory
#'
#' format the daily trajectory into more readable
#' @param x  the object$result of a trajectory
#' @return dataframe
#' @export
clean_trajectory <- function(x) {

    ## Check that the trajectory is just from one day:
    ob_class <- "MRSA_trajectory"
    if (all(table(x$node) == 1)) {
        ob_class <- "MRSA_single_step_trajectory"
    }

    ## Label the pentype
    x$pentype <- factor(x$pentype,
                        labels = c("Sow breeding",
                                   "Sow breeding buffer",
                                   "Gilt breeding",
                                   "Gilt breeding buffer",
                                   "Sow gestation",
                                   "Gilt gestation",
                                   "Farrowing",
                                   "Growing",
                                   "Growing buffer",
                                   "Finishing",
                                   "Gilt growing"))

    ## Add the number of animals and capacity to the result object
    ## then we don't have to keep calculating it.
    x$capacity <- pen_capacity(x$pentype)

    if(!(ob_class %in% class(x))) {
        class(x) <- c(ob_class, class(x))
    }
    x
}

##' as_u_matrix
##'
##' Coerce the data.frame to a transposed u matrix.
##'
##' @param x A trajectory
##' @return A u matrix
##' @export
as_u_matrix <- function(x) {
    stopifnot("MRSA_single_step_trajectory" %in% class(x))
    x  <- x[, c("Ssows", "Isows1", "Isows2",  "Isows3", "Sgilts",
                "Igilts1", "Igilts2",  "Igilts3", "Spiglets",
                "Ipiglets1", "Ipiglets2",  "Ipiglets3", "Sgrowers",
                "Igrowers1", "Igrowers2",  "Igrowers3", "Sfinish",
                "Ifinish1", "Ifinish2",  "Ifinish3", "pentype",
                "section", "countdown", "test_indicator",
                "result_indicator", "cleaning_indicator", "Icum")]
    n_col <- ncol(x)
    n_row <- nrow(x)
    lbl <- colnames(x)
    x <- as.integer(t(data.matrix(x)))
    attributes(x) <- NULL
    dim(x) <- c(n_col, n_row)
    rownames(x) <- lbl
    x
}

##' as_v_matrix
##'
##' Coerce the data.frame to a transposed v matrix.
##'
##' @param x a trajectory
##' @return a vmatrix
##' @export
as_v_matrix <- function(x) {
    stopifnot("MRSA_single_step_trajectory" %in% class(x))
    x  <- x[, c("phi", "s_phi", "f_phi", "npigs"), drop = FALSE]
    n_col <- ncol(x)
    n_row <- nrow(x)
    lbl <- colnames(x)
    x <- as.double(t(data.matrix(x)))
    attributes(x) <- NULL
    dim(x) <- c(n_col, n_row)
    rownames(x) <- lbl
    x
}
