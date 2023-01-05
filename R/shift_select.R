##' select_matrix
##'
##' Build a select matrix
##'
##' @return a matrix
##' @param compartments A vector of compartment
select_matrix <- function(compartments = NULL) {
    stopifnot(!is.null(compartments))
    shape <- 3
    E <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            rep(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), shape),
                  0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
            rep(c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), shape),
                  0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0,
            rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), shape),
                  0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
            rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), shape),
                  0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0,
            rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), shape),
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                byrow = TRUE,
                ncol = 12,
                dimnames = list(compartments, c("Sows",
                                                "Gilts",
                                                "Susceptible piglets",
                                                "Piglets",
                                                "Growers",
                                                "Finishers",
                                                "Susceptible gilts",
                                                "Infected gilts",
                                                "All susceptible",
                                                "test_indicator",
                                                "both_indicators",
                                                "cleaning_indicator")))

    E
}

##' shift_matrix
##'
##' Build a shift matrix
##'
##' @return A matrix
##' @param compartments A vector of compartment
shift_matrix <- function(compartments = NULL) {
    stopifnot(!is.null(compartments))
    shape <- 3
    N <- matrix(c(0                    ,           0 ,           0 ,              0 ,   1 ,
                  rep(c(0              ,           0 ,           0 ,              0 ,   0) , shape) ,
                  (-(shape + 1))       ,           0 ,           0 ,              0 ,   1 ,
                  rep(c((-(shape + 1)) ,           0 ,           0 ,              0 ,   0) , shape) ,
                  0                    , (shape + 1) ,           0 , (-(shape + 1)) ,   1 ,
                  rep(c(0              , (shape + 1) ,           0 , (-(shape + 1)) ,   0) , shape) ,
                  0                    ,           0 , (shape + 1) ,              0 ,   1 ,
                  rep(c(0              ,           0 , (shape + 1) ,              0 ,   0) , shape) ,
                  0                    ,           0 ,           0 ,              0 ,   1 ,
                  rep(c(0              ,           0 ,           0 ,              0 ,   0) , shape) ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0 ,
                  0                    ,           0 ,           0 ,              0 ,   0),
                dimnames = list(compartments, c("gilts_to_farrowing",
                                                "weaning_of_piglets",
                                                "grow_to_finish",
                                                "piglets_to_gilts",
                                                "S_to_I")),
                ncol = 5,
                byrow = TRUE)
    N
}
