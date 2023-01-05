#' timers
#'
#' Function to return appropriate countdown timer for the given
#' pentype
#' @param pentype A string, dt = downtime
#' @param biweekly are the events biweekly. For the biweekly model we
#'     have different downtimes between groups.
#' @export
#' @return an integer for setting countdown timer
timers <- function(pentype = c("Breeding",
                               "Breeding dt",
                               "Breeding buffer",
                               "Gestation",
                               "Gestation dt",
                               "Farrowing",
                               "Farrowing dt",
                               "Growing",
                               "Growing dt",
                               "Growing buffer",
                               "Growing buffer dt",
                               "Finishing",
                               "Finishing dt",
                               "Gilt growing",
                               "Gilt growing dt"),
                   biweekly = FALSE) {

    extradays <- 0

    if (biweekly) {
        extradays <- 7
    }

    ## Check arguments
    pentype <- match.arg(pentype)

    switch(pentype,
           'Breeding' = 32L,
           'Breeding dt' = 2L + extradays,
           'Breeding buffer' = 56L,
           'Gestation' = 88L,
           'Gestation dt' = 1L + extradays,
           'Farrowing' = 35L,
           'Farrowing dt' = 2L + extradays,
           'Growing' = 56L,
           'Growing dt' = 1L + extradays,
           'Growing buffer' = 23L,
           'Growing buffer dt' = 4L + extradays,
           'Finishing' = 99L,
           'Finishing dt' = 6L + extradays,
           'Gilt growing' = 216L,
           'Gilt growing dt' = 6L + extradays)

}
