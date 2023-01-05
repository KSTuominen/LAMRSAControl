##' sectioning
##'
##' In this function animals are moved one by one (n =1) from one pen
##' into several pens allows specifying if we want the destination
##' section to be fully empty (all-in-all-out) or just have x number
##' of pens used in: sows/gilts from gestation to farrowing,
##' sows/gilts from breeding to gestation This function checks also if
##' there are already events scheduled for the destination pens and
##' removes these from the returned pen list.  Returns a vector of pen
##' ids in given pentype that have enough empty pens in one section.
##'
##' @param x A dataframe which is the output from the trajectory of
##'     the MRSA model from 1 day.
##' @param pentype A string
##' @param pen.total integer for required amount of pens, if it's 0,
##'     we want to have a whole empty section
##' @param events The queued events for this timestep
##' @return vector of pen ids in the given pentype that have enough
##'     empty pens
##' @export
sectioning <- function(x,
                       pentype = c("Sow breeding",
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
                       pen.total = 0,
                       events = NULL) {
    ## Check arguments
    pentype <- match.arg(pentype)
    stopifnot(identical(class(x), c("MRSA_single_step_trajectory", "data.frame")))

    ## when we require the whole section to be empty
    if (pen.total == 0) {
        ## what sections are empty and available and the pentype of
        ## interest? We only need to check countdown since by
        ## definition in the daily function all countdown = 99999 pens
        ## must have npigs = 0.
        empty <- tapply(x$countdown,
                        list(x$pentype, x$section),
                        function(y) {
                            all(y == 99999)
                        })[pentype, ]
        empty <- empty[!is.na(empty)]
        empty <- as.numeric(names(empty[empty]))

        ## which pens have the desired pentype and belong to a section that is empty
        x <- x$node[x$section %in% empty & x$pentype == pentype]

        ## Return those ids that are not in the booked list
        x <- x[!(x %in% events$dest)]

        return(x)
    }

    ## if we don't require the whole section to be empty, we go
    ## through which section have enough free pens what sections have
    ## space what sections are empty and the pentype?
    empty <- tapply(x$countdown,
                    list(x$pentype, x$section),
                    function(y) {
                        sum(y == 99999)
                    })[pentype, ]
    empty <- empty[!is.na(empty)]
    empty <- as.numeric(names(empty[empty >= pen.total]))

    ##  which pen ids have the appropriate section, have the desired pentype and are empty
    index <- x$section %in% empty &
        x$pentype == pentype &
        x$npigs == 0 &
        x$countdown == 99999
    x <- x$node[index]

    ## Remove those ids that are in the booked list
    x[!(x %in% events$dest)]
}
