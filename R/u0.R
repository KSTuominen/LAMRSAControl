#' Generates the herd structure
#'
#' @param sow.breeding.pens.persection integer number of pens in the section
#' @param sow.breeding.pens.section integer number of sections in the unit
#' @param gilt.breeding.pens.persection integer number of pens in the section
#' @param gilt.breeding.pens.section integer number of sections in the unit
#' @param sow.breeding.buffer.pens.persection integer number of pens in the section
#' @param sow.breeding.buffer.pens.section integer number of sections in the unit
#' @param gilt.breeding.buffer.pens.persection integer number of pens in the section
#' @param gilt.breeding.buffer.pens.section integer number of sections in the unit
#' @param sow.gestation.pens.persection integer number of pens in the section
#' @param sow.gestation.pens.section integer number of sections in the unit
#' @param gilt.gestation.pens.persection integer number of pens in the section
#' @param gilt.gestation.pens.section integer number of sections in the unitn
#' @param farrowing.pens.persection integer number of pens in the section
#' @param farrowing.pens.sections integer number of sections in the unit
#' @param growing.pens.persection integer number of pens in the section
#' @param growing.pens.sections integer number of sections in the unit
#' @param growing.buffer.pens.persection integer number of pens in the section
#' @param growing.buffer.pens.sections integer number of sections in the unit
#' @param finishing.pens.persection integer number of pens in the section
#' @param finishing.pens.sections integer number of sections in the unit
#' @param growing.gilt.pens.persection integer number of pens in the section
#' @param growing.gilt.pens.sections integer number of sections in the unit
#' @param node include node id?
#' @param time include time = 1?
#' @param capacity include pen capacity?
#' @param npigs include npigs?
#' @param phi include phi?
#' @param s_phi include s_phi?
#' @param f_phi include f_phi?
#' @param test_indicator include test_indicator?
#' @param result_indicator include test_indicator?
#' @param cleaning_indicator include test_indicator?
#' @param Icum include test_indicator?
#'
#' @return dataframe of herd structure
#' @export
u0 <- function(sow.breeding.pens.persection = 10,
               sow.breeding.pens.section = 1,

               gilt.breeding.pens.persection = 8,
               gilt.breeding.pens.section = 1,

               sow.breeding.buffer.pens.persection = 5,
               sow.breeding.buffer.pens.section = 1,

               gilt.breeding.buffer.pens.persection = 10,
               gilt.breeding.buffer.pens.section = 1,

               sow.gestation.pens.persection = 35,
               sow.gestation.pens.section = 1,

               gilt.gestation.pens.persection = 30,
               gilt.gestation.pens.section = 1,

               farrowing.pens.persection = 26,
               farrowing.pens.sections = 6,

               growing.pens.persection = 26,
               growing.pens.sections = 10,

               growing.buffer.pens.persection = 26,
               growing.buffer.pens.sections = 1,

               finishing.pens.persection = 30,
               finishing.pens.sections = 18,

               growing.gilt.pens.persection = 25,
               growing.gilt.pens.sections = 1,

               node = FALSE,
               time = FALSE,
               capacity = FALSE,
               npigs = FALSE,
               phi = FALSE,
               s_phi = FALSE,
               f_phi = FALSE,
               test_indicator = FALSE,
               result_indicator = FALSE,
               cleaning_indicator = FALSE,
               Icum = FALSE

) {
    ## usually there is only 1 breeding and 1 gestation section that has
    ## both sow and gilt breeding pens the section vector needs to be
    ## changed if we decide to have several sow sections if there are
    ## several sections, then gilts should probably be moved to their own
    ## section
    sow.breed.section.vec <- rep(seq_len(sow.breeding.pens.section),
                                 each = sow.breeding.pens.persection)

    gilt.breed.section.vec <- rep(seq_len(gilt.breeding.pens.section),
                                  each = gilt.breeding.pens.persection)

    sow.breed.buffer.section.vec <- rep(seq_len(sow.breeding.buffer.pens.section),
                                        each = sow.breeding.buffer.pens.persection)

    gilt.breed.buffer.section.vec <- rep(seq_len(gilt.breeding.buffer.pens.section),
                                         each = gilt.breeding.buffer.pens.persection)

    ## gestation pens are normally smaller than breeding pens

    sow.gest.section.vec <- rep(seq_len(sow.gestation.pens.section),
                                each = sow.gestation.pens.persection)

    gilt.gest.section.vec <- rep(seq_len(gilt.gestation.pens.section),
                                 each = gilt.gestation.pens.persection)

    ## Farrowing pens
    farrowing.section.vec <- rep(seq_len(farrowing.pens.sections),
                                 each = farrowing.pens.persection)

    ## Growing pens
    growing.section.vec <- rep(seq_len(growing.pens.sections),
                               each = growing.pens.persection)

    ## Growing buffer pens
    growing.buffer.section.vec <- rep(seq_len(growing.buffer.pens.sections),
                                      each = growing.buffer.pens.persection)

    ## Finishing pens
    finishing.section.vec <- rep(seq_len(finishing.pens.sections),
                                 each = finishing.pens.persection)

    ## Growing gilt section
    growing.gilt.vec <- rep(seq_len(growing.gilt.pens.sections),
                            each = growing.gilt.pens.persection)

    ## concatenating all the section number vectors together
    section<- c(sow.breed.section.vec,
                sow.breed.buffer.section.vec,
                gilt.breed.section.vec,
                gilt.breed.buffer.section.vec,
                sow.gest.section.vec,
                gilt.gest.section.vec,
                farrowing.section.vec,
                growing.section.vec,
                growing.buffer.section.vec,
                finishing.section.vec,
                growing.gilt.vec)

    ## Produce the pentypes
    pentype <- factor(c(rep(1,  length(sow.breed.section.vec)),
                        rep(2,  length(sow.breed.buffer.section.vec)),
                        rep(3,  length(gilt.breed.section.vec)),
                        rep(4,  length(gilt.breed.buffer.section.vec)),
                        rep(5,  length(sow.gest.section.vec)),
                        rep(6,  length(gilt.gest.section.vec)),
                        rep(7,  length(farrowing.section.vec)),
                        rep(8,  length(growing.section.vec)),
                        rep(9,  length(growing.buffer.section.vec)),
                        rep(10, length(finishing.section.vec)),
                        rep(11, length(growing.gilt.vec))),
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

    df <- data.frame(Ssows = 0,
                     Isows1 = 0,
                     Isows2 = 0,
                     Isows3 = 0,
                     Sgilts = 0,
                     Igilts1 = 0,
                     Igilts2 = 0,
                     Igilts3 = 0,
                     Spiglets = 0,
                     Ipiglets1 = 0,
                     Ipiglets2 = 0,
                     Ipiglets3 = 0,
                     Sgrowers = 0,
                     Igrowers1 = 0,
                     Igrowers2 = 0,
                     Igrowers3 = 0,
                     Sfinish = 0,
                     Ifinish1 = 0,
                     Ifinish2 = 0,
                     Ifinish3 = 0,
                     pentype = pentype,
                     section = section,
                     countdown = 99999)

    if (time) {
        time <- data.frame(time = rep(1, nrow(df)))
        df <- cbind(time, df)
    }

    if (node) {
        node <- data.frame(node = seq_len(nrow(df)))
        df <- cbind(node, df)
    }

    if (capacity) {
        capacity <- data.frame(capacity = pen_capacity(df$pentype))
        df <- cbind(df, capacity)
    }

    if (npigs) {
        npigs <- data.frame(npigs = rep(0, nrow(df)))
        df <- cbind(df, npigs)
    }

    if (phi) {
        phi <- data.frame(phi = rep(0, nrow(df)))
        df <- cbind(df, phi)
    }

    if (s_phi) {
        s_phi <- data.frame(s_phi = rep(0, nrow(df)))
        df <- cbind(df, s_phi)
    }

    if (f_phi) {
        f_phi <- data.frame(f_phi = rep(0, nrow(df)))
        df <- cbind(df, f_phi)
    }

    if (test_indicator) {
        test_indicator <- data.frame(test_indicator = rep(0, nrow(df)))
        df <- cbind(df, test_indicator)
    }

    if (result_indicator) {
        result_indicator <- data.frame(result_indicator = rep(0, nrow(df)))
        df <- cbind(df, result_indicator)
    }

    if (cleaning_indicator) {
        cleaning_indicator <- data.frame(cleaning_indicator = rep(0, nrow(df)))
        df <- cbind(df, cleaning_indicator)
    }

    if (Icum) {
        Icum <- data.frame(Icum = rep(0, nrow(df)))
        df <- cbind(df, Icum)
    }

    df
}
