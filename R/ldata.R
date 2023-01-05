#' ldata
#'
#' Generates dataframe that is needed for SimInf model run to describe the node
#' level data
#'
#' @return data.frame
ldata <- function() {
    df <- u0(node = TRUE)

    ## These pen types are grouped into the same sections:
    unit <- factor(df$pentype,
                   levels = c("Sow breeding",
                              "Sow breeding buffer",
                              "Gilt breeding",
                              "Gilt breeding buffer",
                              "Sow gestation",
                              "Gilt gestation",
                              "Farrowing",
                              "Growing",
                              "Growing buffer",
                              "Finishing",
                              "Gilt growing"),
                   labels = c("mating",
                              "mating",
                              "mating",
                              "mating",
                              "mating",
                              "mating",
                              "Farrowing",
                              "Growing",
                              "Growing buffer",
                              "Finishing",
                              "Gilt growing"))
    df$unit <- paste(unit, df$section, sep = "-")

    ## Count the number of pens in each section and keep them in
    ## order.
    reps <- sapply(unique(df$unit), function(x) {
        nrow(df[df$unit == x, ])
    })

    ## Get the last pen of each section
    last <- cumsum(reps)

    ## Get the first pen of each section
    first <- c(1, (last + 1)[-(length(last))])

    ## Repeat the first and last pens for the number of pens in each section
    first <- mapply(rep, x = first, times = reps)
    last <- mapply(rep, x = last, times = reps)

    ## put in a dataframe and add the first and last of the farm
    ldata <- data.frame(section_first = unlist(first),
                        section_last = as.numeric(unlist(last)),
                        farm_first = 1,
                        farm_last = as.numeric(nrow(df)))

    ## subtract 1 from the columns to make indexing in c more easier
    ldata - 1
}
