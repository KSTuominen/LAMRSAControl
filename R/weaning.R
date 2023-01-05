##' weaning
##'
##' In this function we:
##'  - wean pigs to the grower and to the gilt grower.
##'  - wean sows to the breeding unit and cull sows that are to be culled
##'  - schedule events to bring in new gilts from gilt growing unit if we need refilling
##'  - refilling need is based on the total number of sows coming from farrowing to breeding and
##'  - sows in breeding buffer pens
##' All of these need to be together in one function as they have feedback on one one another.
##'
##' @param result The result from the previous day or modified result
##'     from the current day.
##' @param residual The rows with timer == 0 that still need to be
##'     processed. The default is NULL if there is nothing to be done
##' @param events The events that have already been generated. The
##'     default is NULL in the case that no events have yet been
##'     generated for this day
##' @param fraction_to_gilt the proportion of pigs to be moved to gilt growing
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @importFrom utils tail
##' @export
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
weaning <- function(result,
                    residual = NULL,
                    events = NULL,
                    fraction_to_gilt = 0.05,
                    biweekly = FALSE) {

    if (is.null(residual)) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    index <- (residual$pentype == "Farrowing") & (residual$npigs !=0)

    if (sum(index) == 0) {
        return(list(result = result,
                    residual = residual,
                    events = events))
    }

    ##  Weaning piglets and sampling for new gilt growers
    #####################################################

    ## how many piglet pens need to be weaned today
    total.pens.to.weaning <- sum(residual$pentype == "Farrowing" & (residual$Spiglets !=0 | (residual$Ipiglets1 + residual$Ipiglets2 + residual$Ipiglets3) !=0))

    ## how many piglets we have to wean in total
    ## can't use "index" in here because we don't want to count breeding buffers
    pens <- residual[residual$pentype == "Farrowing" & (residual$Spiglets !=0 | (residual$Ipiglets1 + residual$Ipiglets2 + residual$Ipiglets3) !=0), c("Spiglets", "Ipiglets1", "Ipiglets2", "Ipiglets3")]
    pigs.per.pen <- rowSums(pens)
    total.piglets.to.wean <-sum(pigs.per.pen)

    ## pen ids of the departure pens
    weaning.pen.id <- residual[residual$pentype == "Farrowing" & (residual$Spiglets !=0 | (residual$Ipiglets1 + residual$Ipiglets2 + residual$Ipiglets3) !=0),"node"]

    ## expected number to gilt growing
    piglet.to.gilt <- round(total.piglets.to.wean * fraction_to_gilt)

    ## Number of gilt growing pens required given the capacity
    npens.gilt <- ceiling(piglet.to.gilt / pen_capacity("Gilt growing"))

    ## finding destination pens, only move animals if we have enough destination pens
    empty.gilt.growing <- empty.pens(result, "Gilt growing", countdown_check = TRUE)[seq_len(npens.gilt)]

    ## if there is no space in the gilt grower then don't move there
    if(any(is.na(empty.gilt.growing)) | is.null(empty.gilt.growing)) {
        piglet.to.gilt <- 0
    }

    source_pens <- sample(unlist(mapply(rep,
                                        times = pigs.per.pen,
                                        x = names(pigs.per.pen))),
                          piglet.to.gilt,
                          replace = FALSE)

    ## finding empty growing pens from one section for the ones that are weaned "normally
    empty.growing.pens <- sectioning(result, "Growing", 0)[seq_len(total.pens.to.weaning)]

    ## Sort out which pigs from the source pens are going which way
    weaned_pigs <- table(source_pens)
    weaned_pigs <- weaned_pigs[match(names(pigs.per.pen), names(weaned_pigs))]
    weaned_pigs[is.na(weaned_pigs)] <- 0
    weaned_pigs <- data.frame(node = as.numeric(rep(names(pigs.per.pen), 2)),
                              n = c(as.integer(weaned_pigs), pigs.per.pen - as.integer(weaned_pigs)),
                              type = rep(c(11, 8), each = length(pigs.per.pen)),
                              stringsAsFactors = FALSE)
    weaned_pigs <- weaned_pigs[weaned_pigs$n != 0, ]
    ## pick pens for gilts
    weaned_pigs$dest <-NA
    ## If one gilt destination pen then don't sample. (See help on sample())
    if(length(empty.gilt.growing) > 1) {
        weaned_pigs$dest[weaned_pigs$type == 11] <- sample(empty.gilt.growing, sum(weaned_pigs$type == 11), replace = TRUE)
    }
    if(length(empty.gilt.growing) == 1) {
        weaned_pigs$dest[weaned_pigs$type == 11] <- empty.gilt.growing
    }
    ## pick pens for growers
    weaned_pigs$dest[weaned_pigs$type == 8] <- empty.growing.pens[seq_len(sum(weaned_pigs$type == 8))]

    ## check that you don't index too many pens in the
    ## sectioning_multi vector
    stopifnot(all(!is.na(empty.growing.pens)))

    ## create the events:
    event1 <- event(type = weaned_pigs$type,
                    time = result$time[1],
                    node = weaned_pigs$node,
                    dest = weaned_pigs$dest,
                    n = weaned_pigs$n,
                    proportion = 0)
    events <- rbind(events, event1)
    ## Set the timers for the destination pens
    result$countdown[result$node %in% weaned_pigs$dest[weaned_pigs$type == 11]] <- timers("Gilt growing", biweekly = biweekly)
    result$countdown[result$node %in% weaned_pigs$dest[weaned_pigs$type == 8]] <- timers("Growing", biweekly = biweekly)

    ## Moving sows from farrowing to breeding (incl. removal)
    ## merging animals from breeding buffers
    ## filling the missing amount of animals from growing gilts
    ##########################################################
    total.sows.to.breeding <- SI.pentype(residual, "Sows", "Farrowing")
    total.sows.from.buffer <- SI.pentype(result, "Sows", "Sow breeding buffer")
    total.gilts.from.buffer <- SI.pentype(result, "Gilts", "Gilt breeding buffer")
    total.buffer <- total.sows.from.buffer + total.gilts.from.buffer
    sows.in.buffer <- result[result$pentype == "Sow breeding buffer" & (result$Ssows != 0 | (result$Isows1 + result$Isows2 + result$Isows3) != 0), ]
    sows.in.buffer <- sows.in.buffer[order(sows.in.buffer$countdown, decreasing = FALSE),]
    gilts.in.buffer <- result[result$pentype == "Gilt breeding buffer" & (result$Sgilts != 0 | (result$Igilts1 + result$Igilts2 + result$Igilts3) != 0), ]
    gilts.in.buffer <- gilts.in.buffer[order(gilts.in.buffer$countdown, decreasing = FALSE),]

    ## priorities 1. sows from farrowing 2. sows/gilts from buffer units 3. refill if space

    if (total.sows.to.breeding > 0) {
        ## Part of the sows are going to be removed from the herd at weaning
        ## sampling the amount of animals and their departure pens
        farrowing.pen.ids <- residual[residual$pentype == "Farrowing" & (residual$Ssows !=0 | (residual$Isows1 + residual$Isows2 + residual$Isows3) !=0), "node"]
        remove.sows <- sow.culling(farrowing.pen.ids)

        ## generating events for sows to be removed
        ## it doesn't matter if it's null, then we don't create anything
        event1 <- event(type = 14,
                        time = result$time[1],
                        node = remove.sows,
                        dest = 0,
                        n = 1,
                        proportion = 0)
        events <- rbind(events, event1)

        result$countdown[result$node %in% remove.sows] <- timers("Farrowing dt", biweekly = biweekly)

        ## calculating how many sows we have left to move after the removal
        total.sows.to.breeding <- total.sows.to.breeding - length(remove.sows)

        ## removing the removed sows from the pen list that will be weaned
        sow.pens.to.breeding <- farrowing.pen.ids[!(farrowing.pen.ids %in% remove.sows)]

        ## calculating how many pens we need for the weaned sows
        npens.sow <- total.sows.to.breeding %/% pen_capacity("Sow breeding")
        if(total.sows.to.breeding %% pen_capacity("Sow breeding") != 0) {
            npens.sow <- npens.sow + 1
        }
        if(total.sows.to.breeding < pen_capacity("Sow breeding")){
            npens.sow <- 1
        }

        ## using free pens function instead of sectioning because we group up many animals into one pen
        empty.breeding.pen <- empty.pens(result, "Sow breeding", countdown_check = FALSE)[seq_len(npens.sow)]

        ## creating a destination pen vector for the sows
        breeding.div <- total.sows.to.breeding %/% pen_capacity("Sow breeding")
        breeding.remain <- total.sows.to.breeding %% pen_capacity("Sow breeding")
        div.vec <- NULL
        if (breeding.div == 0){
            dest.pens.vec <- rep(empty.breeding.pen, total.sows.to.breeding)
        }
        else {
            for (i in 1:breeding.div){
                temp <- rep(empty.breeding.pen[i], pen_capacity("Sow breeding"))
                div.vec <- c(div.vec, temp)
            }
            breeding.remain.vec <- rep(tail(empty.breeding.pen, 1), breeding.remain)
            dest.pens.vec <- c(div.vec, breeding.remain.vec)
        }

        ## check for NA in empty pens using check.pens()-function
        checkforspace <- check.pens(npens.sow, empty.breeding.pen)

        if (checkforspace==FALSE){
            sow.breeding.nospace <- rbind(sow.breeding.nospace, result$time[1])
        }

        if (checkforspace) {
            ## create the events
            event1 <- event(type = 3,
                            time = result$time[1],
                            node = sow.pens.to.breeding,
                            dest = dest.pens.vec,
                            n = 1,
                            proportion = 0)

            events <- rbind(events, event1)

            ## setting timers for the destination pens
            result$countdown[result$node %in% empty.breeding.pen] <- timers("Breeding", biweekly = biweekly)

            ## timer for the pen to be empty before new animals.
            result$countdown[result$node %in% sow.pens.to.breeding] <- timers("Farrowing dt", biweekly = biweekly)

            ## calculating the amount of space and fillings we need from other pentypes
            #########
            ## how much room we have in a pen after sows come from farrowing
            filling <- total.breeding()-total.sows.to.breeding

            ## will we get enough animals from buffer to fill the total breeding rate
            buffer.balance <- filling - total.buffer

            new.in <- integer(0)
            ## do we need to get new animals from gilt growing and how many
            if (buffer.balance >= 0){
                new.in <- buffer.balance
            }
            else {
                new.in <- 0
            }

            ## if we have excess amount of animals in buffer, only take what is needed
            ## prioritize animals with smaller timer
            if (total.buffer >= filling){
                if (total.sows.from.buffer >= filling){
                    sow.buffer.to.breed <- filling
                    gilt.buffer.to.breed <- 0
                }
                else{
                    sow.buffer.to.breed <- total.sows.from.buffer
                    gilt.buffer.to.breed <- filling - total.sows.from.buffer
                }
            }
            ## if buffer has less animals than we want, take all we can from buffers
            else {
                sow.buffer.to.breed <- total.sows.from.buffer
                gilt.buffer.to.breed <- total.gilts.from.buffer
            }

            ## if we also have animals in breeding buffers, move them to the same pen

            if(sow.buffer.to.breed > 0){

                ## creating a vector of free space in the pens that is left after sows are weaned
                if (breeding.remain != 0){
                    last.pen <- tail(empty.breeding.pen, 1)
                    free.space <- pen_capacity("Sow breeding") - breeding.remain
                    free.space.vec <- rep(last.pen, free.space)
                }
                else {
                    free.space.vec <- integer(0)
                }

                pens <- sows.in.buffer[, c("Ssows", "Isows1", "Isows2", "Isows3")]

                if(nrow(pens) > 0){
                    ## animals per pen
                    pigs.per.pen <- rowSums(pens)
                    ## sum of all to see if we have enough
                    total.sows <- sum(pigs.per.pen)
                    ## pen ids of the departure pens
                    sow.pen.id <- sows.in.buffer$node
                    ## creating vector of departure pens and crop it to the needed length.
                    ## Also save the unused ones for determining later which departure pens have gone empty
                    all_dep <- rep(sow.pen.id, pigs.per.pen)
                    dep <- all_dep[1:sow.buffer.to.breed]
                    stopifnot(length(dep) <= length(all_dep))
                    if(length(dep) < length(all_dep)){
                        rest <- all_dep[seq((sow.buffer.to.breed +1), length(all_dep))]
                    }
                    else{
                        rest <- NULL
                    }


                    ## find the destination pens and generate the events.
                    ## If we have space left after weaning sows, combine the buffer sows with them
                    if (total.sows.to.breeding > 0){
                        if ((length(free.space.vec) > 0) & (length(free.space.vec) >= sow.buffer.to.breed)){
                            dest.pens.vec <- free.space.vec[seq_len(sow.buffer.to.breed)]
                        }

                        ## when we don't have enough room in the same pens with weaned sows, we need to find new ones
                        else {
                            npens.sow <- sow.buffer.to.breed %/% pen_capacity("Sow breeding")
                            if(sow.buffer.to.breed %% pen_capacity("Sow breeding") != 0) {
                                npens.sow <- npens.sow + 1
                            }
                            if(sow.buffer.to.breed < pen_capacity("Sow breeding")){
                                npens.sow <- 1
                            }

                            ## using empty pens function so that we won't try to put sows into the already filled pen(s)
                            empty.breeding.pen <- empty.pens(result, "Sow breeding", events)[seq_len(npens.sow)]

                            ## creating a destination pen vector for the sows
                            breeding.div <- sow.buffer.to.breed %/% pen_capacity("Sow breeding")
                            breeding.remain <- sow.buffer.to.breed %% pen_capacity("Sow breeding")

                            # creating a vector of destination pens
                            div.vec <- NULL

                            if (breeding.div == 0){
                                dest.pens.vec <- rep(empty.breeding.pen, sow.buffer.to.breed)
                            }
                            else {
                                for (i in 1:breeding.div){
                                    temp <- rep(empty.breeding.pen[i], pen_capacity("Sow breeding"))
                                    div.vec <- c(div.vec, temp)
                                }
                                breeding.remain.vec <- rep(tail(empty.breeding.pen, 1), breeding.remain)
                                dest.pens.vec <- c(div.vec, breeding.remain.vec)
                            }
                        }

                        ## check for NA in empty pens using check.pens()-function
                        checkforspace <- check.pens(npens.sow, empty.breeding.pen)

                        if (checkforspace==FALSE){
                            sow.breeding.nospace <- rbind(sow.breeding.nospace, result$time[1])
                        }

                        if (checkforspace){
                            ## generate the events
                            event1 <- event(type = 1,
                                            time = result$time[1],
                                            node = dep,
                                            dest = dest.pens.vec,
                                            n = 1,
                                            proportion = 0)

                            events <- rbind(events, event1)

                            result$countdown[result$node %in% empty.breeding.pen] <- timers("Breeding", biweekly = biweekly)

                            ## set timer only for those breeding pens that end up empty
                            emptied_pens <- unique(all_dep[!(all_dep %in% rest)])
                            if(length(emptied_pens)>0){
                                result$countdown[result$node %in% emptied_pens] <- timers("Breeding dt", biweekly = biweekly)
                            }
                        }
                    }
                }
            }


            ## if we have gilts to inseminate, move them from buffer to gilt breeding pen
            ## (separate from sows)
            if (gilt.buffer.to.breed > 0){
                pens <- gilts.in.buffer[, c("Sgilts", "Igilts1", "Igilts2", "Igilts3")]
                if(nrow(pens) > 0){
                    ## animals per pen
                    pigs.per.pen <- rowSums(pens)
                    ## sum of all to see if we have enough
                    total.gilts <- sum(pigs.per.pen)
                    ## pen ids of the departure pens
                    gilt.pen.id <- gilts.in.buffer$node

                    ## creating vector of departure pens and crop it to the needed length.
                    ## Also save the unused ones for determining later which departure pens have gone empty
                    all_dep <- rep(gilt.pen.id, pigs.per.pen)
                    dep <- all_dep[1:gilt.buffer.to.breed]
                    stopifnot(length(dep) <= length(all_dep))
                    if(length(dep) < length(all_dep)){
                        rest <- all_dep[seq((gilt.buffer.to.breed +1), length(all_dep))]
                    }
                    else{
                        rest <- NULL
                    }
                    ## creating vector of departure pens, crop it to the amoung of animals we want to move
                    dep <- rep(gilt.pen.id, pigs.per.pen)[1:gilt.buffer.to.breed]

                    ## how many gilt breeding pens are needed
                    npens.gilt <- gilt.buffer.to.breed %/% pen_capacity("Gilt breeding")
                    if(gilt.buffer.to.breed %% pen_capacity("Gilt breeding") != 0) {
                        npens.gilt <- npens.gilt + 1
                    }
                    if(gilt.buffer.to.breed < pen_capacity("Gilt breeding")){
                        npens.gilt <- 1
                    }
                    ## find empty destination pen(s)
                    empty.gilt.breeding <- empty.pens(result, "Gilt breeding", events)[seq_len(npens.gilt)]

                    ## creating a destination pen vector for the gilts
                    breeding.div <- gilt.buffer.to.breed %/% pen_capacity("Gilt breeding")
                    breeding.remain <- gilt.buffer.to.breed %% pen_capacity("Gilt breeding")

                    # creating a vector of destination pens
                    div.vec <- NULL

                    if (breeding.div == 0){
                        dest.pens.vec <- rep(empty.gilt.breeding, gilt.buffer.to.breed)
                    }
                    else {
                        for (i in 1:breeding.div){
                            temp <- rep(empty.gilt.breeding[i], pen_capacity("Gilt breeding"))
                            div.vec <- c(div.vec, temp)
                        }
                        breeding.remain.vec <- rep(tail(empty.gilt.breeding, 1), breeding.remain)
                        dest.pens.vec <- c(div.vec, breeding.remain.vec)
                    }

                    ## check for NA in empty pens using check.pens()-function
                    checkforspace <- check.pens(1, empty.gilt.breeding)

                    if (checkforspace==FALSE){
                        gilt.breeding.nospace <- rbind(gilt.breeding.nospace, result$time[1])
                    }

                    if(checkforspace) {
                        ## create the events
                        event1 <- event(type = 5,
                                        time = result$time[1],
                                        node = dep,
                                        dest = dest.pens.vec,
                                        n = 1,
                                        proportion = 0)

                        events <- rbind(events, event1)

                        ## setting timers for the destination pens
                        result$countdown[result$node %in% empty.gilt.breeding] <- timers("Breeding", biweekly = biweekly)

                        ###  only set timer for departure pen if it has gotten empty
                        emptied_pens <- unique(all_dep[!(all_dep %in% rest)])
                        if(length(emptied_pens) > 0){
                            result$countdown[result$node %in% emptied_pens] <- timers("Breeding dt", biweekly = biweekly)
                        }

                        ## if we also need to add animals from gilt growing to breeding buffer,


                        ## if we also need to add animals from gilt growing to breeding buffer,
                        ## save the booked pens and how many animals we are moving and calculate room left

                        if (new.in > 0){
                            if (breeding.remain != 0){
                                last.pen <- tail(empty.gilt.breeding, 1)
                                free.space <- pen_capacity("Gilt breeding") - breeding.remain
                                free.space.vec <- rep(last.pen, free.space)
                            }
                            else {
                                free.space.vec <- integer(0)
                            }
                            ## create a vector for the free space in gilt breeding after buffer movements
                            free.space.vec <- rep(empty.gilt.breeding, free.space)
                        }
                    }
                }
            }

            ## Try to put new gilts into the same pen with gilts from buffer
            ## do nothing if we don't have animals left to move
            if (new.in > 0){
                ## make a temporary dataframe for growing gilts
                growing.gilts <- result[result$pentype == "Gilt growing" & (result$Sgilts !=0 | (result$Igilts1 + result$Igilts2 + result$Igilts3) !=0), ]
                growing.gilts <- growing.gilts[order(growing.gilts$countdown, decreasing = FALSE), ]
                pens <- growing.gilts[, c("Sgilts", "Igilts1", "Igilts2", "Igilts3")]

                if(nrow(pens) > 0){
                    ## animals per pen
                    pigs.per.pen <- rowSums(pens)
                    ## sum of all to see if we have enough
                    total.gilts <- sum(pigs.per.pen)
                    ## pen ids of the departure pens
                    gilt.pen.id <- growing.gilts$node

                    ## check if we are capable of giving as many animals as needed, if not, give what is possible
                    if(new.in > total.gilts){
                        new.in <- total.gilts
                    }

                    ## creating vector of departure pens and crop it to the needed length.
                    ## Also save the unused ones for determining later which departure pens have gone empty

                    all_dep <- rep(gilt.pen.id, pigs.per.pen)
                    dep <- all_dep[1:new.in]
                    stopifnot(length(dep) <= length(all_dep))
                    if(length(dep) < length(all_dep)){
                        rest <- all_dep[seq((new.in+1), length(all_dep))]
                    }
                    else{
                        rest <- NULL
                    }

                    ## creating vector of departure pens and crop it to the length we need
                    dep <- rep(gilt.pen.id, pigs.per.pen)[1:new.in]

                    ## find the destination pens and generate the events.
                    ## different ways of generating destination pens depending on if we are moving gilts from breeding buffer at the same time
                    ## if we have also animals coming from gilt breeding buffer, fill in also the extra space left in their destination pens
                    if (gilt.buffer.to.breed > 0){
                        ## use previously generated destination pen list cropped to the length of need
                        ## if we have animals coming from breeding buffer at the same time
                        dest.pens.vec <- free.space.vec[new.in]

                        ## generate events for new breeding gilts
                        event1 <- event(type = 4,
                                        time = events$time[1],
                                        node = dep,
                                        dest = dest.pens.vec,
                                        n = 1,
                                        proportion = 0)

                        events <- rbind(events, event1)

                        result$countdown[result$node %in% empty.gilt.breeding] <- timers("Breeding", biweekly = biweekly)

                    }

                    ## if we have no gilts coming from buffer, just move the growing gilts into new pens
                    else{
                        ## how many pens we need in total
                        npens.gilt <- new.in %/% pen_capacity("Gilt breeding")
                        if(new.in %% pen_capacity("Gilt breeding") != 0) {
                            npens.gilt <- npens.gilt + 1
                        }
                        if(new.in < pen_capacity("Gilt breeding")){
                            npens.gilt <- 1
                        }

                        dest.pens <- empty.pens(result, "Gilt breeding", countdown_check = FALSE)[seq_len(npens.gilt)]

                        ## checking if we got NAs into destination pens
                        checkforspace.gilt <- check.pens(npens.gilt, dest.pens)

                        ## generate events if we have empty destination pens
                        if(checkforspace.gilt) {

                            ## create a vector for the destination pens
                            dest.pens.vec <- integer(0)
                            for(i in 1:length(dest.pens)){
                                temp <- c(rep(dest.pens[i], pen_capacity("Gilt breeding")))
                                dest.pens.vec <- c(dest.pens.vec, temp)
                            }
                            dest.pens.vec <- dest.pens.vec[seq_len(new.in)]

                            ## create events
                            event1 <- event(type = 4,
                                            time = result$time[1],
                                            node = dep,
                                            dest = dest.pens.vec,
                                            n = 1,
                                            proportion = 0)

                            events <- rbind(events, event1)
                            result$countdown[result$node %in% dest.pens] <- timers("Breeding", biweekly = biweekly)
                        }
                    }
                    ## setting a timer for the gilt growing departure pens if they end up empty
                    emptied_pens <- unique(all_dep[!(all_dep %in% rest)])
                    if(length(emptied_pens) > 0){
                        result$countdown[result$node %in% emptied_pens] <- timers("Gilt growing dt", biweekly = biweekly)
                    }
                }
            }
        }
    }

    residual <- residual[!index, ]
    return(list(result = result,
                residual = residual,
                events = events))
}

#' SI.pentype
#'
#' function for returning back the sum of of animals both susceptible and infected compartments
#' from all pens with given pentype
#' instead of having to type then separately
#' @param x A dataframe which is the output from the trajectory of
#'     the MRSA model from 1 day.
#' @param SI.comp A string
#' @param pentype A string'
#' @return an integer, sum of animals
SI.pentype <- function(x,
                       SI.comp = c("Sows",
                                   "Gilts",
                                   "Piglets",
                                   "Growers",
                                   "Finish"),
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
                                   "Gilt growing")){

    ## check that the compartment names and pentype are default values
    SI.comp <- match.arg(SI.comp)
    pentype <- match.arg(pentype)

    ## changing the general compartment title into the susceptible and infected one
    SI.comp <- switch(SI.comp,
                      'Sows' = list("Ssows", "Isows1", "Isows2", "Isows3"),
                      'Gilts' = list("Sgilts", "Igilts1", "Igilts2", "Igilts3"),
                      'Piglets' = list("Spiglets", "Ipiglets1", "Ipiglets2", "Ipiglets3"),
                      'Growers' = list("Sgrowers", "Igrowers1", "Igrowers2", "Igrowers3"),
                      'Finish' = list("Sfinish", "Ifinish1", "Ifinish2", "Ifinish3"))

    ## indexing only the rows that match the desired pentype
    rows <- x[x$pentype == pentype,]

    SI.sum.pentype(rows, SI.comp[[1]], SI.comp[[2]]) ## send the parameters to SI sum to get the sum of the animals in these compartments

}
#' SI.sum.pentype
#'
#' function to sum up animals from the 2 compartments combined in SI.pentype
#' @param rows A dataframe
#' @param Scomp A string
#' @param Icomp A string
#' @return an integer, sum of animals

SI.sum.pentype <- function(rows, Scomp, Icomp){
    sum(rows[,Scomp], rows[, Icomp])
}

#'  sow.culling
#'
#'  Function to calculate how many sows to remove at farrowing
#'  and which pens to take them from
#'
#' According to Engblom et al. 2007, average annual removal rate for
#' sows was 49.5 % of these 26.9 % were of reproductive disorders.
#' If we assume, that the reproductive ones are removed at time of the gestation
#' (but the probability in the code hasn't been adjusted accordingly),
#' then 36.18 % of the sows are removed annually at the time of the weaning.
#' When the average amount of litters per sow is 2.23 (winpig 2019),
#' this gives the culling ratio of  ((36.18/2.23)/100 = 0.16 after each weaning
#' If one sows has average of 2.23 litters per year
#' (winpig 2019) According to Winpig 2019 statistics, the
#' average % of gilt litters in the herds was 24.2 %, this is fairly close to the
#' 22.2 % culling that comes up when we get 2.23 litters per year
#'
#' @param pen.id integer, vector of pen ids that are being weaned
#' @param culling_rate integer, rate of culling per weaning
#'
#' @return vector of integers (pen ids)
sow.culling <- function(pen.id,
                        culling_rate = 0.184) {

    ## Find the mean of the poisson distribution that you will sample
    ## from to determine the number of sows to cull
    removed.mean <- culling_rate * length(pen.id)

    ## Sample from that distribution to determine the number of sows
    removed.tot <- rpois(1, removed.mean)
    if(removed.tot > length(pen.id)) {
        removed.tot <- length(pen.id)
    }
    if(removed.tot == 0) return(NULL)

    ## Return the pens
    sample(pen.id, removed.tot)
}

#' total.breeding
#'
#' Calculate the farrowing rate
#' the average farrowing rate for a sow/piglet according to Winpig
#' 2019 is 86.8 %
#'
#' @param farrowing.rate the proportion of sows that are bred that
#'     successfully farrow
#' @param farrowing.pen.capacity the number of pens in a farrowing
#'     room
#' @return The number to breed in a week
total.breeding <- function(farrowing.rate = 0.868,
                           farrowing.pen.capacity = 22) {
    stopifnot(farrowing.rate <= 1 & farrowing.rate > 0)
    round(farrowing.pen.capacity/farrowing.rate)
}

#' check.pens
#'
#' function to check if there is space in farrowing pens

#' @param animals integer of the amount of animals in departure pen
#' @param dest.pen vector of pen ids
#' @return boolean

check.pens<- function(animals, dest.pen){

    ## check if you have enough empty pens
    empty.pens <- !is.na(dest.pen)
    empty.pens <- empty.pens && length(dest.pen) >= animals
    return(empty.pens)
}
