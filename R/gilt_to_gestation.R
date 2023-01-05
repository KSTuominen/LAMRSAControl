##' gilt_to_gestation
##'
##' In this function we move the gilts to gestation from breeding and
##' deal with buffers in the gestation and breeding.
##'
##' @param result The result from the previous day or modified result
##'     from the current day.
##' @param residual The rows with timer == 0 that still need to be
##'     processed. The default is NULL if there is nothing to be done
##' @param events The events that have already been generated. The
##'     default is NULL in the case that no events have yet been
##'     generated for this day
##' @param biweekly Should the events we biweekly. Biweekly events
##'     means that sows only farrow every second week and that at the
##'     beginning of the model only 10 groups of gilts are added to
##'     the model in 14 day increments.
##' @return A list of the (modified) result, the new residual and the
##'     new events appended to the events you fed to the function
##' @export
gilt_to_gestation <- function(result,
                              residual = NULL,
                              events = NULL,
                              biweekly = FALSE) {

  index <- residual$pentype == "Gilt breeding"

  ## Check for anything to do, take only pens with animals in them

  if (sum(index) == 0) {
    return(list(result = result,
                residual = residual,
                events = events))
  }
  ## make this more general, so not row by row but for all gestation pens
  pigs.per.pen <- residual[index, "npigs"]
  total.to.move <-sum(pigs.per.pen)

  ## calculating how many animals in the pens are not pregnant and how many buffer pens they need
  empty <- pregnancy(total.to.move)

  ## save the departure pens
  dep <- residual[index, "node"]

  ## Find the number of pens that we need at the destination for buffer
  total.buffer.pens <- ceiling(empty / pen_capacity("Gilt breeding buffer"))

  ## Get the available gilt breeding buffer pens:
  empty.gilt.buffer.pens <- empty.pens(result, "Gilt breeding buffer", countdown_check = FALSE)

  ## Check that there are enough pens in the buffer and throw error if not
  stopifnot(length(empty.gilt.buffer.pens) >= total.buffer.pens)
  empty.gilt.buffer.pens <- empty.gilt.buffer.pens[seq_len(total.buffer.pens)]

  ## dataframe with only pen ids of the pens to be sampled and
  ## sum of both susceptible and infected piglets
  sampling.gilts <- data.frame(node = dep,
                               pigs = pigs.per.pen,
                               stringsAsFactors = FALSE)

  ## Divide pigs going to buffer evenly among dest pens
  dest.pens.vec <- rep(empty.gilt.buffer.pens,
                       length.out = empty)

  ## samples the pigs from the source pens. Having only one pig in one pen needs to be handled
  ## separately, because otherwise R samples from 1:penID instead from than penId only
  if(nrow(sampling.gilts) == 1 & sampling.gilts$pigs[1] == 1){
    source_pens <- sampling.gilts$node
  }

  else{
    source_pens <- sample(unlist(mapply(rep,
                                        times = sampling.gilts$pigs,
                                        x = sampling.gilts$node)),
                          empty,
                          replace = FALSE)
  }

  ## Keep track of those that have been removed
  removed <- table(source_pens)
  removed <- removed[match(sampling.gilts$node, names(removed))]
  removed[is.na(removed)] <- 0
  sampling.gilts$pigs <- sampling.gilts$pigs - removed

  if(empty > 0) {
    event1 <- event(type = 5,
                    time = result$time[1],
                    node = source_pens,
                    dest = dest.pens.vec,
                    n = 1,
                    proportion = 0)
    events <- rbind(events, event1)
  }


  ## Set the timers for the destination pens
  result$countdown[result$node %in% dest.pens.vec] <- timers("Breeding buffer", biweekly = biweekly)

  ## How many gestation pens do we need for the remainder that are
  ## going onto gestation?
  npens <- ceiling(sum(sampling.gilts$pigs) / pen_capacity("Gilt gestation"))

  ## Get some empty pens:
  empty.gilt.gest.pens <- sectioning(result, "Gilt gestation", npens, events)

  ## stop if there are fewer than needed
  stopifnot(length(empty.gilt.gest.pens) >= npens)

  ## keep just those needed
  dest.pens.vec <- empty.gilt.gest.pens[seq_len(npens)]

  ## determine where to put which animal:
  dest.pens.vec <- sort(rep(dest.pens.vec,
                            length.out = sum(sampling.gilts$pigs)))

  ## And which animals to move
  source.pens.vec <- unlist(mapply(rep,
                                   times = sampling.gilts$pigs,
                                   x = sampling.gilts$node))

  if(sum(sampling.gilts$pigs) > 0) {
    event1 <- event(type = 5,
                    time = result$time[1],
                    node = source.pens.vec,
                    dest = dest.pens.vec,
                    n = 1,
                    proportion = 0)
    events <- rbind(events, event1)

  }

  ## set timers
  result$countdown[result$node %in% dest.pens.vec] <- timers("Gestation", biweekly = biweekly)

  ## set the downtown in all the source pens at once
  result$countdown[result$node %in% sampling.gilts$node] <- timers("Breeding dt", biweekly = biweekly)

  ## modify the residual and return
  residual <- residual[!index, ]
  return(list(result = result,
              residual = residual,
              events = events))
}
