#' pregnancy
#'
#' Function to sample how many animals in given pen are not
#' pregnant. Sums the amount of animals of given category from given
#' node. Mean omloepningsprocent is 5.5 % in Winpig at 2019
#'
#' @param pigs an integer of the amount of animals in pen
#' @param failure_rate The proportion of failures
#' @return an integer representing the amount of animals that are not
#'     pregnant in the given pen
#' @export
pregnancy <- function(pigs,
                      failure_rate = 0.055){
    rbinom(1, pigs, failure_rate)
}
