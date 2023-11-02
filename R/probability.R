#' Convert a probability between time intervals
#'
#' @param p Probability
#' @param interval_in Interval in
#' @param interval_out Interval out (in same time units as interval in)
#'
#' @return Rescaled probability
#' @export
rescale_prob <- function(p, interval_in, interval_out){
  1 - (1 - p) ^ (interval_out / interval_in)
}