#' Equilibrium age distribution
#'
#' Estimate the equilbrium age distribution given a lifetable (probability, of death in each year).
#' Life table raw data can be obtained from http://apps.who.int/gho/data/node.main.687?lang=en. The input
#' used is the "nqx - probability of dying between ages x and x+n" column, which is then smoothly
#' extrapolated to yearly probabilities.
#'
#' @param life_table Vector of yearly death probability for 0-90.
#'
#' @return Proportion of popution in each age group (at equilibrium)
#' @export
equilibrium_age_distribution <- function(life_table){
  n <- length(life_table)
  
  # convert life table to transition matrix
  m <- lifetable_to_transtion_matrix(life_table)
  
  # convert to rates
  r = m - diag(n)
  
  # compute Eigenvalues of the rate matrix
  E = eigen(t(r))
  
  # there should be one Eigenvalue that is zero (up to limit of computational
  # precision). Find which Eigenvalue this is
  w <- which.min(abs(E$values))
  
  # the stable solution is the corresponding Eigenvector, suitably normalised
  age_stable <- Re(E$vectors[,w] / sum(E$vectors[,w]))
  
  return(age_stable)
}

#' Lifetable to transition matrix
#'
#' @inheritParams equilibrium_age_distribution
#'
#' @return Transition matrix
lifetable_to_transtion_matrix <- function(life_table){
  n <- length(life_table)
  m <- matrix(0, n, n)
  m[col(m) == (row(m) + 1)] <- 1 - life_table[1:(n - 1)]
  m[,1] <- 1 - rowSums(m)
  return(m)
}

#' optimisation objective function
#'
#' @param mortality_rates Mortality rates
#' @param target_age_distribution Target age distribution
#'
#' @return The sum of squared  differences between the target and current age distributions
optim_objective <- function(mortality_rates, target_age_distribution){
  sum((equilibrium_age_distribution(mortality_rates) - target_age_distribution) ^ 2)
}

#' Estimate mortality rates
#' 
#' Optimises the mortality rates to reproduce a target (equilibrium) age distribution
#'
#' @param starting_mortality_rates A vector of mortality rates to initialise the optimisation. 
#' This is recommended to be the variable qx from the UN WPP life tables
#' @param target_age_distribution Target age distribution. The proportion of the
#' population in each age band. This should sum to 1.
#'
#' @return The sum of squared  differences between the target and current age distributions
#' @export
estimate_mortality_rates <- function(target_age_distribution, starting_mortality_rates){
  stopifnot(length(target_age_distribution) == length(starting_mortality_rates))
  
  opt <- stats::optim(
    par = starting_mortality_rates,
    fn = optim_objective,
    method = "L-BFGS-B",
    lower = 1 / 10000,
    upper = 0.9,
    target_age_distribution = target_age_distribution
  )
  
  return(opt$par)
}