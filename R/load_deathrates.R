#' Load deathrates
#'
#' Taking the input of a country or iso3c code, load the deathrates in
#' the format required by [malariasimulation](https://github.com/mrc-ide/malariasimulation/).
#'
#' @param iso3c Country code to subset deathrates by.
#' @param country Country name to subset deathrates by.
#' @param year Year to subset deathrates by. Can be single value or vector.
#'
#' @return Data.frame of deathrates for country.
#' @export
load_deathrates <- function(iso3c = NULL, country = NULL, year = NULL) {
  deathrates <- peeps::mortality_rates
  
  if(!is.null(iso3c) & !is.null(country)){
    stop("Please provide iso3c code or country name")
  }
  if(!is.null(iso3c)){
    deathrates <- deathrates[deathrates$iso3c %in% iso3c, ]
  }
  if(!is.null(country)){
    deathrates <- deathrates[deathrates$country %in% country, ]
  }
  
  if(!is.null(year)){
    deathrates <- deathrates[deathrates$year %in% year, ]
  }
 
  return(deathrates)
}