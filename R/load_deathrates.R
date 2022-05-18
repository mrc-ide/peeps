#' Load deathrates
#'
#' Taking the input of a country or iso3c code, load the deathrates in
#' the format required by [malariasimulation](https://github.com/mrc-ide/malariasimulation/).
#'
#' @param iso3c Country code to subset deathrates by.
#' @param country Country name to subset deathrates by.
#' @param year Year to subset deathrates by. Can be single value or vector.
#' If format = 'asmr', year data is given in 5 year chunks. If format = 'nmr',
#' year is available by single year. There is no difference in the distribution
#' of values between the two formats.
#'
#' @return Data.frame of deathrates for country.
#' @export
load_deathrates <- function(iso3c = NULL, country = NULL, year = NULL) {
  deathrates <- peeps::mortality_rates

  if (!is.null(country) && !is.null(iso3c)) {
    if (!all(country %in% unique(deathrates$country)) |
      !all(iso3c %in% unique(deathrates$country_code))) {
      stop("Country/country code not found. Double check input/spelling.")
    }
    if (length(country) == 1) {
      deathrates <- deathrates[deathrates$country == country, ]
      iso3c <- toupper(as.character(iso3c))
      if (!all(iso3c %in% unique(deathrates$country_code))) {
        stop("Country and country code do not match. 
             Double check input/spelling.")
      }
    } else {
      deathrates <- deathrates[deathrates$country %in% country, ]
      if (!all(iso3c %in% unique(deathrates$country_code))) {
        stop("Countries and country codes do not match. 
             Double check input/spelling.")
      }
    }
  } else if (!is.null(country)) {
    if (!all(country %in% unique(deathrates$country))) {
      stop("Country not found. Double check input/spelling.")
    }
    if (length(country) == 1) {
      deathrates <- deathrates[deathrates$country == country, ]
    } else {
      deathrates <- deathrates[deathrates$country %in% country, ]
    }
  } else if (!is.null(iso3c)) {
    iso3c <- toupper(as.character(iso3c))
    if (!all(iso3c %in% unique(deathrates$country_code))) {
      stop("Country code not found. Double check input/spelling.")
    }
    if (length(iso3c) == 1) {
      deathrates <- deathrates[deathrates$country_code == iso3c, ]
    } else {
      deathrates <- deathrates[deathrates$country_code %in% iso3c, ]
    }
  }

  if (!is.null(year)) {
    if (!all(year %in% unique(deathrates$year))) {
      stop("Data not available for specified year(s). Double check input. 
           Recall that data for format = 'asmr' is only available for years 
           that are multiples of 5, and data of format = 'nmr' are available
           for all years.")
    }
    if (length(year) == 1) {
      deathrates <- deathrates[deathrates$year == year, ]
    } else {
      deathrates <- deathrates[deathrates$year %in% year, ]
    }
  }
  
  deathrates <- deathrates[order(
    deathrates$country,
    deathrates$year,
    deathrates$age_from
  ), ]
  
  return(deathrates)
}