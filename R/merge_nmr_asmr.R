#' Merge neonatal deathrates with population deathrates.
#'
#' While the neonatal mortality rates are estimates by year, the
#' age specific mortality rates are given per group of 5 years. The user
#' can combine these functions a few different ways:
#'
#' 1. Take the 5 year averages of the neonatal rates, so transform them to be of
#' the same format as the death rates, then combine (format = "asmr", uses the 
#' nmr_to_asmr() function).
#' 2. Expand the death rates data -> simply copy the values from 1950 onto years
#' 1951, 1952, 1953, and 1954, etc. then combine with neonatal data (format = 
#' "nmr", uses the asmr_to_nmr() function).
#'
#' This output can be subset by country and year to be directly fed into 
#' malariasimulation.
#'
#' @param format Format to convert dataset to. Can be either asmr or nmr format.
#' 
#' @importFrom dplyr bind_rows
#'
#' @return Dataframe of death rates from the ages of 0-120, from 1950-2100.
#' @export
merge_nmr_asmr <- function(format = "asmr") {
  nmr <- peeps::nmr
  asmr <- peeps::asmr

  asmr <- subset(asmr, asmr$gender == "both") # 75240 obs

  # recode ages for merging
  asmr$age_to <- ifelse(asmr$age_to == 0, 1, asmr$age_to)
  asmr$age_from <- ifelse(asmr$age_from == 0, 28 / 365.24, asmr$age_from)

  if (format == "asmr") {
    new_nmr <- nmr_to_asmr(nmr)
    combined_rates <- dplyr::bind_rows(new_nmr, asmr)
  } else if (format == "nmr") {
    new_asmr <- asmr_to_nmr(asmr)
    combined_rates <- dplyr::bind_rows(nmr, new_asmr)
  } else {
    stop("Correct format not specified. Must be either 'asmr' or 'nmr'.")
  }

  combined_rates <- combined_rates[order(
    combined_rates$country,
    combined_rates$year
  ), ]
  return(combined_rates)
}

#' Function to convert NMR to ASMR format
#'
#' This function returns the average of the NMR for every five year band, i.e.
#' for 1950-1954, 1955-1959, etc., so that it can be combined with the ASMR.
#' 
#' @param nmr Dataframe of neonatal mortality rates.
#' 
#' @importFrom stats aggregate
#' @import data.table
#' 
#' @return Dataframe of 5 year averages of neonatal mortality rates
#' @export
nmr_to_asmr <- function(nmr = peeps::nmr) {
  # find the average of the neonatal mortality rates for every 5 years
  if (nrow(nmr) == 0 || ncol(nmr) == 0) {
    stop("Input data frame must have dimensions larger than 0.")
  }
  year_groups <- cut(nmr$year, seq(min(nmr$year), max(nmr$year + 1), by = 5),
    labels = seq(min(nmr$year), max(nmr$year), by = 5), right = F
  )
  avg_nmr <- stats::aggregate(nmr$value, list(nmr$country, year_groups), mean,
    na.rm = TRUE,
    na.action = NULL
  )

  # reformat to match original dataset
  colnames(avg_nmr) <- c("country", "year", "value")
  avg_nmr$year <- as.numeric(as.character(avg_nmr$year))
  avg_nmr <- avg_nmr[order(avg_nmr$country, avg_nmr$year), ]

  # cut data frame so can do an easy merge, keeping all column values
  new_nmr <- nmr[nmr$year %% 5 == 0, ]
  new_nmr$value <- avg_nmr$value

  return(new_nmr)
}

#' Function to convert ASMR to NMR format
#'
#' Breaks the 5 year averages of the ASMR into single year values.
#' 
#' @param asmr Data frame of age specific mortality rates.
#' 
#' @import data.table
#'
#' @return Dataframe of age specific mortality rates expanded by year.
#' @export
asmr_to_nmr <- function(asmr = peeps::asmr) {
  if (nrow(asmr) == 0 || ncol(asmr) == 0) {
    stop("Input data frame must have dimensions larger than 0.")
  }
  # get all of the intervening years between year markers
  asmr$yearstart <- as.numeric(asmr$year)
  asmr$yearend <- asmr$year + 4
  new_asmr <- subset(asmr, select = -c(year))

  # copy out the average values so they are attached to each year in the range
  new_asmr <- data.table::data.table(new_asmr)
  new_asmr <- new_asmr[, list(year = seq(yearstart, yearend)),
    by = c(
      "country_code", "country", "age_from", "age_to", "gender",
      "value", "yearstart", "yearend"
    )
  ]

  new_asmr <- subset(new_asmr, select = -c(yearstart, yearend))

  return(as.data.frame(new_asmr))
}
