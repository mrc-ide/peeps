#' Wrangles child deathrates
#'
#' @param mylist List of deathrates, returned by
#' download_data(indicator="deathrates").
#' @param deathrates The wrangled deathrates, returned by
#' wrangle_deathrates(mylist).
#' @param na_match Defaults to true. Matches NAs with the average by
#' world bank income group and year. Only missing values are between 1960-1966.
#' @param na_rm Defaults to true. Removes NAs from dataset, after matching if
#' na_match is TRUE. Note: if set to FALSE, will return region values.
#' @param extrapolate_rates Defaults to true. Extrapolates deathrates following
#' the trend observed by country in the deathrates in the under 5 group.
#' @param by Either mx or qx, the rate to use in extrapolation.
#'
#' @import data.table
#' @import tidyr
#' @import dplyr
#' @importFrom plyr ddply
#' @importFrom magrittr %>%
#'
#' @return Vector of deathrates by year.
#' @export
wrangle_neorates <- function(mylist, deathrates, na_match = TRUE, na_rm = TRUE,
                             extrapolate_rates = TRUE, by = "mx") {
  neorates <- mylist$neorates
  wb <- mylist$worldbank

  # RECODE neorates
  # get rid of superfluous columns
  neorates <- neorates[, !sapply(neorates, is.logical)]
  neorates <- subset(neorates, select = -c(Indicator.Name, Indicator.Code))

  # remove the X in front of the year columns
  names(neorates) <- sub("^X", "", names(neorates))
  # pivot longer, turn year columns into one variable
  neorates <- data.table::melt(data.table::setDT(neorates),
    id.vars = c("Country.Name", "Country.Code"),
    variable.name = "year"
  )
  names(neorates) <- c("country_name", "country_code", "year", "value")
  # add age group variables
  neorates$agegrpstart <- 0
  neorates$agegrpend <- 0.0833333

  # MATCH TO SIMILAR COUNTRIES based on world bank income groups
  neorates$income_group <- wb$`Income group`[match(
    neorates$country_code,
    wb$Code
  )]
  # will yield NAs because some are not countries but regions

  # replace NA values with the mean by year and income_group
  if (na_match == TRUE) {
    neorates <- plyr::ddply(neorates, ~ income_group + year, transform,
      value = ifelse(is.na(value),
        mean(value, na_rm = TRUE),
        value
      )
    )
  }
  # this only removes regions
  if (na_rm == TRUE) {
    complete_data <- neorates[stats::complete.cases(neorates), ]
  } else {
    complete_data <- neorates
  }

  complete_data$year <- as.numeric(as.character(complete_data$year))
  complete_data$country_code <- as.factor(complete_data$country_code)
  complete_data$sex <- rep("Total", nrow(complete_data))

  # convert neonatal rates into three year average, or five year average?
  # is that really necessary?
  # gonna just start by dividing it by 1000
  complete_data$value <- complete_data$value / 1000

  # EXTRAPOLATION:
  # compute change in deathrates of u5 RELATIVE to 2019
  if (extrapolate_rates == TRUE) {
    # get the data to calculate the extrapolated rates: all u-5s from 2019 on
    proj_data <- subset(
      deathrates,
      agegrpend == 5 & year >= 2019 & sex == "total"
    )

    # compute the proportional change in death rates between every year & 2019
    proj_data <- proj_data %>%
      group_by(country_name) %>% # can add sex here if get sex specific rates
      mutate(
        change_mx = mx / mx[year == 2019],
        change_qx = qx / qx[year == 2019]
      ) %>%
      ungroup()

    # New Zealand was lumped together with Australia -
    # Since Australia has their own data, will recode New Zealand & Australia
    # as only New Zealand
    proj_data$country_name <- ifelse(grepl(
      "New Zealand",
      proj_data$country_name
    ),
    "New Zealand", proj_data$country_name
    )
    suppressWarnings(proj_data$country_code <- ifelse(proj_data$country_name
      %in% countrycode::codelist$country.name.en,
    countrycode::countrycode(proj_data$country_name,
      origin = "country.name",
      destination = "iso3c"
    ), NA
    ))

    # slice and copy the original neonatal data to store results
    ex_data <- subset(complete_data, year == 1960)
    ex_data$year <- NA
    ex_data <- subset(ex_data, select = -c(value))

    # copy the rows as many times as there are years to be extrapolated
    extrapolate_rates <- ex_data[rep(seq_len(nrow(ex_data)), 81), ]
    # initialize the column of years
    extrapolate_rates$year <- sort(rep(seq(2020, 2100, 1), nrow(ex_data)))

    # get the 2019 values to times by
    data_2019 <- subset(complete_data, year == 2019)
    # combine the dataframes to yield income group, age, and death rate columns
    mydata <- merge(subset(proj_data,
      select = c(country_code, change_mx, change_qx, year)
    ),
    extrapolate_rates,
    by = c("country_code", "year")
    )
    mydata <- merge(mydata, subset(data_2019, select = c(country_code, value)),
      by = "country_code"
    )

    # find the extrapolated rates using the proportional change in the u5 rates
    mydata <- mydata %>%
      group_by(country_code, year) %>%
      mutate(
        proj_value_mx = change_mx * value, # where value = 2019 value
        proj_value_qx = change_qx * value
      ) %>%
      ungroup()
    # mydata now contains all the extrapolated neonatal rates

    if (by == "mx") {
      mydata <- subset(mydata, select = c(
        country_name, country_code, year,
        proj_value_mx, agegrpstart, agegrpend,
        income_group, sex
      ))
      colnames(mydata)[4] <- "value"
      data_with_extrapolation <- rbind(complete_data, mydata)
    } else {
      mydata <- subset(mydata, select = c(
        country_name, country_code, year,
        proj_value_qx, agegrpstart, agegrpend,
        income_group, sex
      ))
      colnames(mydata)[4] <- "value"
      data_with_extrapolation <- rbind(complete_data, mydata)
    }
    return(data_with_extrapolation)
  } else {
    return(complete_data)
  }
}
