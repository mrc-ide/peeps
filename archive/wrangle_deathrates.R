#' Wrangles death rate data into a dataframe
#'
#' @param mylist List of deathrates, returned by
#' download_data(indicator="deathrates").
#'
#' @import data.table
#' @import tidyr
#' @import dplyr
#' @import countrycode
#' @importFrom stats aggregate
#'
#' @return Dataframe of deathrates by year.
#' @export
wrangle_deathrates <- function(mylist) {
  deathrates <- mylist$deathrates
  wb <- mylist$worldbank

  # RECODE DEATHRATES
  # get rid of superfluous columns
  deathrates <- deathrates[, c(
    "Location", "Time", "Sex", "AgeGrpStart",
    "AgeGrpSpan", "mx", "qx"
  )]
  # rename columns
  names(deathrates) <- c(
    "country_name", "timespan", "sex", "agegrpstart",
    "agegrpspan", "mx", "qx"
  )
  # recode sex
  deathrates$sex <- tolower(deathrates$sex)
  # add age group variable
  deathrates$agegrpend <- deathrates$agegrpstart + deathrates$agegrpspan
  # expand according to timespan
  deathrates <- tidyr::separate(
    deathrates, timespan,
    c("yearstart", "yearend")
  )
  deathrates$yearstart <- as.numeric(deathrates$yearstart)
  deathrates$yearend <- as.numeric(deathrates$yearend)
  deathrates <- data.table::data.table(deathrates)
  deathrates <- deathrates[, list(year = seq(yearstart, yearend)),
    by =
      c(
        "country_name", "sex", "agegrpstart", "agegrpend",
        "mx", "qx", "yearstart", "yearend"
      )
  ]
  df <- deathrates[, c(
    "country_name", "sex", "agegrpstart", "agegrpend", "mx",
    "qx", "year"
  )]
  df <- df[!duplicated(df), ]


  # take arithmetic mean of overlapping years
  data_5 <- data.frame()
  # take out all the years divisible by 5 from data frame
  data_5 <- subset(df, df$year %% 5 == 0)
  # get rid of them in the original data frame
  df <- df[!(df$year %% 5 == 0), ]

  # find the mean of mx and qx values for years where variables overlap,
  # as done here:
  # https://population.un.org/wpp/Publications/Files/WPP2019_Methodology.pdf
  agg_data <- stats::aggregate(cbind(qx, mx) ~ country_name + sex + agegrpstart
    + agegrpend + year, data_5, mean)

  # add back in the arithmetic mean of the overlapping years
  mydata <- rbind(df, agg_data)
  mydata <- mydata[with(mydata, order(
    mydata$country_name, mydata$year,
    mydata$agegrpstart, mydata$sex
  )), ]



  # add in country codes
  suppressWarnings(mydata$country_code <- ifelse(mydata$country_name
    %in% countrycode::codelist$country.name.en,
  countrycode::countrycode(mydata$country_name,
    origin = "country.name",
    destination = "iso3c"
  ), NA
  ))
  # add in income
  mydata$income_group <- wb$`Income group`[match(
    mydata$country_code,
    wb$Code
  )]

  return(mydata)
}
