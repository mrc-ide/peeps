# data wrangle
library(zoo)
library(dplyr)
library(tidyverse)

# laod data first
load(file="data/asmr.rda")
load(file="data/nmr.rda")

# Interpolation ---------------------------------------------------------------

# Linearly interpolate between mortality rates for certain age groups of 
# certain genders in certain countries by year
asmr_interpolated <- asmr %>%
                      group_by(age_to, gender, country) %>%
                      complete(year = 
                                 seq(first(year), last(year), len = 146)) %>%
                      mutate(value = na.approx(value))

# fill the NAs 
asmr_interpolated <- fill(asmr_interpolated, 
                          c(country_code, income_group, age_from))


# Combine with NMR ------------------------------------------------------------
# NMR is only available for both genders
asmr_interpolated <- subset(asmr_interpolated, 
                            asmr_interpolated$gender == "both")

# recode ages for merging
asmr_interpolated$age_to <- ifelse(asmr_interpolated$age_to == 0, 1, 
                                   asmr_interpolated$age_to)
asmr_interpolated$age_from <- ifelse(asmr_interpolated$age_from == 0, 0.083333, 
                                     asmr_interpolated$age_from)


mortality_rates <- dplyr::bind_rows(nmr, asmr_interpolated)

mortality_rates <- mortality_rates[order(
  mortality_rates$country,
  mortality_rates$year
), ]

# save the mortality rates
usethis::use_data(mortality_rates, overwrite = TRUE, internal = FALSE)