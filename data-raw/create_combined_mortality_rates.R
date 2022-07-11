library(dplyr)
library(tidyr)

asmr <- read.csv("data-raw/unwpp_mort_rate_both.csv") |>
  filter(year >= 1990, year <= 2050) |>
  mutate(age_upper = if_else(age_to == 120, 200, age_to + 1)) |>
  select(country_code, country, year, age_upper, value) |>
  rename(iso3c = country_code) |> 
  arrange(country, iso3c, year, age_upper) |>
  group_by(iso3c, country, age_upper) |>
  complete(year = seq(first(year), last(year))) |>
  mutate(value = zoo::na.approx(value)) |>
  ungroup()

nmr <- read.csv("data-raw/unwpp_cm_nmr_both.csv") |>
  filter(year >= 1990, year <= 2050) |>
  mutate(age_upper = round(1/12, 4), 
         value = value * 12) |>
  select(country_code, country, year, age_upper, value) |>
  rename(iso3c = country_code)

mortality_rates <- bind_rows(asmr, nmr) |>
  arrange(country, iso3c, year, age_upper) |>
  rename(mortality_rate = value)

# save the mortality rates
usethis::use_data(mortality_rates, overwrite = TRUE)
