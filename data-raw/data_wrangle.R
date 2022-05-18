# data wrangle
library(zoo)
library(dplyr)
library(tidyverse)
library(splines)

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

asmr_interpolated <- asmr_interpolated[order(
  asmr_interpolated$country,
  asmr_interpolated$year
), ]

# look at plots for interpolated values
asmr_interpolated$interpolated<-ifelse(asmr_interpolated$year %% 5 == 0, "base",
                                       "interpolated")
asmr_int_split<-split(asmr_interpolated,
                      list(asmr_interpolated$country_code, 
                           asmr_interpolated$age_from, asmr_interpolated$gender))

make_asmr_plots<-function(data) {
  ggplot(data=data, aes(year,value,
                     color=as.factor(data$interpolated), 
                     fill=as.factor(data$interpolated))) +
    geom_point()+
    labs(title = paste0(data$country, " death rates, ages: ", data$age_from, 
                        "-",data$age_to, " years"),
         fill = "Interpolated",
         color = "Interpolated")+
    theme_bw()
}

# Use this function to look at different countries
make_asmr_plots(data=asmr_int_split[[756]])

mortality_rates <- dplyr::bind_rows(nmr, asmr_interpolated)

mortality_rates <- mortality_rates[order(
  mortality_rates$country,
  mortality_rates$year
), ]

# save the mortality rates
usethis::use_data(mortality_rates, overwrite = TRUE, internal = FALSE, 
                  compress = "bzip2")
