# data wrangle
# asmr
asmr <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_both.csv")
asmr_f <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_female.csv")
asmr_m <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_male.csv")

asmr_total <- dplyr::bind_rows(asmr, asmr_f, asmr_m)
asmr <- subset(asmr_total, select=-c(country_code_numeric))
usethis::use_data(asmr, overwrite = TRUE, internal = FALSE)

cdr <- read.csv("data-raw/202110gavi-3_dds-201910_cdr_both.csv")
cdr <- subset(cdr, select=-c(country_code_numeric))
cdr$age_to <- 120
usethis::use_data(cdr, overwrite = TRUE, internal = FALSE)

# child mortality
nmr <- read.csv("data-raw/202110gavi-3_dds-201910_2_unwpp_cm_nmr_both.csv")
imr <- read.csv("data-raw/202110gavi-3_dds-201910_unwpp_imr_both.csv")
u5mr <- read.csv("data-raw/202110gavi-3_dds-201910_unwpp_u5mr_both.csv")

nmr <- subset(nmr, select=-c(country_code_numeric))
imr <- subset(imr, select=-c(country_code_numeric))
u5mr <- subset(u5mr, select=-c(country_code_numeric))

nmr$age_to <- 28 / 365.24
imr$age_to <- 1
usethis::use_data(nmr, overwrite = TRUE, internal = FALSE)
usethis::use_data(imr, overwrite = TRUE, internal = FALSE)
usethis::use_data(u5mr, overwrite = TRUE, internal = FALSE)

cbr <- read.csv("data-raw/202110gavi-3_dds-201910_cbr_both.csv")
pop <- read.csv("data-raw/202110gavi-3_dds-201910_2_tot_pop_both.csv")

cbr <- subset(cbr, select=-c(country_code_numeric))
pop <- subset(pop, select=-c(country_code_numeric))


cbr$age_to <- 120


usethis::use_data(cbr, overwrite = TRUE, internal = FALSE)
usethis::use_data(pop, overwrite = TRUE, internal = FALSE)

