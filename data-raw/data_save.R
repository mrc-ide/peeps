# data wrangle
# download world bank data
tmp <- tempfile()
if (Sys.info()[1] == "Windows") {
  utils::download.file("http://databank.worldbank.org/data/download/site-content/CLASS.xlsx",
                       tmp,
                       quiet = TRUE, mode = "wb"
  )
} else {
  utils::download.file("http://databank.worldbank.org/data/download/site-content/CLASS.xlsx",
                       tmp,
                       quiet = TRUE
  )
}
wb <- readxl::read_excel(tmp)
unlink(tmp)
#### MORTALITY RATES --------------
# asmr ----------------
asmr <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_both.csv")
asmr_f <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_female.csv")
asmr_m <- read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_male.csv")

asmr_total <- dplyr::bind_rows(asmr, asmr_f, asmr_m)
asmr <- subset(asmr_total, select=-c(country_code_numeric))
asmr$income_group <- wb$`Income group`[match(
  asmr$country_code,
  wb$Code
)]
usethis::use_data(asmr, overwrite = TRUE, internal = FALSE)

# cdr -----------------
cdr <- read.csv("data-raw/202110gavi-3_dds-201910_cdr_both.csv")
cdr <- subset(cdr, select=-c(country_code_numeric))
cdr$age_to <- 120
cdr$income_group <- wb$`Income group`[match(
  cdr$country_code,
  wb$Code
)]
usethis::use_data(cdr, overwrite = TRUE, internal = FALSE)

# child mortality ----------------
nmr <- read.csv("data-raw/202110gavi-3_dds-201910_2_unwpp_cm_nmr_both.csv")
imr <- read.csv("data-raw/202110gavi-3_dds-201910_unwpp_imr_both.csv")
u5mr <- read.csv("data-raw/202110gavi-3_dds-201910_unwpp_u5mr_both.csv")

nmr <- subset(nmr, select=-c(country_code_numeric))
imr <- subset(imr, select=-c(country_code_numeric))
u5mr <- subset(u5mr, select=-c(country_code_numeric))

nmr$age_to <- 0.083333
imr$age_to <- 1

nmr$income_group <- wb$`Income group`[match(
  nmr$country_code,
  wb$Code
)]
imr$income_group <- wb$`Income group`[match(
  imr$country_code,
  wb$Code
)]
u5mr$income_group <- wb$`Income group`[match(
  u5mr$country_code,
  wb$Code
)]
usethis::use_data(nmr, overwrite = TRUE, internal = FALSE)
usethis::use_data(imr, overwrite = TRUE, internal = FALSE)
usethis::use_data(u5mr, overwrite = TRUE, internal = FALSE)

### FERTILITY ------------
cbr <- read.csv("data-raw/202110gavi-3_dds-201910_cbr_both.csv")
cbr <- subset(cbr, select=-c(country_code_numeric))
cbr$age_to <- 120
cbr$income_group <- wb$`Income group`[match(
  cbr$country_code,
  wb$Code
)]

usethis::use_data(cbr, overwrite = TRUE, internal = FALSE)

### POPULATION --------------
qq_both <- read.csv("data-raw/202110gavi-3_dds-201910_qq_pop_both.csv")
qq_f <- read.csv("data-raw/202110gavi-3_dds-201910_qq_pop_female.csv")
qq_m <- read.csv("data-raw/202110gavi-3_dds-201910_qq_pop_male.csv")
pop <- read.csv("data-raw/202110gavi-3_dds-201910_2_tot_pop_both.csv")

pop_detail <- dplyr::bind_rows(qq_both, qq_f, qq_m)
pop_detail <- subset(pop_detail, select=-c(country_code_numeric))
pop_detail$income_group <- wb$`Income group`[match(
  pop_detail$country_code,
  wb$Code
)]
usethis::use_data(pop_detail, overwrite = TRUE, internal = FALSE)

pop <- subset(pop, select=-c(country_code_numeric))
pop$income_group <- wb$`Income group`[match(
  pop$country_code,
  wb$Code
)]
usethis::use_data(pop, overwrite = TRUE, internal = FALSE)