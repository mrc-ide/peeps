test_that("Using countrycode or countryname results in same dataframe", {
  data_countrycode <- load_deathrates(iso3c = "AFG")
  data_countryname <- load_deathrates(country = "Afghanistan")
  
  expect_equal(data_countrycode, data_countryname, ignore_attr = TRUE)
})

test_that("Countryname or countrycode can be single value or vector", {
  data_countrycode <- load_deathrates(iso3c = c("AFG", "JAM", "KEN"))
  data_countryname <- load_deathrates(country = c("Afghanistan", "Jamaica", "Kenya"))
  
  expect_equal(data_countrycode, data_countryname, ignore_attr = TRUE)
})

test_that("Year input works as single value and vector", {
  data_year<-load_deathrates(year = 2000)
  data_year_00_01<-load_deathrates(year = c(2000,2001))
  data_year_95_00 <- load_deathrates(year=c(1995, 2000))
  expect_true(all(data_year_95_00$year %% 5 == 0))
  expect_false(all(data_year_00_01$year %% 5 == 0))
})

test_that("Country code and country need to be of the same countries if used together", {
  data_morocco<-load_deathrates(iso3c = "MAR", country="Morocco")
  data_mar_ken<-load_deathrates(iso3c = c("MAR","KEN"), country=c("Morocco","Kenya"))
  
  expect_true(all(data_morocco$country %in% data_mar_ken$country))
  expect_true(all(data_morocco$country_code %in% data_mar_ken$country_code))
  
  expect_false(all(data_mar_ken$country %in% data_morocco$country))
  
  expect_error(load_deathrates(iso3c="AZE", country="Jamaica"))
  expect_error(load_deathrates(iso3c=c("MAR", "AZE", "JAM"), country="Azerbaijan"))
})