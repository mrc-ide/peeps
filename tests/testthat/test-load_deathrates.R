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

test_that("Only one of country code or country need must be specified", {
  expect_error(load_deathrates(iso3c = "AZE", country ="Jamaica"), "Please provide iso3c code or country name")
  expect_error(load_deathrates(iso3c = c("MAR", "AZE", "JAM"), country = "Azerbaijan"), "Please provide iso3c code or country name")
})