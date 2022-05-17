test_that("Using countrycode or countryname results in same dataframe", {
  data_countrycode <- load_deathrates(iso3c = "AFG")
  data_countryname <- load_deathrates(country = "Afghanistan")
  
  expect_equivalent(data_countrycode, data_countryname)
})

test_that("Countryname or countrycode can be single value or vector", {
  data_countrycode <- load_deathrates(iso3c = c("AFG", "JAM", "KEN"))
  data_countryname <- load_deathrates(country = c("Afghanistan", "Jamaica", "Kenya"))
  
  expect_equivalent(data_countrycode, data_countryname)
})

test_that("Year input works as single value and vector in tandem with format", {
  data_year<-load_deathrates(year = 2000)
  expect_error(load_deathrates(year = c(2000,2001)))
  data_year_95_00 <- load_deathrates(year=c(1995, 2000))
  data_year_00_01<-load_deathrates(year=c(2000,2001), format="nmr")
  expect_true(all(data_year_95_00$year %% 5 == 0))
  expect_false(all(data_year_00_01$year %% 5 == 0))
})