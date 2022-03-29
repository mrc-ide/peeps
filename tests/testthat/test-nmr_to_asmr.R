test_that("nmr -> asmr conversion only creates year entries divisible by 5", {
  nmr <- peeps::nmr
  nmr_new<- nmr_to_asmr(nmr)
  expect_true(all(nmr_new$year %% 5 == 0))
})

test_that("All nmr input and converted values are positive", {
  nmr <- peeps::nmr
  expect_true(all(nmr$value >= 0))
  expect_true(all(nmr$age_from >= 0))
  expect_true(all(nmr$age_to >= 0))
  expect_true(all(nmr$year >= 0))
  nmr_new<- nmr_to_asmr(nmr)

  expect_true(all(nmr_new$value >= 0))
  expect_true(all(nmr_new$age_from >= 0))
  expect_true(all(nmr_new$age_to >= 0))
  expect_true(all(nmr_new$year >= 0))
})

test_that("nmr -> asmr conversions does not yield NAs when 10% NAs are input", {
  nmr <- peeps::nmr
  set.seed(123)
  nmr$value[sample(nrow(nmr),0.1*nrow(nmr))]<-NA # only testing 10% NA threshold here
  nmr_new<- nmr_to_asmr(nmr)
  
  expect_false(any(is.na(nmr_new$value)))
  
  # sometimes this test will not work, because of the random sampling.
})
# should I add some testing that the mean is working?
test_that("nmr -> asmr conversion does not handle empty dataset", {
  df<-data.frame(NULL)
  
  expect_error(nmr_to_asmr(df))
  
})