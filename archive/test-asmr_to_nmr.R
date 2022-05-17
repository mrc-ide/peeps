test_that("asmr -> nmr conversion only creates year entries that are not only
          divisible by 5", {
  asmr <- peeps::asmr
  asmr_new<- asmr_to_nmr(asmr)
  expect_false(all(asmr_new$year %% 5 == 0))
})

test_that("All asmr input and converted values are positive", {
  asmr <- peeps::asmr
  expect_true(all(asmr$value >= 0))
  expect_true(all(asmr$age_from >= 0))
  expect_true(all(asmr$age_to >= 0))
  expect_true(all(asmr$year >= 0))
  asmr_new<- asmr_to_nmr(asmr)
  
  expect_true(all(asmr_new$value >= 0))
  expect_true(all(asmr_new$age_from >= 0))
  expect_true(all(asmr_new$age_to >= 0))
  expect_true(all(asmr_new$year >= 0))
})

# this test does not work. any ideas on how to test for NA handling? in which cases would we get NAs?
# test_that("asmr -> nmr conversions does not yield NAs when 10% NAs are input", {
#   asmr <- peeps::asmr
#   set.seed(300)
#   asmr$value[sample(nrow(asmr),0.1*nrow(asmr))]<-NA # only testing 10% NA threshold here
#   asmr_new<- asmr_to_nmr(asmr)
#   
#   expect_false(any(is.na(asmr_new$value)))
#   
#   # sometimes this test will not work, because of the random sampling.
# })
# should I add some testing that the mean is working?
test_that("asmr -> nmr conversion does not handle empty dataset", {
  df<-data.frame(NULL)
  
  expect_error(asmr_to_nmr(df))
  
})