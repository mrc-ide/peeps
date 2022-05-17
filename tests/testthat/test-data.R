test_that("You can load the datasets", {
  mortality_rates<- peeps::mortality_rates
  asmr <- peeps::asmr
  
  expect_type(asmr, "list")
  expect_type(mortality_rates, "list")
})
