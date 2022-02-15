test_that("Sequence works", {
  mylist <- download_data()
  deathrates <- wrangle_deathrates(mylist)
  neorates <- wrangle_neorates(mylist, deathrates)
  rates <- merge_rates(deathrates, neorates)
  expect_s3_class(rates, "data.frame")
})
