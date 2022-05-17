test_that("formating merging by nmr gives more row than by asmr, but same number
          of columns", {
  data_asmr <- merge_nmr_asmr(format="asmr")
  data_nmr <- merge_nmr_asmr(format="nmr")
  
  expect_true(nrow(data_nmr)>nrow(data_asmr))
  expect_true(ncol(data_nmr)==ncol(data_asmr))
})

test_that("testing format specification for merging", {
  data_asmr <- merge_nmr_asmr(format="asmr")
  data_asmr_default <- merge_nmr_asmr()
  
  expect_equivalent(data_asmr, data_asmr_default)
  expect_error(merge_nmr_asmr(format="dataframe"))
})