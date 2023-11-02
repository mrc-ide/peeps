test_that("multiplication works", {
  # With same interval in and out
  expect_equal(rescale_prob(0.25, 1, 1), 0.25)
  expect_equal(rescale_prob(0.25, 10, 10), 0.25)
  # With limits of prob
  expect_equal(rescale_prob(1, 1, 100), 1)
  expect_equal(rescale_prob(1, 100, 1), 1)
  expect_equal(rescale_prob(0, 1, 100), 0)
  expect_equal(rescale_prob(0, 100, 1), 0)
})
