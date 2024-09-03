test_that("Equilibrium age dist works", {
  lt <- matrix(rep(0.1, 100)) # Constant
  ead <- equilibrium_age_distribution(lt)
  expect_equal(round(weighted.mean(1:100, ead), 1), 1 / 0.1)
})
