test_that("Reproduce values in fig 1 caption", {
  # check power values under H1 against result values in caption
  pwr1 <- cdf(0.05,2,0,pi=1)
  pwr2 <- cdf(0.05,3,0,pi=1)
  expect_equal( round(pwr1,3), 0.516 )
  expect_equal( round(pwr2,3), 0.851 )
  # check mixture CDFs of 0.05 against values in caption
  val1 <- cdf(0.05,2,0,pi=0.2)
  val2 <- cdf(0.05,2,0,pi=0.8)
  val3 <- cdf(0.05,3,0,pi=0.2)
  val4 <- cdf(0.05,3,0,pi=0.8)
  expect_equal( round(val1,3), 0.143 )
  expect_equal( round(val2,3), 0.423 )
  expect_equal( round(val3,3), 0.210 )
  expect_equal( round(val4,3), 0.691 )
})
