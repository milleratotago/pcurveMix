test_that("jackknife computations are correct", {
  ests_orig <- list(sample_var = 11.6)
  jacksample_ests <- c(9.5, 13.25, 14.1875, 14.1875, 3.25)
  jackknife_summaries <- data.frame(parameter = c("sample_var", "sample_var"),
                                    summary = c("mean", "sd"),
                                    value = c(10.875, sd(jacksample_ests)))
  full_sample_n <- 5
  answers <- jackknife_computations(ests_orig, jackknife_summaries, full_sample_n,
                                    t_or_z = 1.96, center_ci_at_est_orig = FALSE)
  # Correct answers: bias = -2.9, estimate_bc = 14.5 jack_se = 8.372201,
  #  bounds = -1.909514, 30.909514
  expect_equal(answers$bias, -2.9, tolerance = 0.001)
  expect_equal(answers$bias_corrected_estimate, 14.5, tolerance = 0.001)
  expect_equal(answers$jack_se, 8.3722, tolerance = 0.0001)
  expect_equal(answers[[CI_LOWER_BOUND_LABEL]], -1.9095, tolerance = 0.0001)
  expect_equal(answers[[CI_UPPER_BOUND_LABEL]], 30.9095, tolerance = 0.0001)
})
