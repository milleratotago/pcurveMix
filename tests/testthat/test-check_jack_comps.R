test_that("jackknife computations are correct", {
  ests_orig <- list(sample_var = 11.6)
  jacksample_ests <- c(9.5, 13.25, 14.1875, 14.1875, 3.25)
  testing_with_bias_correction <- TRUE
  # jackknife_summaries <- data.frame(parameter = c("sample_var", "sample_var"),
  #                                   summary = c("mean", "sd"),
  #                                   value = c(10.875, sd(jacksample_ests)))
  jackknife_summaries <- data.frame(parameter = "sample_var",
                                    mean = 10.875, sd = sd(jacksample_ests)  )
  full_sample_n <- 5
  answers <- jackknife_computations(ests_orig, jackknife_summaries, full_sample_n,
                                    t_or_z = 1.96,
                                    bias_correct_ci_bounds = testing_with_bias_correction)
  # Correct answers: bias = -2.9, estimate_bc = 14.5 jack_se = 8.372201,
  #  bounds = -1.909514, 30.909514
  expect_equal(answers$bias, -2.9, tolerance = 0.001)
  expect_equal(answers$bc_estimate, 14.5, tolerance = 0.001)
  # expect_equal(answers$jack_se, 8.3722, tolerance = 0.0001) # no longer included in table
  if (testing_with_bias_correction) {
    lower_bound_name <- bias_corrected_name(CI_LOWER_BOUND_LABEL)
    upper_bound_name <- bias_corrected_name(CI_UPPER_BOUND_LABEL)
  } else {
    lower_bound_name <- CI_LOWER_BOUND_LABEL
    upper_bound_name <- CI_UPPER_BOUND_LABEL
  }
  expect_equal(answers[[lower_bound_name]], -1.9095, tolerance = 0.0001)
  expect_equal(answers[[upper_bound_name]], 30.9095, tolerance = 0.0001)
})
