test_that("observationsexist", {
  expect_true("data.frame" %in% class(hpp_obs))
}
)

test_that("pain intensity model", {
  expect_s3_class(m_pain_int, "stan_nma")
})