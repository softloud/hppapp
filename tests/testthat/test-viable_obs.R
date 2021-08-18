test_that("viable obs", {
  expect_true(
    "data.frame" %in% (
    viable_observations(
    hpp_obs %>% 
      dplyr::filter(outcome == "pain_int")
  ) %>% class())
  
  )
})
