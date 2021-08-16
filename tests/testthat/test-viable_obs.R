obs <- readr::read_rds("obs.rds")

test_that("viable obs", {
  expect_success(viable_observations(
    obs %>% 
      filter(outcome == "pain_int")
  ))
})
