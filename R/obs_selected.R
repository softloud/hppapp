#' Update dataset
#'
#' @param outcome_selected User-input outcome, defaults to `pain_int`.
#' @param timepoint_selected User-input timepoint, defaults to `post_int`.
#'
#' Applies user-selected filters to dataset.
#'
#' @export

obs_selected <- function(outcome_selected = "pain_int",
                         timepoint_selected = "post_int") {
  hpp_obs %>%
    dplyr::filter(outcome == outcome_selected,
                  timepoint == timepoint_selected
                  # these are currently filtered in update_hpp_dat
                  # ,
                  # stringr::str_detect(design, "parallel"),
                  # stringr::str_detect(type, "antidepressant|placebo")
                  ) %>%
                  viable_observations()
}