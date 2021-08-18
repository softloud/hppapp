#' Update dataset
#'
#' @param outcome_selected User-input outcome, defaults to `pain_int`.
#' @param timepoint_selected User-input timepoint, defaults to `post_int`.
#' @param class_selected User-selected classes. 
#'
#' Applies user-selected filters to dataset.
#'
#' @export

obs_selected <- function(outcome_selected = "pain_int",
                         class_selected,
                         timepoint_selected = "post_int") {
  hpp_obs %>%
    dplyr::filter(outcome == outcome_selected,
                  timepoint == timepoint_selected,
                  stringr::str_detect(design, "parallel"),
                  stringr::str_detect(type, "antidepressant|placebo"),
                  is.na(class) | class %in% class_selected
                  ) %>%
                  viable_observations()
}