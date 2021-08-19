#' Update dataset
#'
#' @param outcome_selected User-input outcome, defaults to `pain_int`.
#' @param condition_selected User-selected conditions.
#' @param class_selected User-selected classes.
#' @param study_selected User-selected studies.
#' @param timepoint_selected User-input timepoint, defaults to `post_int`.
#'
#' Applies user-selected filters to dataset.
#'
#' @export

obs_selected <- function(outcome_selected = "pain_int",
                         condition_selected,
                         class_selected,
                         study_selected,
                         timepoint_selected = "post_int") {
  hpp_obs %>%
    # hardcoded filters
    dplyr::filter(
      type %in% c("placebo", "antidepressant"),
      stringr::str_detect(design, "parallel")
    ) %>% 
    dplyr::filter(
      outcome == outcome_selected,
      timepoint == timepoint_selected,
    ) %>%
    dplyr::filter(is.na(class) | class %in% class_selected) %>%
    dplyr::filter(study %in% study_selected) %>%
    dplyr::filter(condition %in% condition_selected) %>% 
    viable_observations()
}