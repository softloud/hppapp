#' Function to extract viable arms
#' 
#' Takes set of observations and returns only those with at least two observations
#' per study.
#' 
#' @param hpp_df A subset of `w_obs`.
#' 
#' @export

viable_observations <- function(hpp_df) {
  hpp_df %>% 
    dplyr::group_by(study) %>% 
    dplyr::mutate(
      study_obs_count = length(study)
    ) %>% 
    dplyr::arrange(study) %>% 
    dplyr::select(study_obs_count, everything()) %>% 
    dplyr::filter(study_obs_count > 1)  %>% 
    dplyr::ungroup() 
}