#' Outcome-specific summary
#' 
#' @param obs Selected observations produced by [obs_selected].
#' 
#' 
#' Table of selection-specific summary information.

ss_tab <- function(obs){
  obs %>%
    dplyr::group_by(condition) %>% 
    dplyr::summarise(
      participants = sum(n),
      studies = dplyr::n_distinct(study),
      intervention = unique(intervention) %>% paste(collapse = ", ")
    ) %>% 
    dplyr::ungroup() %>% 
    gt::gt(groupname_col = "outcome") %>% 
    hpp_tab(vertical_divider = "condition")
  
}