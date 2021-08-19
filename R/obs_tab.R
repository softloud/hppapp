#' Observation-level data
#'
#' @param obs Dataset produced by [obs_selected].
#' @param mod_type `lor` or `smd`.
#'
#' These are the data that are included in the model.

obs_tab <- function(obs, mod_type) {
  reported_var <-
    obs %>%
    dplyr::select(study, intervention,
                  condition,
                  class,
                  mean, sd, r, n, scale)
  
  output_tab <-
    if (mod_type == "smd") {
      reported_var %>%
        dplyr::select(-r) %>%
        gt::gt(groupname_col = "condition") %>%
        gt::fmt_number(sd, decimals = 2)
      
    } else if (mod_type == "lor") {
      reported_var %>%
        dplyr::select(-mean,-sd) %>%
        gt::gt(groupname_col = "condition")
    }
  
  
  output_tab %>%
    hpp_tab(vertical_divider = "study")
}