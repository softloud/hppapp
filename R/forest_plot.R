#' Relative effects plot
#' 
#' @param hpp_mod A network meta-analysis model object.
#' @param mod_type Type of model.

forest_plot <- function(hpp_mod, mod_type) {
  hpp_transf <- if (mod_type == "lor") exp else NULL
  hpp_type <- if (mod_type == "lor") "log odds ratio" else 
    "standardised mean difference"
  
  multinma::relative_effects(hpp_mod) %>% 
    plot(
      # transf = hpp_transf # todo: find out how to transform lor
      ) +
    ggplot2::geom_vline(
      xintercept = 0, # nb this will need to be tweaked when transf = exp 
      linetype = "dotted",
      alpha = 0.5
    ) +
    ggthemes::theme_tufte(
      base_size = 16
    ) +
    ggplot2::labs(
      title = "Antidepressant effects",
      subtitle = stringr::str_wrap(
        glue::glue("Network meta-analysis {hpp_type} of intervention effect compared to placebo")
        , 40)
    ) 
  
}