#' Network plot
#' 
#' @param hpp_net Network meta-analysis network object.

net_plot <- function(hpp_net) {
  hpp_net %>% 
    plot() +
    ggthemes::theme_tufte(
      base_size = 19
    ) +
    ggplot2::labs(
      title = "Direct evidence network",
      subtitle = "Antidepressant comparisons",
      caption = stringr::str_wrap("A network meta-analysis extends on pairwise meta-analysis,
                     allowing for the comparison of multiple treatments.
                     The method takes an incomplete set of pairwise comparisons,
                     shown in this network diagram. The algorithm estimates
                     the missing comparisons and uses the estimated complete
                     network to produce a set of comparisons of each treatment
                     from placebo.", 60),
      y = "",
      x = ""
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}