#' Set network for model
#'
#' @param dat A dataset produced by [obs_selected].
#' @param mod_type Type of model: `lor` or `smd`.

set_net <- function(dat, mod_type) {
  if (mod_type == "smd") {
    multinma::set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      y = mean,
      se = se,
      sample_size = n,
      trt_ref = "placebo"
    )
  } else if (mod_type == "lor") {
    multinma::set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      r = r,
      n = n,
      sample_size = n,
      trt_ref = "placebo"
    )
  }
}