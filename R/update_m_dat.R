#' Update model data
#' 
#' Grab all-in model results from happypillpain.
#' 
#' @export

update_m_dat <- function(){
  m_pain_int <-
    withr::with_dir("../happypillpain", targets::tar_read(m_pain_int))
  
  usethis::use_data(m_pain_int, overwrite = TRUE)
  
  devtools::document()
  devtools::build()
  
}