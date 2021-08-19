#' Update data
#'
#' Grab the data from the happypillpain dir and store it as package data.
#'
#' @export

update_hpp_dat <- function() {
  hpp_obs <-
    withr::with_dir("../happypillpain", targets::tar_read(w_obs)) %>% 
    dplyr::filter(stringr::str_detect(design, "parallel"),
                  stringr::str_detect(type %in% c("antidepressant", "placebo")))
  
    usethis::use_data(hpp_obs, overwrite = TRUE)
    
    hpp_outcomes <- 
      withr::with_dir("../happypillpain", targets::tar_read(w_outcomes)) 
    
    usethis::use_data(hpp_outcomes, overwrite = TRUE)
    
    m_key <-
      withr::with_dir("../happypillpain", targets::tar_read(m_key)) 
    
    usethis::use_data(m_key, overwrite = TRUE)
    
    devtools::document()
    devtools::build()

}
