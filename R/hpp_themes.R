#' Theme colours
#' 
#' @param colour Colour

hpp_pal <- function(colour) {
  colours <- 
    c("#ecf2f8", "#d9e6f2")
  
  colours[colour]
}



#' Theme across all tables
#' 
#' @param vertical_divider Dashed vertical line indicating row identifying columns
#' 
#' @export

hpp_tab <- function(gt_tab, vertical_divider = NULL) {
  tab <- 
    gt_tab %>%
  gt::opt_row_striping() %>% 
    gt::tab_options(
      column_labels.background.color = hpp_pal(1),
      row_group.background.color =  hpp_pal(2)
    ) %>% 
    gt::tab_style(
      style = gt::cell_borders(sides = "all", color = hpp_pal(2), 
                               weight = gt::px(3)),
      locations = gt::cells_column_labels(everything())
    ) 
  
  if (is.null(vertical_divider)) {
    tab
  } else {
    tab %>% 
      gt::tab_style(
        style = gt::cell_borders(sides = "right", color = hpp_pal(2),
                             style = "dashed", weight = gt::px(3)),
        locations = gt::cells_body(columns ={{vertical_divider}})
      ) 
      
  }
}