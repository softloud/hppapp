#' Shiny application for chronic pain and antidepressants
#' 
#' Display dashboard of network meta-analysis results for treatment of chronic 
#' pain with antidepressants. 
#' 
#' @export

# library(shiny)

hpp_app <- function(...) {
    # todo: write update target
    # obs_dat <- withr::with_dir("../happypillpain", targets::tar_read(w_obs))
    # write_rds(obs_dat, "obs.rds")
    
    obs_dat <- readr::read_rds("obs.rds")
    
    # ui ----------------------------------------------------------------------
    
    ui <- shiny::fluidPage(
        # shinythemes::themeSelector(),  # try out different themes
        theme = shinythemes::shinytheme("sandstone"),
        # set theme
        
        # Application title
        shiny::titlePanel("Treating chronic pain with antidepressants"),
        
        # Sidebar with a slider input for number of bins
        shiny::sidebarLayout(shiny::sidebarPanel(shiny::inputPanel(
            shiny::selectInput(
                inputId = "outcome",
                label = "Outcome",
                selected = "pain_int",
                choices = obs_dat %>% dplyr::pull(outcome) %>% unique()
                
            )
        )),
        
        # Show a plot of the generated distribution
        shiny::mainPanel(
            shiny::tabsetPanel(
                shiny::tabPanel("Outcome-specific findings",
                         tableOutput("sof")),
                
                shiny::tabPanel(
                    "Network meta-analysis",
                    plotOutput("net"),
                    plotOutput("forest"),
                    plotOutput("tau")
                ),
                
                shiny::tabPanel("Data")
                
            )# end tabset panel
        )# end main panel)# end sidebar layout
        )# end fluidpage
    ) # end ui
        
        
        # server ------------------------------------------------------------------
        
        
        server <- function(input, output, session) {
            
        }
        shiny::shinyApp(ui, server, ...)
}
