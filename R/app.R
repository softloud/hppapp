#' Shiny application for chronic pain and antidepressants
#'
#' Display dashboard of network meta-analysis results for treatment of chronic
#' pain with antidepressants.
#'
#' @export

# library(shiny)

hpp_app <- function(...) {
    # ui ----------------------------------------------------------------------
    
    ui <- shiny::fluidPage(
        # shinythemes::themeSelector(),  # try out different themes
        theme = shinythemes::shinytheme("sandstone"),
        # set theme
        
        # Application title
        shiny::titlePanel("Treating chronic pain with antidepressants"),
        
        # Sidebar with a slider input for number of bins
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 4,
                shiny::inputPanel(
                    shiny::selectInput(
                        inputId = "outcome",
                        label = "Outcome",
                        selected = "pain_int",
                        choices = hpp_outcomes
                    )),
                
                shiny::inputPanel(
                    shiny::h5("Subgroups"),
                    shinydashboard::box(
                        title = "Class of Antidepressant",
                        collapsible = TRUE,
                        collapsed = TRUE,
                    shiny::checkboxGroupInput(
                        inputId = "class",
                        label = "Class of Antidepressant",
                        choices = hpp_obs %>% dplyr::pull(class) %>% unique()
                    ))
                ),
                
                shiny::inputPanel(
                    shiny::selectInput(
                        inputId = "timepoint",
                        label = "Timepoint",
                        selected = "post_int",
                        choices = hpp_obs %>% dplyr::pull(timepoint) %>% 
                            unique()
                    )
                )
            ),
            
            # Show a plot of the generated distribution
            shiny::mainPanel(
                shiny::tabsetPanel(
                    shiny::tabPanel("Selection summary",
                                    shiny::tableOutput("ss")),
                    
                    shiny::tabPanel(
                        "Network meta-analysis",
                        shiny::fluidRow(
                            shiny::column(
                                7,
                                align = "center",
                                shiny::plotOutput("net",
                                                  height = 650,
                                                  width = 650),
                                shiny::plotOutput("tau",
                                                  height = 200,
                                                  width = 400)
                            ),
                            shiny::column(5,
                                          shiny::plotOutput(
                                              "forest",
                                              height = 850,
                                              width = 400
                                          ))
                        )
                    ),
                    
                    shiny::tabPanel("Data",
                                    shiny::tableOutput("obs"))
                    
                )# end tabset panel
            )# end main panel)# end sidebar layout
        )# end fluidpage
    ) # end ui
    
    
    # server ------------------------------------------------------------------
    
    
    server <- function(input, output, session) {
        obs <- reactive({
            obs_selected(
                outcome_selected = input$outcome,
                timepoint_selected = input$timepoint
            )
        })
        
        m_type <- reactive({
            m_key %>%
                dplyr::filter(outcome == input$outcome) %>%
                dplyr::pull(model_type)
        })
        
        
        # summary information -----------------------------------------------------
        
        output$ss <- gt::render_gt({
            ss_tab(obs())
        })
        
        # nma results -------------------------------------------------------------
        
        output$net <- shiny::renderPlot({
            m_pain_int$network %>%
                net_plot()
        })
        
        output$forest <- shiny::renderPlot({
            m_pain_int %>%
                forest_plot(hpp_mod = .,
                            mod_type = m_type())
        })
        
        output$tau <- shiny::renderPlot({
            m_pain_int %>%
                tau()
        })
        

# data --------------------------------------------------------------------

        output$obs <- gt::render_gt({
            obs_tab(obs(), m_type())
        })
        
    }
    

# run app -----------------------------------------------------------------

    
    shiny::shinyApp(ui, server, ...)
}
