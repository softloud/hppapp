#' Shiny application for chronic pain and antidepressants
#'
#' Display dashboard of network meta-analysis results for treatment of chronic
#' pain with antidepressants.
#'
#' @export

# library(shiny)

hpp_app <- function(...) {
    # ui ----------------------------------------------------------------------
    
    ui <- shinydashboard::dashboardPage(
        skin = "blue",
        
        # ui header ---------------------------------------------------------------
        
        
        shinydashboard::dashboardHeader(title = "Treating chronic pain with antidepressants",
                                        # puts sidebar toggle on right
                                        titleWidth = "calc(100% - 44px)"),
        
        # ui sidebar --------------------------------------------------------------
        shinydashboard::dashboardSidebar(
            width = 200,
            shinydashboard::sidebarMenu(
                id = "tabs",
                shinydashboard::menuItem(
                    "Direct evidence",
                    tabName = "net",
                    icon = shiny::icon("project-diagram")
                ),
                shinydashboard::menuItem(
                    "Summary information",
                    tabName = "summary",
                    icon = shiny::icon("table")
                ),
                shinydashboard::menuItem(
                    "Network meta-analysis",
                    tabName = "nma",
                    icon = shiny::icon("calculator")
                ),
                shinydashboard::menuItem(
                    "Observation-level data",
                    tabName = "data",
                    icon = shiny::icon("file-csv")
                )
            ),
            shiny::selectInput(
                inputId = "outcome",
                label = "Outcome",
                selected = "pain_int",
                choices = hpp_outcomes
            ),
            shinydashboard::box(
                title = "Class of antidepressant",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                background = "black",
                shiny::checkboxGroupInput(
                    inputId = "class",
                    label = NULL,
                    choices = hpp_obs %>% 
                        dplyr::filter(!is.na(class)) %>% 
                        dplyr::pull(class) %>% 
                        unique(),
                    selected = hpp_obs %>% 
                        dplyr::filter(!is.na(class)) %>% 
                        dplyr::pull(class) %>% 
                        unique()
                )
            ),
            shinydashboard::box(
                title = "Studies",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                background = "black",
                shiny::checkboxGroupInput(
                    inputId = "studies",
                    label = NULL,
                    choices = hpp_obs %>% 
                        dplyr::pull(study) %>%
                        unique(),
                    selected = hpp_obs %>% 
                        dplyr::pull(study) %>%
                        unique()
                )
            ),
            shiny::selectInput(
                inputId = "timepoint",
                label = "Timepoint",
                choices = hpp_obs %>% dplyr::pull(timepoint) %>% unique(),
                selected = "post_int"
            )
            
        ),
        # ui body -----------------------------------------------------------------
        
        
        shinydashboard::dashboardBody(
            shinyjs::useShinyjs(),
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = "net",
                    shiny::plotOutput("net",
                                      height = 800,
                                      width = 1500)
                    
                ),
                
                shinydashboard::tabItem(
                    tabName = "nma",
                    shiny::column(
                        7,
                        align = "center",
                        shiny::plotOutput("tau",
                                          height = 200,
                                          width = 700)
                    ),
                    shiny::column(5,
                                  shiny::plotOutput(
                                      "forest",
                                      height = 850,
                                      width = 600
                                  ))
                ),
                
                shinydashboard::tabItem(tabName = "summary",
                                        gt::gt_output("ss")),
                
                shinydashboard::tabItem(tabName = "data",
                                        shiny::h3("Hello, world."))
            )
        )
    )
    
    
    # server ------------------------------------------------------------------
    
    
    server <- function(input, output, session) {
        obs <- reactive({
            obs_selected(
                outcome_selected = input$outcome,
                class_selected = input$class,
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


# nma ---------------------------------------------------------------------

nma_net <- reactive({
    set_net(dat = obs(), mod_type = m_type())
})                

# nma plots ---------------------------------------------------------------

        output$net <- shiny::renderPlot({
            net_plot(nma_net())
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
