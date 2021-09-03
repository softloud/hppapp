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
            # shiny::uiOutput("condition_ui"),
            shinydashboard::box(
                title = "Condition",
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                background = "black",
                shiny::checkboxGroupInput(
                    inputId = "condition",
                    label = NULL,
                    choices = hpp_obs %>%
                        dplyr::arrange(condition) %>%
                        dplyr::filter(!is.na(condition)) %>%
                        dplyr::pull(condition) %>%
                        unique(),
                    selected = hpp_obs %>%
                        dplyr::filter(!is.na(condition)) %>%
                        dplyr::pull(condition) %>%
                        unique()
                )
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
                        dplyr::arrange(study) %>%
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
                    shiny::column(8,
                                  shiny::plotOutput(
                                      "forest",
                                      height = 850,
                                      width = 600
                                  )),
                    shiny::column(4,
                                  align = "center",
                                  shiny::actionButton(
                                      inputId = "run_nma",
                                      label = "Update model",
                                      icon = shiny::icon("calculator")
                                  ),
                                  shiny::plotOutput(
                                      "tau",
                                      height = 200,
                                      width = 400
                                  ))
                ),
                
                shinydashboard::tabItem(tabName = "summary",
                                        gt::gt_output("ss")),
                
                shinydashboard::tabItem(
                    tabName = "data",
                    shiny::downloadButton(outputId = "obs_dat",
                                          "Download observation-level data"),
                    gt::gt_output("obs")
                )
            )
        )
    )
    
    
    # server ------------------------------------------------------------------
    
    
    server <- function(input, output, session) {
        obs <- reactive({
            obs_selected(
                outcome_selected = input$outcome,
                condition_selected = input$condition,
                class_selected = input$class,
                study_selected = input$studies,
                timepoint_selected = input$timepoint
            )
        })
        
        m_type <- reactive({
            m_key %>%
                dplyr::filter(outcome == input$outcome) %>%
                dplyr::pull(model_type)
        })
        
        
        # reactive ui -------------------------------------------------------------
        
        # observe({
        #     class_updated <- obs() %>%
        #         dplyr::arrange(class) %>%
        #         dplyr::pull(class) %>%
        #         unique()
        #
        #
        #     updateCheckboxGroupInput(
        #         inputId = "class",
        #         choices = class_updated
        #     )
        # })
        #
        # output$condition_ui <-
        #     shiny::renderUI({
        #         selections <-
        #             obs() %>%
        #             dplyr::arrange(condition) %>%
        #             dplyr::pull(condition) %>%
        #             unique()
        #
        #         shiny::checkboxGroupInput(
        #             inputId = "condition",
        #             label = "Condition",
        #             choices = selections,
        #             selected = selections
        #         )
        #     })
        #
        
        # summary information -----------------------------------------------------
        
        output$ss <- gt::render_gt({
            ss_tab(obs())
        })
        
        
        # nma ---------------------------------------------------------------------
        
        nma_net <-  reactive({
            set_net(dat = obs(), mod_type = m_type())
        })
        
        nma_mod <- observeEvent(input$run_nma, {
            multinma::nma(
                nma_net(),
                trt_effects = "random"
            )
        })
        
        nma_forest <- reactive({
            forest_plot(hpp_mod = nma_mod(),
                        mod_type = m_type())
            
        })
        
        nma_tau <- reactive({
            # nma_mod() %>%
                tau(m_pain_int)
            
        })
        
        # nma plots ---------------------------------------------------------------
        
        output$net <- shiny::renderPlot({
            net_plot(nma_net())
        })
        
        output$forest <- shiny::renderPlot({
            # nma_forest()
            multinma::relative_effects(m_pain_int) %>% 
                plot()
        })
        
        output$tau <- shiny::renderPlot({
            nma_tau()
        })
        
        
        # data --------------------------------------------------------------------
        
        output$obs <- gt::render_gt({
            obs_tab(obs(), m_type())
        })
        
        output$obs_dat <- shiny::downloadHandler(
            filename = function() {
                glue::glue("observations-{lubridate::now()}.csv")
            },
            content = function(file) {
                readr::write_csv(obs(),
                                 file)
            }
        )
        
    }
    
    
    # run app -----------------------------------------------------------------
    
    
    shiny::shinyApp(ui, server, ...)
}
