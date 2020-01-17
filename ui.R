library(shiny)
library(ggvis)
library(shinyjs)
library(shinyWidgets)

# Define UI for application that draws a histogram
navbarPage("California Select Area Fishery Enhancement (CalSAFE)",
           tabPanel("Application",
                    useShinyjs(),
                    useSweetAlert(),
                    sidebarLayout(
                      sidebarPanel(
                        wellPanel(
                          h4("Release strategy proportion"),
                          sliderInput("r_river", "River", min = 0, max = 1, value = 0.4, step = 0.01),
                          sliderInput("r_safe", "SAFE", min = 0, max = 0.6, value = 0.3, step = 0.01),
                          sliderInput("r_bay", "Bay", min = 0, max = 1, value = 0.3, step = 0.01),
                          absolutePanel(top = 25, right = 40,
                                        actionBttn("rsp_info", "", icon("info"), style = "material-circle", size = "xs")
                          )
                        ),
                        wellPanel(
                          sliderInput("oh", "Mixed-stock harvest impact rate", min = 0, max = 0.99, value = 0.5, step = 0.01),
                          sliderInput("th", "Terminal harvest (SAFE only)", min = 0, max = 0.99, value = 0.9, step = 0.01),
                          sliderInput("ir", "In-river harvest", min = 0, max = 0.2, value = 0.1, step = 0.01)
                        ),
                        wellPanel(
                          sliderInput("wa", "Wild spawners", min = 0, max = 10000, value = 5000, step = 100),
                          h4(textOutput("hs")),
                          h4(textOutput("phos")),
                          absolutePanel(bottom = 225, right = 40,
                                        actionBttn("phos_info", "", icon("info"), style = "material-circle", size = "xs")
                          )
                        )
                      ),
                      mainPanel(
                        fluidRow(
                          column(width = 6, 
                                 h4("Ocean Harvest"),
                                 ggvisOutput("plot_oh")),
                          column(width = 6, 
                                 h4("Terminal Harvest"),
                                 ggvisOutput("plot_th"))
                          
                        ),
                        br(),
                        fluidRow(
                          column(width = 6, 
                                 h4("In-River Harvest"),
                                 ggvisOutput("plot_ir")),
                          column(width = 6, 
                                 h4("Hatchery Returns"),
                                 textOutput("hr_warning"),
                                 ggvisOutput("plot_hr"))
                        )
                      )
                    )
           ),
           tabPanel("About",
                    includeCSS("table.css"),
                    includeMarkdown("About.md"),
                    br(),
                    HTML('<center><a href = "http://www.fishsciences.net"><img src="cfs-logo_web-centered.jpg" width = "216" height = "147"/></a></center>')
           )
)

