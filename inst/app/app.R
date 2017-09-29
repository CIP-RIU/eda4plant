library(shiny)
#library(DT)
library(shinydashboard)
library(shinyFiles)
library(ggplot2)
#library(st4gi)
#library(pepa)
library(readxl)
library(eda4plant)

#library(knitr)

tabNameS <- "eda_graph"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  eda4plant::edaplant_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = ""),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("EDA", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        eda4plant::edaplant_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)


