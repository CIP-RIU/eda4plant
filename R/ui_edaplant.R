#' UI for EDA
#'
#' @description user interface for EDA
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export


edaplant_ui <- function(type = "tab", title = "Exploratory Analysis", name= "eda_plot_module"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("EDA", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 12,
                                                     #shinyFiles::shinyFilesButton('file_eda', 'Select File', 'Select a file',FALSE),
                                                     #infoBoxOutput("file_message_genetic", width = NULL) ,
                                                     
                                                     br(),
                                                     br(),
                                                    # fluidRow(
                                                       box(title = "Box title", "Box content",width = 10,
                                                           
                                                           
                                                           
                                                           plotOutput('plot1')
                                                           
                                                           
                                                      ),
                                                       box(status = "warning", "Box content",width = 2,
                                                           br(),
                                                           br(),
                                                           
                                                           #shinyFiles::shinyFilesButton('file_genetic', 'Select File', 'Select a file',FALSE),
                                                           selectInput("eda_type_chart", "Choose a graphic", choices = c("scatter","histogram"),selected = 1),
                                                           selectInput('xcol', 'X Variable', names(iris), names(iris)[[1]]),
                                                           selectInput('ycol', 'Y Variable', names(iris),selected=names(iris)[[2]])#,
                                                           # numericInput('clusters', 'Cluster count', 3,
                                                           #              min = 1, max = 9)#,
                                                           )
                                                    # )#,
                                                     
                                                     #shiny::selectInput(inputId = "omar", label = "Un label", choices = 1:10,selected = 1)
                                                     
                                              ) #end column
                                            )
                                   )
                            )
                          )
  )
                                                     
  
  
}