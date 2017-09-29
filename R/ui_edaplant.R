#' UI for EDA
#'
#' @description user interface for EDA
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @importFrom shinydashboard box
#' @importFrom shinyFiles parseFilePaths
#' @export


edaplant_ui <- function(type = "tab", title = "Exploratory Data Analysis", name= "eda_plot_module"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          shinydashboard::box( 
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("EDA", #begin tabset "CHECK"
                                            fluidRow( 
                                              column(width = 12,
                                                     shinyFiles::shinyFilesButton('file_eda', 'Select File', 'Select a file',FALSE),
                                                     infoBoxOutput("file_message_eda", width = NULL) ,
                                                     
                                                     br(),
                                                     br(),
                                                    # fluidRow(
                                                     #  box(title = "Box title", "Box content", width = 10,
                                                           
                                                           plotOutput('plot1'),
                                                      #),
                                                    
                                                       #box(status = "warning", "Box content", width = 2, #begin second box
                                                           br(),
                                                           br(),
                                                           hr(),
                                                           #shinyFiles::shinyFilesButton('file_genetic', 'Select File', 'Select a file',FALSE),
                                                        #  div(id='sbase2_div',
                                                             selectInput("eda_type_chart", "Choose a graph", 
                                                                         choices = c("boxplot","scatterplot","pairsplot","histogram","density", "ammi"), 
                                                                         
                                                                         selected = 1),
                                                         # ) ,
                                                    
                                                           # uiOutput(outputId = "xcolumn",inline = TRUE),
                                                           # uiOutput(outputId = "ycolumn", inline = TRUE),
                                                           
                                                    
                                                         conditionalPanel(
                                                                          condition = "input.eda_type_chart =='boxplot'|
                                                                                       input.eda_type_chart=='histogram'|
                                                                                       input.eda_type_chart=='density'|
                                                                                       input.eda_type_chart=='ammi'", 
                                                                          
                                                                          uiOutput("sel_trait_eda")
                                                         ),
                                                    
                                                        conditionalPanel( 
                                                          condition = "input.eda_type_chart =='scatterplot'",
                                                          #uiOutput("sel_dots_eda")
                                                          #sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
                                                          uiOutput("sel_traitX_eda")
                                                        ),
                                                        
                                                        conditionalPanel( 
                                                          condition = "input.eda_type_chart =='scatterplot'",
                                                          #sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
                                                          uiOutput("sel_traitY_eda")
                                                        ),
                                                    
                                                    
                                                        conditionalPanel( 
                                                          condition = "input.eda_type_chart =='pairsplot'",
                                                          #sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
                                                          uiOutput("sel_pairs_trait_eda")
                                                        ),
                                                    
                                                    
                                                         conditionalPanel( 
                                                            condition = "input.eda_type_chart =='histogram'",
                                                            #uiOutput("sel_dots_eda")
                                                            # req(input$file_eda),
                                                            # sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
                                                            uiOutput("sel_bins_eda")
                                                          ),
                                                    
                                                         
                                                    
                                                          conditionalPanel(
                                                            condition = "input.eda_type_chart =='boxplot'|
                                                            input.eda_type_chart=='scatterplot'|
                                                            input.eda_type_chart=='histogram'|
                                                            input.eda_type_chart=='density'", 
                                                            
                                                            uiOutput("sel_gby_eda")
                                                          ),
                                                          
                                                          conditionalPanel(
                                                            condition = "input.eda_type_chart =='boxplot'",
                                                            uiOutput("sel_dots_eda")
                                                          )#,
                                                    
        
                                                    
                                                           # fluidRow(
                                                           #   column(3,
                                                           #          h4("Diamonds Explorer"),
                                                           #          sliderInput('sampleSize', 'Sample Size', 
                                                           #                      min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                                                           #                      step=500, round=0),
                                                           #          br(),
                                                           #          checkboxInput('jitter', 'Jitter'),
                                                           #          checkboxInput('smooth', 'Smooth')
                                                           #   ),
                                                           #   column(4, offset = 1,
                                                           #          selectInput('x', 'X', names(dataset)),
                                                           #          selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                                                           #          selectInput('color', 'Color', c('None', names(dataset)))
                                                           #   ),
                                                           #   column(4,
                                                           #          selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                                                           #          selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
                                                           #   )
                                                           # )   
                                      
                                                         #  ) #end second box
                                                                             
                                              ) #end column
                                            )
                                   )
                            )
                          )
  )
                                                     
  
  
}