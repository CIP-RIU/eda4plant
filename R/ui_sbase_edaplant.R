#' UI for EDA for SweetPotato Base
#'
#' @description user interface for EDA for SweetPotato Base
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title title name
#' @param name UI TabName
#' @importFrom shinydashboard box
#' @importFrom shinyFiles parseFilePaths
#' @export

edaplant_sbase_ui <- function(type = "tab", title = "Exploratory Data Analysis", name= "eda_plot_module"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          shinydashboard::box( 
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("EDA", #begin tabset "EDA"
                                            fluidRow( 
                                              column(width = 12, #begin column principal layout
                                                     #shinyFiles::shinyFilesButton('file_eda', 'Select File', 'Select a file',FALSE),
                                                     #infoBoxOutput("file_message_eda", width = NULL) ,
                                                     
                                                     fluidRow( #begin fluidRow edatypechart
                                                       column(3, #begin column 3
                                                            uiOutput("programName_eda_sbase")
                                                        ),
                                                       
                                                        column(4, offset = 1, 
                                                            uiOutput("trialName_eda_sbase")
                                                        ),
                                                       column(4,
                                                            uiOutput("studyName_eda_sbase")
                                                       )#,
                                                     ),
                                                     
                                                     br(),
                                                     br(),
                                                     
                                                     shinysky::shinyalert("alert_eda_done", FALSE, auto.close.after = 5),
                                                     
                                                     shinycustomloader::withLoader(plotOutput('plot1'), type = "html", loader = "loader4"),
                                                     
                                                     br(),
                                                     br(),
                                                     hr(),
                                                     
                                                     fluidRow( #begin fluidRow edatypechart
                                                       column(3, #begin column 3
                                                              
                                                              
                                                              selectInput("eda_type_chart", "Choose a graph", 
                                                                          #choices = c("boxplot","scatterplot","pairsplot","histogram","density", "ammi"), 
                                                                          choices = c("boxplot","scatterplot","histogram","density", "pairsplot"),
                                                                          
                                                                          selected = 1),
                                                              
                                                              conditionalPanel(
                                                                condition = "input.eda_type_chart =='boxplot'|
                                                                input.eda_type_chart=='histogram'|
                                                                input.eda_type_chart=='density'",#|
                                                                #input.eda_type_chart=='ammi'", 
                                                                
                                                                uiOutput("sel_trait_eda")
                                                              ),
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='scatterplot'",
                                                                uiOutput("sel_traitX_eda")
                                                              ),
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='scatterplot'",
                                                                uiOutput("sel_traitY_eda")
                                                              ),
                                                              
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='pairsplot'",
                                                                #sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
                                                                uiOutput("sel_pairs_trait_eda")
                                                              ),
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='histogram'",
                                                                uiOutput("sel_bins_eda")
                                                              ),
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='ammi'",
                                                                uiOutput("sel_trait_ammi_eda"),
                                                                shinysky::shinyalert("alert_met_sbase_done", FALSE, auto.close.after = 1)
                                                              ),
                                                              
                                                              conditionalPanel( 
                                                                condition = "input.eda_type_chart =='ammi'",
                                                                uiOutput("sel_method_ammi_eda"),
                                                                uiOutput("sel_env_ammi_eda"),
                                                                uiOutput("sel_gen_ammi_eda"),
                                                                uiOutput("sel_rep_ammi_eda")
                                                              )#,
                                                              
                                                              
                                                       ),#end column 3
                                                       #),#end fluidRow edatypechart
                                                       
                                                       
                                                       #f5f5f5
                                                       column(3, offset = 1,  #begin second column #Graphic settings
                                                              
                                                              
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
                                                              ),
                                                              
                                                              conditionalPanel(
                                                                condition = "input.eda_type_chart =='boxplot'|
                                                                input.eda_type_chart=='scatterplot'|
                                                                input.eda_type_chart=='density'|
                                                                input.eda_type_chart=='pairsplot'|
                                                                input.eda_type_chart=='histogram'", 
                                                                selectInput(inputId = 'sel_orientation_eda', 'Orientation of the label (Degrees °)', 
                                                                            c("none", "45°", "90°"), selected = "none", selectize=TRUE)
                                                              )
                                                              
                                                     ),  #end second column #Graphic settings
                                                     
                                                     
                                                     column(3,
                                                            textInput(inputId = "xlabel_graph",
                                                                      label = "X label",
                                                                      value = ""),
                                                            textInput(inputId = "ylabel_graph",
                                                                      label = "Y label",
                                                                      value = "")#,
                                                            #downloadButton('downloadReport', label = "Download the plot")
                                                            
                                                            #          selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                                                            #          selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
                                                       ),
                                                     
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                     
                                                     
                                                     ),#end fluidRow edatypechart      
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                     
                                                     
                                                   ), #end column layout
                                                    
                                              br(),
                                              br(),
                                              br()   
                                                 
                                              
                                              
                                               ),
                                            br(),
                                            br(),
                                            br()
                                            
                                            
                                            
                                            )#End TAB Tabset "EDA"
                            )#End Tab Box
                    ), #End Box
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
  
  
            )
  
  
  
}