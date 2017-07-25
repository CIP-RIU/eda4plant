#' Server EDA
#'
#'@description server side for EDA
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow 
#@importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#@importFrom shinyFiles parseFilePaths
#@importFrom readxl read_excel
#' @export
#' 

edaplant_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_eda', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  # hot_path <- reactive ({
  # 
  #   #validate(
  #   #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
  #   #)
  # 
  #   if(length(input$file_genetic)==0){return (NULL)}
  #   if(length(input$file_genetic)>0){
  #     hot_file <- as.character(parseFilePaths(volumes, input$file_genetic)$datapath)
  #   }
  # })
  # 
  # hot_bdata <- reactive({
  #   hot_file <- hot_path()
  #   if(length(hot_file)==0){return (NULL)}
  #   if(length(hot_file)>0){
  #     hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
  #   }
  #   dat <- iris
  #   dat
  # })
  
  
  
  selectedData <- reactive({
    iris
  })
  
  # clusters <- reactive({
  #   kmeans(selectedData(), input$clusters)
  # })
  # 
  output$plot1 <- renderPlot({
    
    
    
    xcol <- input$xcol
    ycol <- input$ycol
    
    fb <- selectedData()
    # print(xcol)
    # print(ycol)
    # print(fb)
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    
    
    xdata <- fb[, xcol]
    ydata <- fb[, ycol]
    
    print(xcol)
    # print(ycol)
    
    if(input$eda_type_chart=="histogram"){

      res <- plot_hist(fb, xcol = xdata )
      
    }
    
    
   if(input$eda_type_chart == "scatter"){
     
     res <- plot_corr(fb, xcol= xdata, ycol = ydata)
     
   }
    
   if(input$eda_type_chart == "boxplot"){
      
      res <- plot_bplot(fb, xcol= xdata, ycol = ydata)
      
    }
    
    
    res
    
    #print(gg)
    
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    # 
    
    
  })
  
  
  
  
  
  
}