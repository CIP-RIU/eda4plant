#' Server EDA
#'
#'@description server side for EDA
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow 
#' @importFrom shinydashboard infoBox tabBox infoBoxOutput renderInfoBox
#' @importFrom shinyFiles parseFilePaths
#' @importFrom readxl read_excel
#' @export
#' 

edaplant_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_genetic', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({
    
    #validate(
    #  need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    #)
    
    if(length(input$file_genetic)==0){return (NULL)}
    if(length(input$file_genetic)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_genetic)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  # clusters <- reactive({
  #   kmeans(selectedData(), input$clusters)
  # })
  # 
  output$plot1 <- renderPlot({
    
    xcol <- input$xcol
    ycol <- input$ycol
    
    fb <- selectedData()
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    
    if(input$eda_type_chart=="histogram"){
      
      gg <-  ggplot(data = fb, aes_(x = xcol)) +
        geom_histogram(binwidth= 0.5, color="black", fill="grey", size=1)+
        #geom_histogram( aes(y =..density..) , bins = 50, color="black", fill="grey", size=1)+
        # geom_vline(aes_(xintercept= mean(xcol)),
        #            color="red", linetype="dashed", size=1)+
        labs(x = "BIOM", y = "Number of genotypes")+
        scale_color_manual(values = c("#00AFBB", "#E7B800"))+
        scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
        #geom_density(col=2)+
        theme_classic()+
        theme(
          axis.text.x = element_text(size = 12, colour = "black",face = "bold"),
          axis.text.y = element_text(size = 12, colour = "black",face = "bold"),
          axis.line.x = element_line(colour = "black", size = 1),
          axis.line.y = element_line(colour = "black", size = 1),
          legend.position = "bottom"
        )
      gg
    }
    
    
   if(input$eda_type_chart == "Scatter"){
     
     
       gg <- ggplot(fb, aes_(x=xcol, y=y_col)) + 
       #geom_point(aes_(col= , size= )) + 
       geom_smooth(method="loess", se=F) + 
       xlim(c(0, 0.1)) + 
       ylim(c(0, 500000)) + 
       labs(subtitle="Area Vs Population", 
            y="Population", 
            x="Area", 
            title="Scatterplot", 
            caption = "Source: midwest")
     
     
     
     
   }
    
    
    gg
    
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    # 
    
    
  })
  
  
  
  
  
  
}