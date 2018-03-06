#' Server EDA
#' Return the server component for exploratory graphics
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @description server side for EDA
#' @importFrom shiny tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow plotOutput
#' @importFrom readxl read_excel
#' @export
#' 

edaplant_server <- function(input, output, session, values){
  
  
  #volumes <- shinyFiles::getVolumes()
  # shinyFiles::shinyFileChoose(input, 'file_eda', roots=volumes, session=session,
  #                             restrictions=system.file(package='base'),filetypes=c('xlsx'))
  # 
  
  # hot_path <- reactive ({
  # 
  #   # if(length(input$file_eda)==0){return (NULL)}
  #   # if(length(input$file_eda)>0){
  #   #   hot_file <- as.character(parseFilePaths(volumes, input$file_eda)$datapath)
  #   # # }
  # 
  #   hot_file <- input$file_eda_input
  #   
  #   if(is.null(hot_file)){return()}
  #   if(!is.null(hot_file)){
  #     
  #     file.copy(hot_file$datapath, paste(hot_file$datapath, ".xlsx", sep=""))
  #     fieldbook <- readxl::read_excel(paste(hot_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook")
  #     #mtl_list <- as.list(hot_file) #mtl in list format
  #   }
  #   
  # })

  hot_bdata <- reactive({
 
    hot_file <- input$file_eda_input
    
    if(is.null(hot_file)){return()}
    if(!is.null(hot_file)){
      
      file.copy(hot_file$datapath, paste(hot_file$datapath, ".xlsx", sep=""))
      fieldbook <- readxl::read_excel(paste(hot_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook")
      #mtl_list <- as.list(hot_file) #mtl in list format
    }
   
    fieldbook
    
  })
  

  output$sel_trait_eda <- renderUI({
    #req(input$file_eda)
    req(input$file_eda_input)
    print(hot_bdata())
    vars <- names(hot_bdata())
    type_chart <- input$eda_type_chart

    selectInput(inputId = 'trait_eda', 'Select trait', c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_pairs_trait_eda <- renderUI({
    #req(input$file_eda)
    req(input$file_eda_input)
    vars <- names(hot_bdata())
    type_chart <- input$eda_type_chart
  
    selectInput(inputId = 'pairs_trait_eda', 'Select at least 2 traits',  c(Choose='', vars), selectize=TRUE, multiple = TRUE)
  })
  
  output$sel_bins_eda <- renderUI({
      #req(input$file_eda)
      req(input$file_eda_input)
      sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
  })
  
  output$sel_gby_eda <- renderUI({
    #req(input$file_eda)
    req(input$file_eda_input)
    vars <- names(hot_bdata())
    selectInput(inputId = 'gby_eda', 'Grouped by',  c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_dots_eda <- renderUI({
     #req(input$file_eda)
    req(input$file_eda_input) 
    req(input$trait_eda)  
    #vars <- names(hot_bdata())
    selectInput(inputId = 'dots_eda', 'Use dots', choices = c("no", "yes"), selected = 1 )
  })
  
  
  output$sel_traitX_eda <- renderUI({
    #req(input$file_eda) 
    req(input$file_eda_input)
    vars <- names(hot_bdata())
    selectInput(inputId = 'trait_x_eda', 'Select trait (X)', c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_traitY_eda <- renderUI({
    #req(input$file_eda)
    req(input$file_eda_input)
    vars <- names(hot_bdata())
    selectInput(inputId = 'trait_y_eda', 'Select trait (Y)', c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$plot1 <- renderPlot({
    
    req(input$file_eda_input)
    
    fb <- as.data.frame(hot_bdata()) #fieldboook
    print(fb)
    
    gby <- input$gby_eda        #grouped by
    use_dots <- input$dots_eda  #plot points in charts
    bins <- input$bins_eda      #histogram
    
    if(input$eda_type_chart=="boxplot"){
      
      req(input$trait_eda)
      trait <- input$trait_eda    #trait  
      trait<-  fb[, trait]
      
      
      #req(input$trait_eda)
      
      if(gby == "" || is.null(gby)){
        
        res_plot <- plot_box(trait = trait, fb = fb)
        
        if(use_dots == "yes"){
          res_plot <- plot_box(trait = trait, dots = use_dots, fb = fb)
        }
        
      }
      
      if(gby!= ""){
        
        res_plot <- plot_box(trait = trait, by = gby, fb = fb)
        
        if(use_dots =="yes"){
          res_plot <- plot_box(trait = trait,by =  gby,dots =  use_dots, fb = fb)
        }
        
      }
      
      res_plot <- res_plot
      
    }
    
    if(input$eda_type_chart=="histogram"){
      
      req(input$trait_eda)
      trait <- input$trait_eda    #trait  
      trait<-  fb[, trait]
      
      if(gby == ""){
        res_plot <- plot_hist(trait, bins, fb = fb)
      }
      
      if(gby != ""){
        res_plot <- plot_hist(trait, bins, gby, fb)
      }
      
      res_plot <- res_plot
    }
    
    if(input$eda_type_chart == "scatterplot"){
      
      req(input$trait_x_eda)
      req(input$trait_y_eda)
      
      traitX <- input$trait_x_eda #scatterplot
      traitX <- fb[,traitX]
      
      traitY <- input$trait_y_eda #scatterplot
      traitY <- fb[,traitY]
      
      
      if(gby == ""){
        res_plot <- plot_scat(traitX, traitY, fb = fb)
      }
      
      if(gby != ""){
        res_plot <- plot_scat(traitX, traitY, gby, fb)
      }
      res_plot <- res_plot
      #res <- plot_corr(fb, xcol= xcol, ycol = ycol)
      
    }
    
    if(input$eda_type_chart == "density"){
      
      req(input$trait_eda)
      
      trait <- input$trait_eda    #trait  
      trait<-  fb[, trait]
      
      if(gby == ""){
        res_plot <- plot_dens(trait, fb = fb)
      }
      
      if(gby != ""){
        res_plot <- plot_dens(trait, gby, fb)
      }
      res_plot <- res_plot
      
    }
    
    if(input$eda_type_chart == "pairsplot"){
      
      req(input$pairs_trait_eda)
      
      pairs_trait <-  input$pairs_trait_eda #pairsplot
      pairs_trait <- fb[, pairs_trait]
      
      fb <- fb
      cols_pairs <- lapply(X = 1:length(pairs_trait), function(x)which(names(fb)==pairs_trait[x])) %>% unlist()
      
      res_plot <- plot_pairs(traits = cols_pairs, fb = fb)
      
    }
    
    if(input$eda_type_chart == "ammi"){
      
      req(input$trait_ammi_eda)
      req(input$method_ammi_eda)
      req(input$env_ammi_eda)
      req(input$gen_ammi_eda)
      req(input$rep_ammi_eda)
      
      fb <- hot_fb_sbase()  
      trait <- input$trait_ammi_eda    #trait
      #trait<-  fb[, trait]
      method <- input$method_ammi_eda
      env <- input$env_ammi_eda
      gen <- input$env_ammi_eda
      rep <- input$rep_ammi_eda
      
      
      
      if(method=="ammi") {  
        model <- ammi(trait, gen, env, rep, data =fb,  method = "ammi" )
        res_plot  <- plot_ammi(model, 1) 
        
      }
      if(method=="gge")  {  
        model <- ammi(trait, gen, env, rep, data =  fb, method = "gge" )
        res_plot <- plot_ammi(model, 2) 
      }
      
      
      
    }
    
    
    
    if(input$eda_type_chart=="boxplot" || input$eda_type_chart=="histogram" || input$eda_type_chart == "scatterplot" 
       || input$eda_type_chart == "density" || input$eda_type_chart == "pairsplot"){
      
      if(input$sel_orientation_eda=="none"){
        res_plot <- res_plot + theme(axis.text.x=element_text(angle=0, hjust=1, vjust=0.5))
      }
      
      if(input$sel_orientation_eda=="45°"){
        res_plot <- res_plot + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5))
      } 
      
      if(input$sel_orientation_eda=="90°"){  
        res_plot <- res_plot + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
      }
      #source https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2 
    }
    
    
    
    if(input$xlabel_graph!=""){
      res_plot <- res_plot + xlab(input$xlabel_graph)
    }
    
    if(input$ylabel_graph!=""){
      res_plot <- res_plot + ylab(input$ylabel_graph)
    }
    
    res <- res_plot 
    #dev.off()
    
    print(res)
    
  })


}