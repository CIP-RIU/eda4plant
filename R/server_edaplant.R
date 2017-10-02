#' Server EDA
#'
#'@description server side for EDA
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow plotOutput
#' @importFrom readxl read_excel
#' @export
#' 

edaplant_server <- function(input, output, session, values){
  
  
  volumes <- shinyFiles::getVolumes()
  shinyFiles::shinyFileChoose(input, 'file_eda', roots=volumes, session=session,
                              restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  
  hot_path <- reactive ({

    if(length(input$file_eda)==0){return (NULL)}
    if(length(input$file_eda)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_eda)$datapath)
    }
  })

  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  
    hot_bdata
  })
  

  output$sel_trait_eda <- renderUI({
    req(input$file_eda)
    vars <- names(hot_bdata())
    type_chart <- input$eda_type_chart

    selectInput(inputId = 'trait_eda', 'Select trait', c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_pairs_trait_eda <- renderUI({
    req(input$file_eda)
    vars <- names(hot_bdata())
    type_chart <- input$eda_type_chart
  
    selectInput(inputId = 'pairs_trait_eda', 'Select at least 2 traits',  c(Choose='', vars), selectize=TRUE, multiple = TRUE)
  })
  
  output$sel_bins_eda <- renderUI({
      req(input$file_eda)
      sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
  })
  
  output$sel_gby_eda <- renderUI({
    req(input$file_eda)
    vars <- names(hot_bdata())
    selectInput(inputId = 'gby_eda', 'Grouped by',  c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_dots_eda <- renderUI({
     req(input$file_eda)
     req(input$trait_eda)  
    #vars <- names(hot_bdata())
    selectInput(inputId = 'dots_eda', 'Use dots', choices = c("no", "yes"), selected = 1 )
  })
  
  
  output$sel_traitX_eda <- renderUI({
    req(input$file_eda)
    vars <- names(hot_bdata())
    selectInput(inputId = 'trait_x_eda', 'Select trait (X)', c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_traitY_eda <- renderUI({
    req(input$file_eda)
    vars <- names(hot_bdata())
    selectInput(inputId = 'trait_y_eda', 'Select trait (Y)',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$plot1 <- renderPlot({

    # xcol <- input$xcol
    # ycol <- input$ycol
    req(input$file_eda)
    
    
    # palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    # 
    # par(mar = c(5.1, 4.1, 0, 1))

    # xdata <- fb[, xcol]
    # ydata <- fb[, ycol]

    fb <- as.data.frame(hot_bdata()) #fieldboook
    trait <- input$trait_eda    #trait  
    gby <- input$gby_eda        #grouped by
    use_dots <- input$dots_eda  #plot points in charts
    bins <- input$bins_eda      #histogram
    
    traitX <- input$trait_x_eda #scatterplot
    traitY <- input$trait_y_eda #scatterplot
    
    pairs_trait <-  input$pairs_trait_eda #pairsplot
    # print(fb)
    # print(trait)
    # print(use_dots)
    # print(gby)
    #print(bins)
    #cat(trait, use_dots, gby)
    
    if(input$eda_type_chart=="boxplot"){

      req(input$trait_eda)
        
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
      
      if(gby == ""){
         res_plot <- plot_hist(trait, bins, fb = fb)
      }
      
      if(gby != ""){
        res_plot <- plot_hist(trait, bins, gby, fb)
      }

      res_plot <- res_plot
    }

    if(input$eda_type_chart == "scatterplot"){
       
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
      
        if(gby == ""){
          res_plot <- plot_dens(trait, fb = fb)
        }
        
        if(gby != ""){
          res_plot <- plot_dens(trait, gby, fb)
        }
        res_plot <- res_plot
  
    }
    
    if(input$eda_type_chart == "pairsplot"){
      fb <- fb
      
      cols_pairs <- lapply(X = 1:length(pairs_trait), function(x)which(names(fb)==pairs_trait[x])) %>% unlist()
      
      res_plot <- plot_pairs(traits = cols_pairs, fb = fb)

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
    
    res <- res_plot 
    
    print(res)
  })


}