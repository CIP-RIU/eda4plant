#' Server EDA for SweetPotato Base
#'
#'@description server side for EDA for SweetPotato Base
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @importFrom shiny reactive tabPanel renderUI selectInput icon h2 uiOutput radioButtons actionButton br column fluidRow plotOutput
#' @importFrom readxl read_excel
#' @export
#' 

edaplant_sbase_server <- function(input, output, session, values){
  

  values <- reactiveValues(fileInput = NULL)
  
  observe({
    
    shiny::withProgress(message = "Loading EDA module",value= 0,{
      
      #NOTE: To use pepa report package we need R 3.3.0 or more.
      #NOTE Finally, we always need pandoc installer.
      #ToDo: In case of poor conection print a message and do not show anything
      
      incProgress(1/5, detail = paste("..."))

      white_list <- brapi::ba_db()
      #establish connection
      incProgress(3/5, detail = paste("..."))
      sp_base_credentials <- white_list$sweetpotatobase
      trial_table <- brapi::ba_trials(con = sp_base_credentials)
      
      out <- list(sp_base_credentials  = sp_base_credentials , trial_table = trial_table)
      incProgress(5/5, detail = paste("..."))
      
      values$hot_bdata <- out
    })
    
  })
  
  #Sbase inputs
  
  # program name for single trial in sbase
  output$programName_eda_sbase  <- renderUI({
    
    #req(input$connect_single_sbase)
    
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    program_name <- sbase_data  %>% select(programName)
    program_name <- program_name %>% unique()
    
    selectInput('eda_selProgram_sbase', 'Select program', c(Choose='', program_name), selectize=TRUE)
    
    
  })
  
  #select trial name
  output$trialName_eda_sbase  <- renderUI({
    
    #req(input$connect_single_sbase)
    req(input$eda_selProgram_sbase)
    
    sel_programName <- input$eda_selProgram_sbase
    
    #sbase_data <- hot_bdata() #using button for connecting to SBASE
    sbase_data <- values$hot_bdata
    sbase_data <- sbase_data$trial_table
    
    sbase_data <- sbase_data %>% filter(programName == sel_programName)
    
    trial_name <- sbase_data %>% select(trialName)
    trial_name <- trial_name[[1]] %>% unique()
    
    selectInput('eda_sbase_trialName', 'Select trial', c(Choose='', trial_name), selectize=TRUE)
    
  })
  
  
  output$studyName_eda_sbase  <- renderUI({
    
    # req(input$connect_single_sbase)
    # req(input$single_selProgram_sbase)
    req(input$eda_sbase_trialName)
    sel_trialName <- input$eda_sbase_trialName
    
    #sbase_data <- hot_bdata() #using button for connecting to SBASE
    sbase_data <- values$hot_bdata #reactive data
    sbase_data <- sbase_data$trial_table
    
    sbase_data <- sbase_data %>% filter(trialName == sel_trialName)
    
    study_name <- sbase_data %>% select(studyName)
    study_name <- study_name[[1]] %>% unique()
    
    selectInput('eda_sbase_studyName', 'Select study', c(Choose='', study_name),multiple = TRUE, selectize=TRUE)
    
  })
  
  ### end sbase inputs
  
  
  hot_fb_sbase <- reactive({
    
    req(input$eda_sbase_studyName)
    #sbase_data <- hot_bdata() #extracting informatin from sbase (credentials and fieldbook) #using button for connecting to SBASE
    sbase_data <- values$hot_bdata
    sbase_fb <- sbase_data$trial_table
    credentials <- sbase_data$sp_base_credentials
    
    if(length(input$eda_sbase_studyName)==1){
    
   
   
    col_fb_sbase <- sbase_fb %>% dplyr::filter(programName== input$eda_selProgram_sbase, trialName == input$eda_sbase_trialName, studyName == input$eda_sbase_studyName)
    
    dt <-  ba_studies_table(credentials , studyDbId = as.character(col_fb_sbase$studyDbId))
    #dt
    }
    
    if(length(input$eda_sbase_studyName)>1){
      
      program <- input$eda_selProgram_sbase
      trial <- input$eda_sbase_trialName
      study <- input$eda_sbase_studyName
      
      #Vector with all the studies selected by users
      sel_multi_study <-  sbase_fb %>%  
                          filter(programName %in% program) %>% 
                          filter(trialName %in% trial) %>% 
                          filter(studyName %in% study)
      
      #id of selected studies
      id_study <- sel_multi_study$studyDbId
      
      #number of studies
      n <- length(id_study)
      #Inizialitation of empty list. It storages of all datasets selected by users 
      combine <- vector(mode="list", length = n)
      
      if(length(id_study)==0){return (NULL)}
      
      if(length(id_study)>=1 && length(id_study)<2 ) {
        flag <- FALSE
       shinysky::showshinyalert(session, "alert_met_sbase_done", paste("Select at least 3 studies (fieldbooks)"), styleclass = "warning")
        return (NULL)
      }
      
      if(length(id_study)>=2){
        
        #Inizialitation of environment vector.
        ENVIRONMENT <- vector(mode = "character", length = n )
        
        for(i in 1:n){
          
          combine[[i]] <-  brapi::ba_studies_table(credentials , studyDbId = as.character(id_study[i])) #get fieldbook and then storage
          ENVIRONMENT <- paste("ENV", unique(combine[[i]][["locationName"]]), i, sep="_")#create a differente environment ID despite of having the same location.
          #put environment columns aside to each fieldbook.
          combine[[i]] <- cbind(ENVIRONMENT, combine[[i]])
        }
        
        #join books. The fieldbook books were previously combined.
        join_books <- data.table::rbindlist(combine,fill = TRUE)
        join_books <- as.data.frame(join_books)
        #write.csv(join_books,"join_books.csv")
        shinysky::showshinyalert(session, "alert_met_sbase_done", paste("Great!. Perform your MET analysis"), styleclass = "success")
        #met_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
        #write.csv(join_books,"metdata.csv")
        join_books <- purrr::map_at(.x = join_books,.at = 1:16,.f = as.factor)  %>% as.data.frame(stringsAsFactors = FALSE)
        dt <- join_books
    }
 
  }
    #print(dt)
    dt <- dt
    
  })
  ###############

  
  #get inputs
 
  
  ##############
  
  output$sel_trait_eda <- renderUI({
    
    req(input$eda_sbase_studyName)
    #print(hot_fb_sbase())
    vars <- names(hot_fb_sbase())
    type_chart <- input$eda_type_chart
    
    selectInput(inputId = 'trait_eda', 'Select trait', c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_pairs_trait_eda <- renderUI({
    req(input$eda_sbase_studyName)
    #print(input$trait_eda)
    vars <- names(hot_fb_sbase())
    type_chart <- input$eda_type_chart
    
    selectInput(inputId = 'pairs_trait_eda', 'Select at least 2 traits',  c(Choose='', vars), selectize=TRUE, multiple = TRUE)
  })
  
  output$sel_bins_eda <- renderUI({
    req(input$eda_sbase_studyName)
    sliderInput(inputId = "bins_eda",label = "Bins", min = 0,max =  100,value = 10)
  })
  
  output$sel_gby_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    selectInput(inputId = 'gby_eda', 'Grouped by',  c(Choose='', vars),  selectize=TRUE)
    
  })
  
  output$sel_dots_eda <- renderUI({
    req(input$eda_sbase_studyName)
    req(input$trait_eda)  
    #vars <- names(hot_fb_sbase())
    selectInput(inputId = 'dots_eda', 'Use dots', choices = c("yes", "no"), selected = 1 )
  })
  
  
  output$sel_traitX_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    selectInput(inputId = 'trait_x_eda', 'Select trait (X)', c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_traitY_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
   
    selectInput(inputId = 'trait_y_eda', 'Select trait (Y)',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  ########### AMMI or GGE######################
  
  output$sel_trait_ammi_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    #print(vars)
    selectInput(inputId = 'trait_ammi_eda', 'Select trait',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_method_ammi_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    #print(vars)
    selectInput(inputId = 'method_ammi_eda', 'Select Method',  c("ammi", "gge"), selected = 1)
    
  })
  
  output$sel_env_ammi_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    #print(vars)
    selectInput(inputId = 'env_ammi_eda', 'Select Environment',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_gen_ammi_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    #print(vars)
    selectInput(inputId = 'gen_ammi_eda', 'Select Genotype',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  output$sel_rep_ammi_eda <- renderUI({
    req(input$eda_sbase_studyName)
    vars <- names(hot_fb_sbase())
    #print(vars)
    selectInput(inputId = 'rep_ammi_eda', 'Select Repetition',  c(Choose='', vars), selectize=TRUE)
    
  })
  
  ####### end AMMI or GGE ######################
  
  output$plot1 <- renderPlot({
    
    #tiff('test.tiff', units="in", width=5, height=5, res=300)
    # xcol <- input$xcol
    # ycol <- input$ycol
    req(input$eda_sbase_studyName)
    
    print(input$eda_sbase_studyName)  
    print(input$eda_selProgram_sbase)
    print(input$eda_sbase_trialName)
    
    
    fb <- as.data.frame(hot_fb_sbase()) #fieldboook
    
    # trait <- input$trait_eda    #trait  
    # trait<-  fb[, trait]
    
    gby <- input$gby_eda        #grouped by
    use_dots <- input$dots_eda  #plot points in charts
    bins <- input$bins_eda      #histogram
    
    # traitX <- input$trait_x_eda #scatterplot
    # traitX <- fb[,traitX]
    # 
    # traitY <- input$trait_y_eda #scatterplot
    # traitY <- fb[,traitY]
    
    # pairs_trait <-  input$pairs_trait_eda #pairsplot
    # pairs_trait <- fb[, pairs_trait]
    
    # print(fb)
    # print(trait)
    # print(use_dots)
    # print(gby)
    #print(bins)
    #cat(trait, use_dots, gby)
    
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