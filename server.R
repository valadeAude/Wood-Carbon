library(shiny)
library(plotly)
library(shinyWidgets)
library(shinybusy)
library(magrittr)
library(shinyjs)
library(logging)

basicConfig()

options(shiny.error = function() { 
  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })


server <- function(input, output, session) {
  w<-Waiter$new()
  
 
  
  data_fltr <- eventReactive(input$submit, {
    filtered_data <- data[data$scaleAgg %in% input$select_scale&
                            data$country %in% input$select_countries&
                            data$singleProduct %in% input$select_single_product&
                            data$time_horizon %in% input$select_time_horizon&
                            rowSums(data[, input$select_processes]) == length(input$select_processes)  &                          
                            rowSums(data[, input$select_dynamics] )== length(input$select_dynamics)  &                          
                            #                            rowSums(data[, input$select_processes]) >0  &                          
                            #                           rowSums(data[, input$select_dynamics] ) >0  &     
                            # data$boundaries %in% input$select_boundaries&
                            #                            data$dynamics %in% input$select_dynamics&
                            
                            data$driver1 %in% input$select_driver1&
                            data$driver1Cat %in% input$select_driver1Cat&
                            (  
                              (data$substitution >= input$select_substitution[1]&
                                 data$substitution <= input$select_substitution[2])|
                                is.na(data$substitution)
                            ),]
    
    if (is.null(input$select_NA_substitution)) {
      filtered_data <- filtered_data[!is.na(filtered_data$substitution), ]
    }
     else{
       filtered_data <- filtered_data
     
     }
    
    #lost_data<-data[!(data$DOI %in% filtered_data),]
  }, ignoreNULL = FALSE) 
  
  
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "select_scale", selected = c("world"="w","regional" = "reg", "local" = "loc"))
    updateSelectInput(session, "select_countries",selected = countries)
    updateCheckboxGroupInput(session, "select_single_product", selected = products)
    updateSelectInput( session,"select_time_horizon",selected = timeHorizon)
    updateSelectInput( session, "select_processes",selected=character(0) )
    updateSelectInput(session, "select_dynamics", selected = character(0))
    updateSelectInput(session,"select_driver1Cat",selected = driver1Cat)
    updateSelectInput(session,"select_driver1", selected = driver1)
    updateSliderInput(session,
                      inputId = "select_substitution",
                      label = "Substitution",
                      min = minValSub,
                      max = maxValSub,
                      value = c(minValSub, maxValSub),
    )#end sliderInput
},ignoreNULL=FALSE )
 

  data_unfltr <- eventReactive(input$submit, {
    unfiltered_data <- data[!(data$scaleAgg %in% input$select_scale)|
                              !(data$country %in% input$select_countries)|
                              !(data$singleProduct %in% input$select_single_product)|
                              !(data$time_horizon %in% input$select_time_horizon)|
                              !(data$boundaries %in% input$select_boundaries)|
                              !(data$dynamics %in% input$select_dynamics)|
                              !(data$driver1 %in% input$select_driver1)|
                              !(data$driver1Cat %in% input$select_driver1Cat),]
    
    # if (is.null(input$select_NA_substitution)) {
    #   unfiltered_data <- unfiltered_data[is.na(filtered_data$substitution), ]
    # }
    # else{
    #   unfiltered_data <- unfiltered_data
    #   
    # }
    
    #lost_data<-data[!(data$DOI %in% filtered_data),]
  }, ignoreNULL = FALSE)     
  data_bibliom_select<-reactive({bibliom(data_fltr()) })
  data_study_select<-reactive({study(data_fltr()) })
  data_country_select<-reactive({country(data_fltr())})
  data_expt_select<-reactive({expt(data_fltr()) })
  data_expt_approach_select<-reactive({assignApproach(data_expt_select())})
  data_expt_unselect<-reactive({expt(data_unfltr()) })

  #Test reactive for intermediary variables
  
  
  
  
  wt_select<-reactive({
    tmp<-data_expt_select()
    levels(droplevels(tmp$singleProduct))
  })

  observe({
    input$select_time_horizon
    input$select_boundaries
    input$select_dynamics
    input$select_driver1
    input$select_driver1Cat
    input$select_substitution
    input$wrap_type_wood_map
    input$wrap_type_wood_processes
    input$wrap_type_wood_cor
    input$exclusion
  })
  
  observe({ #dependence scale -> country
    dt <- data$country[data$scaleAgg %in% input$select_scale]
    updatePickerInput(session, "select_countries", choices = dt, selected = dt)
  })
  cat(file=stderr(), "exit updatepicker")
  
  
  observe({#dependence category -> driver
    dt <- data$driver1[data$driver1Cat %in% input$select_driver1Cat]
    updatePickerInput(session, "select_driver1", choices = dt, selected = dt)
  })
  
  
  output$filtered_db_table<-renderDT(
    unique(data_fltr()[,c("PaperID")]), options = list(lengthChange = FALSE))
  
  output$barplotYear<- renderPlotly({
    #plotBarplotYear(data_bibliom_select())
    plotBarplotYear(data_bibliom_select())
  })
  
  output$countryData <- renderPlotly({
    plotlyCountryData<-plotCountryData(data_country_select(),"Forest.area..1000.ha.")
  }) 
  
  output$processes_plot <- renderPlotly({
    study_freq<-funcFreq(data_study_select(),categoriesdf)
    #Debug info
    print("in server: processes_plot")
    ##
    if ("Wrap by type of wood" %in% input$wrap_type_wood_processes) {
      create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),],"wrap" )
    }else{
      create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),] )
      
    }
  })
  
  output$processes_fluxes_plot <- renderPlotly({
    print("in server: processes_fluxes_plot")
    study_freq<-funcFreq(data_study_select(),categoriesdf)
    print(head(study_freq))
    if ("Wrap by type of wood" %in% input$wrap_type_wood_processes) {
      
      plotlyProcessesFlux<-create_processes_versus_flux_size(study_freq ,"Set1","wrap")
    }else{
      plotlyProcessesFlux<-create_processes_versus_flux_size(study_freq ,"Set1")
    }
  })
  
  output$driver_plot <- renderPlotly({
    expt_freq<-funcFreq(data_expt_select(),categoriesdf)
    #Debug info
    print("in server")
    print(input$wrap_type_wood_processes)
    ##
    if ("Wrap by type of wood" %in% input$wrap_type_wood_drivers) {
      create_driver_frequency(expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),] ,"wrap")
    }else{
      create_driver_frequency(expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),] )
      
    }
  })
  
  
  
  
  
  ## Results section -> use plotData reactive variables that respond to input$submitResults and input$resetResults
 
  res <- reactiveValues(data_expt = data_expt, data_bibliom=data_bibliom,filterResults="no")
  
  observeEvent(input$submitResults, {
    res$data_expt<- data_expt_select()
    res$data_expt_approach<- data_expt_approach_select()
    res$data_bibliom<-data_bibliom_select()
    res$filterResults<-"filter"
  })
  
  observeEvent(c(input$resetResults,input$ignore), {
    res$data_expt <- data_expt
    res$data_expt_approach <- data_expt_approach
    res$data_bibliom <- data_bibliom
    res$filterResults<-"no"
    
  })  
  
  output$dendrogram<-renderPlot({
    print("in server : dendrogram")
    data_expt_approachResults<-assignApproach(res$data_expt)
    create_dendrogram(data_expt_approachResults)
  
  })
  
  output$approachC<-renderPlot({
    #data_expt_approach<-assignApproach(data_expt) #nminTechno,nmaxTechno,nminEcos,nmaxEcos
    #     plotApproachC<-approachC(data_expt_approach)
    data_expt_approachResults<-assignApproach(res$data_expt)
    if (res$filterResults=="filter") {
      print("filters included")
      plotData<-plotDataFunc(data_expt_approachResults, c("Whole sector approach","Technology approach","Ecosystem approach"),NULL,"modelApproach")
      forestPlotData<-forestPlotDataFunc(plotData,"modelApproach",FALSE)
         }else{
      print("no filters")
      plotData<-read.csv(paste0(initDataPath,"plotData.approachC.csv"))
      forestPlotData<-read.csv(paste0(initDataPath,"forestPlotData.approachC.csv"))
    }
    create_forest_plot(plotData,forestPlotData,FALSE)
  })
  
  output$modelComponentsC<-renderPlotly({
    tTestPairsSignifAggVarMelt<-modelComponentsC(data_expt_approach_select(),c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
                                                 , "",("PaperID"))
    
    #  if ("Apply database filters" %in% input$database_filters) {
    #data_expt_approach<-assignApproach(data_expt_select()) #nminTechno,nmaxTechno,nminEcos,nmaxEcos
    #tTestPairsSignifAggVarMelt<-modelComponentsC(data_expt_approach(),c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
    #                                            , "",("PaperID"))
    #  plotModelComponentsC(tTestPairsSignifAggVarMelt())
    #  }else{
    plotModelComponentsC(tTestPairsSignifAggVarMelt)
    #   }    
  })
  
  
  output$driverC<-renderPlot({
    data_expt_approachResults<-assignApproach(res$data_expt)
    
    if (res$filterResults=="filter") {
      plotData.driverC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach"),NULL,"driver1")
      forestPlotData.driverC<-forestPlotDataFunc(plotData.driverC,"driver1",TRUE)
    #  plotData.driverC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach"),NULL,"driver1Cat")
     # forestPlotData.driverC<-forestPlotDataFunc(plotData.driverC,"driver1Cat",FALSE)      
    }else{
     plotData.driverC<-read.csv(paste0(initDataPath,"plotData.driverC.csv"))
      forestPlotData.driverC<-read.csv(paste0(initDataPath,"forestPlotData.driverC.csv"))
   #   plotData.driverC<-read.csv(paste0(initDataPath,"plotData.driverCatC.csv"))
    #  forestPlotData.driverC<-read.csv(paste0(initDataPath,"forestPlotData.driverCatC.csv"))
      
     }
    create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
  })
  
  
  
  output$substitution_average_bars_drivers <- renderPlotly({
    plotSubstitutionBars<-create_substitution_average_bars_drivers(data_expt_select()) 
  })
  output$substitution_average_bars_processes <- renderPlotly({
    plotSubstitutionBars<-create_substitution_average_bars_processes(data_expt_select()) 
  })
  #displays the number of rows of the filtered_data
  output$summaryExpt <- renderText({
    paste("Records processed:", nrow(data_expt_select()),"/",nrow(data_expt), sep = " ")
    
  })
  output$summaryExptUnFilt <- renderText({
    paste("Untreated records:", data_expt_unselect(), sep = " ")
    
  })  
  output$summaryStudy <- renderText({
    paste("Publications processed:", nrow(data_bibliom_select()), "/",nrow(data_bibliom), sep = " ")
    
  })
  output$summaryExptBox <- renderInfoBox({
    infoBox(
      "Records", paste0(nrow(data_expt_select()),"/",nrow(data_expt)), icon = icon("list"),
      color = "maroon"
    )
  })
  output$summaryStudyBox <- renderInfoBox({
    infoBox(
      "Publications", paste0(nrow(data_bibliom_select()), "/",nrow(data_bibliom)), icon = icon("list"),
      color = "orange"
    )
  })
  output$summaryExptBoxResults <- renderInfoBox({
    infoBox(
      "Records", paste0(nrow(res$data_expt),"/",nrow(data_expt)), icon = icon("list"),
      color = "maroon"
    )
  })
  output$summaryStudyBoxResults <- renderInfoBox({
    infoBox(
      "Publications", paste0(nrow(res$data_bibliom), "/",nrow(data_bibliom)), icon = icon("list"),
      color = "orange"
    )
  })
  
  output$downloadDatabase <- downloadHandler(
    filename = "database_substitution_metaanalysis.v5.ALL.QC.xlsx",
    content = function(file) {
      file.copy("database_substitution_metaanalysis.v5.ALL.QC.xlsx", file)
    }
  )
  
  
  
}

#pour commenter ou décommenter plusieurs lignes sélectionnées 
# ctrl+shift+c

