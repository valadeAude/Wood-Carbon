

basicConfig()

options(shiny.error = function() { 
  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })


server <- function(input, output, session) {
  w<-Waiter$new()
  
  rv <- reactiveValues(
    # mode d'application du filtre TRUE=on applique le filtre, FALSE=toutes les données
    filter_data = TRUE,
    # a chaque fois qu'on incrémente cette valeur, le filtre sera réappliqué
    # apply_filter = 0
  )
  
  #data_fltr <- eventReactive(input$submitExp, {
    data_fltr <- eventReactive(list(rv$filter_data,input$submitExp), {
    if(rv$filter_data) {
      
      filtered_data <- data[data$scaleAgg %in% input$select_scale &
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
                            data$substitution >= input$select_substitution[1]&
                            data$substitution <= input$select_substitution[2]
                            ,]
    }else{
      filtered_data <- data
    }
    
   
  }, ignoreNULL = FALSE) 
  
  
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "select_scale", selected = c("world"="w","regional" = "reg", "local" = "loc"))
    updateSelectInput(session, "select_countries",selected = countriesList)
    updateCheckboxGroupInput(session, "select_single_product", selected = productsList)
    updateSelectInput( session,"select_time_horizon",selected = timeHorizonList)
    updateSelectInput( session, "select_processes",selected=character(0) )
    updateSelectInput(session, "select_dynamics", selected = character(0))
    updateSelectInput(session,"select_driver1Cat",selected = driver1CatList)
    updateSelectInput(session,"select_driver1", selected = driver1List)
    updateSliderInput(session,
                      inputId = "select_substitution",
                      label = "Substitution",
                      min = minValSub,
                      max = maxValSub,
                      value = c(minValSub, maxValSub),
    )#end sliderInput
},ignoreNULL=FALSE )
 
 
  
  
  data_bibliom_select<-reactive({bibliom_in(data_fltr()) })
  data_study_select<-reactive({study(data_fltr()) })
  data_country_select<-reactive({countryFreq(data_study_select(),countryRefData)})
  data_expt_select<-reactive({expt(data_fltr()) })
  data_expt_approach_select<-reactive({assignApproach(data_expt_select())})
  data_expt_unselect<-reactive({expt(data_unfltr()) })


  
  
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
    input$wrap_type_wood_fluxes
    input$wrap_type_wood_cor
    input$exclusion
    input$countryRanking
  })
  
  observe({ #dependence scale -> country
    dt <- sort(data$country[data$scaleAgg %in% input$select_scale])
    updatePickerInput(session, "select_countries", choices = dt, selected = dt)
  })

  
  observe({#dependence category -> driver
    dt <- data$driver1[data$driver1Cat %in% input$select_driver1Cat]
    updatePickerInput(session, "select_driver1", choices = dt, selected = dt)
  })
  
  
  output$filtered_db_table<-renderDT(
    unique(data_fltr()[,c("PaperID","DOI")]), options = list(lengthChange = FALSE))

## Database exploration
  observeEvent(list(input$submitExp), {
    rv$filter_data <- TRUE
  })
  
  # désactive le filtrage
  observeEvent(list(input$ignoreExp), {
    rv$filter_data <- FALSE
  })
  
  observeEvent(list(input$submitExp), {
    rv$filter_data <- TRUE
  })
  output$barplotYear<- renderPlotly({
    plotBarplotYear(data_bibliom_select())
  })
  
  output$countryData <- renderPlotly({
   
    plotlyCountryData<-plotCountryData(data_country_select(),input$countryRanking)
  }) 
  
  output$processes_plot <- renderPlotly({
    study_freq<-funcFreq(data_study_select(),categoriesdf)
    if ("Wrap by type of wood" %in% input$wrap_type_wood_processes) {
      create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),],"wrap" )
    }else{
      create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),] )
    }
  })
  
  output$processes_fluxes_plot <- renderPlotly({
    study_freq<-funcFreq(data_study_select(),categoriesdf)
    if ("Wrap by type of wood" %in% input$wrap_type_wood_fluxes) {
      plotlyProcessesFlux<-create_processes_versus_flux_size(refCProcessMean,study_freq ,"Set1",wood_type_names,"wrap")
    }else{
      plotlyProcessesFlux<-create_processes_versus_flux_size(refCProcessMean,study_freq ,"Set1",wood_type_names)
    }
  })
  
  output$driver_plot <- renderPlotly({
    expt_freq<-funcFreq(data_expt_select(),categoriesdf)
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
    create_dendrogram(res$data_expt_approach)
  
  })
  
  output$approachC<-renderPlot({

    if (res$filterResults=="filter") {
      plotData.tmp<-plotDataFunc(res$data_expt_approach, c("Whole sector approach","Technology approach","Ecosystem approach"),NULL,"modelApproach")
      forestPlotData.tmp<-forestPlotDataFunc(plotData.tmp,"modelApproach")
         }else{
           plotData.tmp<-plotData.approachC
           forestPlotData.tmp<-forestPlotData.approachC
        
    }
    create_forest_plot(plotData.tmp,forestPlotData.tmp,FALSE)
  })
  
  
  
  output$modelComponentsC<-renderPlotly({
    if (res$filterResults=="filter") {
      
    tTestPairsSignifAggVarMelt.tmp<-modelComponentsC(data_expt_approach_select(),c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
                                                 , "",("PaperID"))
    }else{
      tTestPairsSignifAggVarMelt.tmp<-tTestPairsSignifAggVarMelt
    }
   
    plotModelComponentsC(tTestPairsSignifAggVarMelt.tmp)
  })
  
  
  
 
  
  
  output$driverC<-renderPlot({

    if (res$filterResults=="filter") {
      plotData.driverC.tmp<-plotDataFunc(res$data_expt_approach, c("Whole sector approach"),NULL,"driver1")
      forestPlotData.driverC.tmp<-forestPlotDataFunc(plotData.driverC.tmp,"driver1","driver1Cat")
    
    }else{
           plotData.driverC.tmp<-plotData.driverC
      forestPlotData.driverC.tmp<-forestPlotData.driverC

     }
    create_forest_plot(plotData.driverC.tmp,forestPlotData.driverC.tmp,TRUE)
  })
  
  
  
  output$knowledgeDyn<-renderPlot({

    if (res$filterResults=="filter") {
      forestPlotData.approachC.dyn.tmp<-knowDynamicsData(res$data_expt_approach)
    }else{
      forestPlotData.approachC.dyn.tmp<-forestPlotData.approachC.dyn
    }
    create_knowDynamicsPlot(forestPlotData.approachC.dyn.tmp)
  })
  
  
  output$substitution_average_bars_drivers <- renderPlotly({
    plotSubstitutionBars<-create_substitution_average_bars_drivers(data_expt_select()) 
  })
  output$substitution_average_bars_processes <- renderPlotly({
    plotSubstitutionBars<-create_substitution_average_bars_processes(data_expt_select()) 
  })

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

