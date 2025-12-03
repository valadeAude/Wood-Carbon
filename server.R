

basicConfig()

options(shiny.error = function() {
  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })


server <- function(input, output, session) {
  w<-Waiter$new()

  legalNoticeHandler(includeMarkdown("TextContent/legal_notice.md"))

  rv <- reactiveValues(
    # mode d'application du filtre TRUE=on applique le filtre, FALSE=toutes les données
    filter_data = TRUE,
    # a chaque fois qu'on incrémente cette valeur, le filtre sera réappliqué
    # apply_filter = 0
  )

  #data_fltr <- eventReactive(input$submitExp, {
    data_fltr <- eventReactive(list(rv$filter_data,input$submitExp), {
    if(rv$filter_data) {

      filtered_data <- data[
                            data$country %in% input$select_countries&
                            data$scaleAgg %in% input$select_scale &
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
    print(paste("check dim :data",dim(data)))
    

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
  data_country_select<-reactive({countryFreq(data_expt_select(),countryRefData)})
  data_expt_select<-reactive({expt(data_fltr()) })
  data_expt_approach_select<-reactive({assignApproach(data_expt_select())})
  data_expt_assumption_select<-reactive({assignAssumption(data_expt_select())})
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
    expt_freq<-funcFreq(data_expt_select(),categoriesdf)
    if ("Wrap by type of wood" %in% input$wrap_type_wood_processes) {
      create_processes_frequency(expt_freq[(expt_freq$cat1 =="Processes") & !is.na(expt_freq$cat1),],percent="percent","wrap" )
    }else{
      create_processes_frequency(expt_freq[(expt_freq$cat1 =="Processes") & !is.na(expt_freq$cat1),],percent="percent" )
    }
  })

  

  output$processes_fluxes_plot <- renderPlotly({
    expt_freq<-funcFreq(data_expt_select(),categoriesdf)
    if ("Wrap by type of wood" %in% input$wrap_type_wood_fluxes) {
      plotlyProcessesFlux<-create_processes_versus_flux_size(refCProcessMean,expt_freq ,"Set1",wood_type_names,"wrap")
    }else{
      plotlyProcessesFlux<-create_processes_versus_flux_size(refCProcessMean,expt_freq ,"Set1",wood_type_names)
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



  ## Results section 

  res <- reactiveValues(data_expt = data_expt, data_bibliom=data_bibliom,filterResults="no")

  observeEvent(input$submitResults, {
    res$data_expt<- data_expt_select()
    res$data_expt_approach<- data_expt_approach_select()
    res$data_expt_assumption<- data_expt_assumption_select()
    res$data_bibliom<-data_bibliom_select()
    res$data_study<-data_study_select()
    res$filterResults<-"filter"
  })

  observeEvent(c(input$resetResults,input$ignore), {
    res$data_expt <- data_expt
    res$data_expt_approach <- data_expt_approach
    res$data_expt_assumption<- data_expt_assumption
    res$data_bibliom <- data_bibliom
    res$data_study <- data_study
    res$filterResults<-"no"

  })

  observeEvent(input$submitResultsAny, {
    res$data_expt<- data_expt_select()
    res$data_expt_approach<- data_expt_approach_select()
    res$data_expt_assumption<- data_expt_assumption_select()
    res$data_bibliom<-data_bibliom_select()
    res$filterResults<-"filter"
  })
  
  observeEvent(c(input$resetResultsAny,input$ignore), {
    res$data_expt <- data_expt
    res$data_expt_approach <- data_expt_approach
    res$data_expt_assumption<- data_expt_assumption
    res$data_bibliom <- data_bibliom
    res$filterResults<-"no"
    
  })
  output$dendrogram<-renderPlot({
    create_dendrogram(res$data_expt_assumption,
                      k=3,
                      c( 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic','off_product_biogenic'))

  })
  
  output$dendrogramTopic<-renderPlot({
    create_dendrogram(res$data_expt_assumption,
                      k=6,
                      c( 'modelAssumption', 'scaleAgg','singleProduct','time_horizon','driver1'))    
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

  output$assumptionC<-renderPlot({
    
    if (res$filterResults=="filter") {

      plotData.tmp<-plotDataFunc(res$data_expt_assumption, 
                                         "outliers_in",
                                         "modelAssumption",
                                         "omitSplit2",
                                         "omitSplit3")
      
      forestPlotData.tmp<-forestPlotDataFunc(plotData.assumptionC,"modelAssumption","omitSplit2")
      
    }else{
      plotData.tmp<-plotData.assumptionC
      forestPlotData.tmp<-forestPlotData.assumptionC
      
    }
    create_forest_plot(plotData.tmp,forestPlotData.tmp,FALSE)
  })
  

  output$modelComponentsC<-renderPlotly({
    if (res$filterResults=="filter") {

    tTestPairsSignifAggVarMelt.tmp<-modelComponentsC(data_expt_assumption_select(),c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
                                                 , "",("PaperID"))
    }else{
      tTestPairsSignifAggVarMelt.tmp<-tTestPairsSignifAggVarMelt
    }

    plotModelComponentsC(tTestPairsSignifAggVarMelt.tmp)
  })



  


  output$driverC<-renderPlot({
      plotData.driverC.tmp<-plotData.driverC.AllCPools
      forestPlotData.driverC.tmp<-forestPlotData.driverC.AllCPools
    create_forest_plot(plotData.driverC.tmp,forestPlotData.driverC.tmp,TRUE)
  })



  output$knowledgeDyn<-renderPlot({

    if (res$filterResults=="filter") {
      #forestPlotData.approachC.dyn.tmp<-knowDynamicsData(res$data_expt_approach)
      forestPlotData.assumptionC.dyn.tmp <-knowDynamicsData(res$data_expt_assumption,"modelAssumption")
    }else{
      #forestPlotData.approachC.dyn.tmp<-forestPlotData.approachC.dyn
      forestPlotData.assumptionC.dyn.tmp<-forestPlotData.assumptionC.dyn
    }
    #create_knowDynamicsPlot(forestPlotData.approachC.dyn.tmp)
    create_forest_plot_dynamics(forestPlotData.assumptionC.dyn.tmp)
    
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
    paste("Publications processed:", nrow(data_study_select()), "/",nrow(data_study), sep = " ")

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
 
  output$summaryExptBoxResultsAny <- renderInfoBox({
    infoBox(
      "Records", paste0(nrow(res$data_expt),"/",nrow(data_expt)), icon = icon("list"),
      color = "maroon"
    )
  })
  output$summaryStudyBoxResultsAny <- renderInfoBox({
    infoBox(
      "Publications", paste0(nrow(res$data_bibliom), "/",nrow(data_bibliom)), icon = icon("list"),
      color = "orange"
    )
  })

  output$downloadProtocol <- downloadHandler(
    filename = "Wood-carbon_full_protocol.pdf",
    content = function(file) {
      file.copy("www/Wood-carbon_full_protocol.pdf", file)
    }
  )
  
 data_expt_select_dl<- reactive({subset(data_expt_select(),
                                         select=categoriesdf[categoriesdf$cat1 %in% c( "Paper description","Driver","DataCorrection","Processes"),'names']) })
  
 
  
  output$downloadDatabase <- downloadHandler(
    filename = "wood-carbon_Valade_2025.csv",
    content = function(file) {
      write.csv(data_expt_select_dl(),file,row.names=FALSE)
    }
  )
  
  
}

#pour commenter ou décommenter plusieurs lignes sélectionnées
# ctrl+shift+c

