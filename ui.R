library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(slider)
sidebar <- dashboardSidebar(
  width = 150,
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Home", tabName="home", selected=TRUE),
              menuItem("Principles", icon=icon("table"),
                       menuSubItem("Context", tabName = "context", icon = icon("angle-right")),
                       menuSubItem("Methods", tabName = "methods", icon = icon("angle-right"))
              ),
              menuItem("Database", tabName="database" ),
              menuItem("Results", tabName="resultsTab" )#,
              # menuItem("Test",  #icon = icon("file-text-o"),
              #          menuSubItem("Approaches", tabName = "approaches", icon = icon("angle-right")),
              #          menuSubItem("Substitution", tabName = "model", icon = icon("angle-right")),
              #          menuSubItem("Models", tabName = "components", icon = icon("angle-right")),
              #          menuSubItem("Drivers", tabName = "drivers", icon = icon("angle-right"))
              # )
  ),
  hr()
  
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            fluidPage(
              includeMarkdown("Home.md")
            )
    ),
    tabItem(tabName = "context",
            fluidPage(
              includeMarkdown('./PrinciplesContext.md'
              )
            )
    ),
    tabItem(tabName = "methods",
            fluidPage(
              includeMarkdown( './PrinciplesMethods.md'
              )
            )
    ),
    tabItem(tabName = "database",
            fluidPage(
              fluidRow(
                column(width = 4, 
                       tabBox(width = NULL,
                              tabPanel(
                                
                                h5("Perimeter"),
                                fluidRow(
                                  column(width=12,
                                         
                                         checkboxGroupInput(#beginning checkboxGroupInput
                                           inputId="select_scale",
                                           label="Spatial scale",
                                           choices= c("world"="w","regional" = "reg", "local" = "loc"),
                                           selected = c("world"="w","regional" = "reg", "local" = "loc"),
                                         ),#end  checkboxGroupInput 
                                         selectInput(#beginning selectInput
                                           inputId = "select_countries",
                                           label = "Country", 
                                           choices = countries, 
                                           multiple = TRUE, 
                                           selected = countries
                                         ),#end selectInput   
                                         checkboxGroupInput( #beginning checkboxGroupInput
                                           inputId="select_single_product",
                                           label="Wood product type",
                                           #  choices = products,
                                           choiceNames=productsLabels,
                                           choiceValues=products,
                                           selected = products,  
                                         ), #end  checkboxGroupInput 
                                         bsPopover(id="select_single_product", 
                                                   "Wood type","Select which wood types the analyzed studies focus on. Mixed product means that more than one wood type is considered.",
                                                   options = list(container = "body"),
                                                   trigger="hover"),
                                         
                                         selectInput(#beginning selectInput
                                           inputId = "select_time_horizon",
                                           label = "Time scale",
                                           choices = timeHorizon,
                                           multiple = TRUE, 
                                           selected = timeHorizon
                                         )#end  selectInput
                                  )#end column
                                )#end fluidrow
                              ),#end tabpanel
                              tabPanel(
                                h5("Model"),
                                fluidRow(
                                  column(
                                    width = 12,
                                    # selectInput(#beginning selectInput
                                    #   "select_processes", #inputId
                                    #   "Carbon processes included in the model", #label
                                    #   choices = processes,
                                    #   multiple = TRUE,
                                    #   selected = NULL
                                    # ),#end  selectInput
                                    selectInput( #beginning checkboxGroupInput
                                      "select_processes",
                                      "Only show records that account for processes",
                                      #  choices = products,
                                      choices = processes,
                                      multiple=TRUE,
                                      selected = NULL 
                                    ), #end  checkboxGroupInput 
                                    selectInput(#beginning checkboxGroupInput
                                      "select_dynamics",#inputId
                                      "Only show records that account for dynamics",#label
                                      choices=dynamics,
                                      multiple=TRUE,
                                      selected = NULL
                                      #                                    selected = dynamics
                                    )#end checkboxgroupInput
                                  )#end column
                                )#end fluidrow
                              ),#end tabpanel
                              tabPanel(
                                h5("Mobilization strategy"),
                                fluidRow(
                                  column(
                                    width = 12,
                                    selectInput(#beginning selectInput
                                      "select_driver1Cat",
                                      "Mobilization strategy category",
                                      driver1Cat,
                                      multiple = TRUE,
                                      selected = driver1Cat
                                    ),#end selectInput 
                                    selectInput(#beginning selectInput
                                      "select_driver1",#inputId
                                      "Mobilization strategy",#label
                                      driver1,#choices
                                      multiple = TRUE, #multiple choice  = TRUE
                                      selected = driver1
                                    )
                                  )
                                )
                              ),
                              tabPanel(
                                h5("C potential of wood"),
                                fluidRow(
                                  column(
                                    width = 12,
                                    sliderInput(#beginning sliderInput
                                      inputId = "select_substitution",
                                      label = "Substitution",
                                      min = minValSub,
                                      max = maxValSub,
                                      value = c(minValSub, maxValSub),
                                    ),#end sliderInput
                                    checkboxGroupInput(#beginning checkboxGroupInput
                                      inputId="select_NA_substitution",
                                      label=NULL,
                                      choices= "NA",
                                      selected = "NA",
                                    )#end checkboxgroupInput
                                  )#end column
                                )#end fluidrow 
                                
                              )#end tabpanel
                              
                       )#end tabbox
                       ,
                       DTOutput('filtered_db_table')
                       
                ),#end column
                
                
                column(
                  width = 8,
                  fluidRow(
                    # Clicking this will increment the progress amount
                    box(width = 4, 
                        actionButton("submit", "Submit filters",style='padding:10px; font-size:100%'),
                        actionButton("reset", "Reset filters",style='padding:10px; font-size:100%;  background-color: #337ab7')),
                    infoBoxOutput("summaryExptBox"),
                    infoBoxOutput("summaryStudyBox")
                    
                  ),
                  
                  
                  tabBox(
                    # Standard TabBox
                    #                  title='Plot',
                    id = "tabset1", height = "500px",width=12,
                    tabPanel("Years", 
                             div(  
                               h3("Years of publication of included studies", class = "description"),
                               plotlyOutput("barplotYear"), #This will display the map 
                             )
                    ),
                    tabPanel("Locations", 
                             div(  
                               h3("Number of included studies with explicit reference to a country location", class = "description"),
                               addSpinner(plotlyOutput("countryData", height = "500px"), spin = "circle", color = "#377EB8") #This will display the plot 
                               
                             ),#end div                  
                    ),
                    tabPanel("Model", 
                             div(  
                               h3("Number of studies considering each process"),
                               checkboxGroupInput(#beginning checkboxGroupInput
                                 inputId="wrap_type_wood_processes",
                                 label = NULL,
                                 choices= "Wrap by type of wood"
                                 #                       selected = "Wrap by type of wood"
                               ),#end  checkboxGroupInput
                               addSpinner(plotlyOutput("processes_plot",height="500px"), spin = "circle", color = "#377EB8")
                             )                 
                    ),
                    tabPanel("Emissions", 
                             div(  
                               h3("Number of studies considering each process"),
                               checkboxGroupInput(#beginning checkboxGroupInput
                                 inputId="wrap_type_wood_processes",
                                 label = NULL,
                                 choices= "Wrap by type of wood"
                                 #                       selected = "Wrap by type of wood"
                               ),#end  checkboxGroupInput
                               addSpinner(plotlyOutput("processes_fluxes_plot", height = "500px"), spin = "circle", color = "#377EB8"),
                             )               
                    ),
                    tabPanel("Mobilization strategy", 
                             div(  
                               h3("Number of experiments considering each strategy of increased wood use"),
                               addSpinner(plotlyOutput("driver_plot", height = "500px"), spin = "circle", color = "#377EB8"),
                               checkboxGroupInput(#beginning checkboxGroupInput
                                 inputId="wrap_type_wood_drivers",
                                 label = NULL,
                                 choices= "Wrap by type of wood"
                                 #                       selected = "Wrap by type of wood"
                               )#end  checkboxGroupInput
                             )                  ),  #end tabpanel      
                    
                  )#end tabbox
                  
                )#end column
              )#end fluirow
              
              # infoBox("Processed", 10 * 2, icon = icon("list"), fill = TRUE),
              
            )#end fluidpage
    ),
    
    
    
    
    
    tabItem(tabName = "resultsTab",
            fluidPage(
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4,
                    actionButton("submitResults", "Submit filters",style='padding:10px; font-size:100%'),
                    actionButton("resetResults", "Reset filters",style='padding:10px; font-size:100%;  background-color: #337ab7')),
                infoBoxOutput("summaryExptBoxResults"),
                infoBoxOutput("summaryStudyBoxResults")
              ),
              
              tabBox(
                # Standard TabBox
                #                  title='Plot',
                id = "tabset1", height = "800px",width=12,
                tabPanel("Modeling approaches", 
                         div(  
                           addSpinner(plotOutput("dendrogram", height = "500px"), spin = "circle", color = "#377EB8")
                           
                         )
                ),
                tabPanel("Carbon balance", 
                         div(  
                           addSpinner(plotOutput("approachC",height="500px"), spin = "circle", color = "#377EB8")
                           
                         ),#end div                  
                ),
                tabPanel("Model components", 
                         div(  
                           addSpinner(plotlyOutput("modelComponentsC", height = "500px"), spin = "circle", color = "#377EB8")
                           
                         )  #end div                
                ),
                tabPanel("Mobilization strategies", 
                         div(  
                           
                           addSpinner(plotOutput("driverC", height = "500px"), spin = "circle", color = "#377EB8")
                         )  #end div                
              ), #end tabpanel      
            )#end tabbox
    )#end fluidpage
  )#,    #end tabItem
  
  
  
  
)
)

dashboardPage(
  dashboardHeader(title = "Wood & carbon"),
  sidebar,
  body
)


