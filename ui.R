
sidebar <- dashboardSidebar(
  width = 150,
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Home", icon=icon("house"),tabName="home", selected=TRUE),
              menuItem("Principles", icon=icon("book"),
                       menuSubItem("Context", tabName = "context", icon = icon("angle-right")),
                       menuSubItem("Carbon balance", tabName = "carbonBalance", icon = icon("angle-right")),
                       menuSubItem("Methods", tabName = "methods", icon = icon("angle-right"))
                       
              ),
              menuItem("Database", icon=icon("table"), tabName="database" ),
              menuItem("Results", icon=icon("newspaper"), tabName="resultsTab" ,
                       menuSubItem("All records", tabName = "AnyResults", icon = icon("angle-right")),
                       menuSubItem("Complete C cycle records only", tabName = "AllCPoolsResults", icon = icon("angle-right"))
                       
                       )#,
             
  ),
  hr()

)
body <- dashboardBody(
  withMathJax(),
  use_theme(my_theme),
  tabItems(
    tabItem(tabName = "home",
            fluidPage(
              includeMarkdown("./TextContent/Home.md")
            )
    ),
    tabItem(tabName = "context",
            fluidPage(
              includeMarkdown('./TextContent/PrinciplesContext.md'
              )
            )
    ),
    tabItem(tabName = "carbonBalance",
              fluidPage(
                includeMarkdown( './TextContent/PrinciplesCarbonBalance.md'
                )
              )
    ),
    tabItem(tabName = "methods",
            fluidPage(              
              downloadButton("downloadProtocol", "Download the protocol",style = "float: right; "),
              tabBox(width = NULL,
                     tabPanel(
                       
                       h5("Study selection"),
                       includeMarkdown( './TextContent/PrinciplesMethodsStudies.md')
                                        
                     ),
                     tabPanel(
                       
                       h5("Data extraction"),
                       includeMarkdown( './TextContent/PrinciplesMethodsPairs.md')
                       
                     )
              )
              
            )
    ),
    tabItem(tabName = "database",
            fluidPage(
              fluidRow(
                column(width = 4,
                       box(width = NULL,
                           actionButton("reset", "Reset filters",style='padding:10px; font-size:100%;  border-color: #00a98e'),
                           br(),
                       tabBox(width = NULL,
                              tabPanel(

                                h5("Perimeter"),
                                fluidRow(
                                  column(width=12,

                                         checkboxGroupInput(#beginning checkboxGroupInput
                                           inputId="select_scale",
                                           label="Spatial scale",
                                           choices= c("Global"="w","Regional" = "reg", "Local" = "loc"),
                                           selected = c("Global"="w","Regional" = "reg", "Local" = "loc"),
                                         ),#end  checkboxGroupInput
                                         selectInput(#beginning selectInput
                                           inputId = "select_countries",
                                           label = "Countries or regions",
                                           choices = sort(countriesList),
                                           multiple = TRUE,
                                           selected = sort(countriesList)
                                         ),#end selectInput
                                         checkboxGroupInput( #beginning checkboxGroupInput
                                           inputId="select_single_product",
                                           label="Wood product type",
                                           #  choices = products,
                                           choiceNames=str_to_title(productsLabels),
                                           choiceValues=productsList,
                                           selected = productsList,
                                         ), #end  checkboxGroupInput
                                         bsPopover(id="select_single_product",
                                                   "Wood type","Select which wood types the analyzed studies focus on. Mixed product means that more than one wood type is considered.",
                                                   options = list(container = "body"),
                                                   trigger="hover"),

                                         selectInput(#beginning selectInput
                                           inputId = "select_time_horizon",
                                           label = "Time scale (years)",
                                           choices = timeHorizonList,
                                           multiple = TRUE,
                                           selected = timeHorizonList
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
                                        choices = processesList,
                                      #choices = processesLabels,
                                      multiple=TRUE,
                                      selected = NULL
                                    ), #end  checkboxGroupInput
                                    selectInput(#beginning checkboxGroupInput
                                      "select_dynamics",#inputId
                                      "Only show records that account for dynamics",#label
                                      choices=dynamicsList,
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
                                      choices=driver1CatList,
                                      multiple = TRUE,
                                      selected = driver1CatList
                                    ),#end selectInput
                                    selectInput(#beginning selectInput
                                      "select_driver1",#inputId
                                      "Mobilization strategy",#label
                                      choices=driver1List,#choices
                                      multiple = TRUE, #multiple choice  = TRUE
                                      selected = driver1List
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
                                      round=-1,
                                      step=0.5,
                                      min = minValSub,
                                      max = maxValSub,
                                      value = c(minValSub, maxValSub),
                                    )#end sliderInput

                                  )#end column
                                )#end fluidrow

                              )#end tabpanel

                       )#end tabbox
                       )#end box
                       #,
                       #DTOutput('filtered_db_table')

                ),#end column


                column(
                  width = 8,
                  fluidRow(
                    # Clicking this will increment the progress amount
                    box(width = 4,
                        actionButton("submitExp", "Submit filters",style='padding:10px; font-size:100%;  background-color: #e27a3a'),
                        actionButton("ignoreExp", "Ignore filters",style='padding:10px; font-size:100%;  background-color: #f3eada')
                    ),
                    infoBoxOutput("summaryExptBox"),
                    infoBoxOutput("summaryStudyBox"),
                   # downloadButton("downloadDatabase", "Download data",style = "float: right; "),
                    

                  ),


                  tabBox(
                    # Standard TabBox
                    #                  title='Plot',
                    id = "tabset1", height = "700px",width=12,
                    tabPanel("Years",
                             div(
                               h3("Years of publication of included studies", class = "description"),
                               plotlyOutput("barplotYear"), #This will display the map
                             )
                    ),
                    tabPanel("Locations",
                             div(
                               h3("Number of included studies with explicit reference to a country location", class = "description"),
                               addSpinner(plotlyOutput("countryData", height = "500px"), spin = "circle", color = "green"), #This will display the plot
                               selectInput(
                                 "countryRanking",
                                 "Select options below:",
                                 list("Forest Area" = "Forest.area..1000.ha.", 
                                      "Roundwood production" = "Roundwood (m3)",
                                      "Part of R&D in GDP" = "GDP_RD",
                                      "Forest area ratio"="Forest area ratio (%)"
                                      )
                               )
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
                                 inputId="wrap_type_wood_fluxes",
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
                               checkboxGroupInput(#beginning checkboxGroupInput
                                 inputId="wrap_type_wood_drivers",
                                 label = NULL,
                                 choices= "Wrap by type of wood"
                                 #                       selected = "Wrap by type of wood"
                               ),#end  checkboxGroupInput
                               addSpinner(plotlyOutput("driver_plot", height = "500px"), spin = "circle", color = "#377EB8"),

                             )
                    ),  #end tabpanel


                  ),#end tabbox
                  box( id = "boxTable", width=12,#height = "500px",
                       DTOutput('filtered_db_table')
                       )

                ),#end column

              )#end fluirow

              # infoBox("Processed", 10 * 2, icon = icon("list"), fill = TRUE),

            )#end fluidpage
    ),


    tabItem(tabName = "AnyResults",
            #tabItem(tabName = "resultsTab",
            fluidPage(
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4,
                    actionButton("submitResultsAny", "Apply filters",style='padding:10px; font-size:100%;  background-color: #e27a3a'),
                    actionButton("resetResultsAny", "Ignore filters",style='padding:10px; font-size:100%;  background-color: #f3eada')),
                infoBoxOutput("summaryExptBoxResultsAny"),
                infoBoxOutput("summaryStudyBoxResultsAny")
              ),
              
              tabBox(
                # Standard TabBox
                #                  title='Plot',
                id = "tabset1", height = "800px",width=12,
                tabPanel("Modeling assumptions",
                         div(
                           addSpinner(plotOutput("dendrogram", height = "500px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_dendrogram.md')
                           
                         )
                ),
                tabPanel("Assumptions applications",
                         div(
                           addSpinner(plotOutput("dendrogramTopic", height = "500px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_dendrogram.md')
                           
                         )
                ),
                tabPanel("Carbon balance",
                         div(
                           addSpinner(plotOutput("assumptionC",height="500px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_approachC.md')
                           
                         ),#end div
                ),
              
                
                
                tabPanel("Knowledge dynamics",
                         div(
                           addSpinner(plotOutput("knowledgeDyn", height = "600px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_driverC.md')
                           
                         )  #end div
                ), #end tabpanel
                tabPanel("Model components",
                         div(
                           addSpinner(plotlyOutput("modelComponentsC", height = "500px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_components.md')
                           
                           
                         )  #end div
                ),# end tabpanel
                
               
              )#end tabbox
            )#end fluidpage
    ),    #end tabItem


    tabItem(tabName = "AllCPoolsResults",
    #tabItem(tabName = "resultsTab",
            fluidPage(
              # fluidRow(
              #   # Clicking this will increment the progress amount
              #   box(width = 4,
              #       actionButton("submitResults", "Apply filters",style='padding:10px; font-size:100%;  background-color: #e27a3a'),
              #       actionButton("resetResults", "Ignore filters",style='padding:10px; font-size:100%;  background-color: #f3eada')),
              #   infoBoxOutput("summaryExptBoxResults"),
              #   infoBoxOutput("summaryStudyBoxResults")
              # ),

              tabBox(
                # Standard TabBox
                #                  title='Plot',
                id = "tabset1", height = "800px",width=12,
                tabPanel("Mobilization strategies",
                         div(
                           
                           addSpinner(plotOutput("driverC", height = "600px"), spin = "circle", color = "#377EB8"),
                           includeMarkdown( './TextContent/leg_driverC.md')
                           
                         )  #end div
                ), #end tabpanel
                
            )#end tabbox
    )#end fluidpage
  )#,    #end tabItem




)
)

page <- dashboardPage(
  dashboardHeader(title = "Wood & carbon"),
  sidebar,
  body
)

dashboardAddFooter(page, legalNotice(2025, "Aude Valade"))

