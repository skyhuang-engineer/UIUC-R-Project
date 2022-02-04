

sidebar <- dashboardSidebar(
  width = 270,
  sidebarMenu(
    
    menuItem("Graphs & Curves", tabName = "Graphs", icon = icon("bar-chart")),

    menuItem("World Chess data", icon = icon("globe"), tabName = "World"),
  
    menuItem("Individual player chart", tabName = "Individual", icon = icon("calendar")),
    
    menuItem("FIDE arbiters", tabName = "Arbiters", icon = icon("group")),
    
    br(),
    
    box( width = 14,
      title = "Filters"
      ,
      
      selectInput("dataset",
                  strong(h4("Time control", style = "color:red")), 
                  choices = c("Standard", "Rapid", "Blitz"),
                  selected = "Standard")
      ,
      
      selectInput("Activity",
                  strong(h4("Activity", style = "color:red")),
                  choices = c("Active", "Inactive"),
                  multiple = TRUE,
                  selected = c("Active")
      )
      ,
      
      selectInput("dataset_standard",
                  strong(h4("Sex", style = "color:red")), 
                  choices = c("Male", "Female"),
                  multiple = TRUE,
                  selected = c("Male", "Female") 
      )
      
      ,
      
      
      selectInput("country",
                  strong(h4("Country", style = "color:red")), 
                  choices = Standard_countries[,1],
                  multiple = TRUE,
                  selected = "All Countries")
      
      ,
      sliderInput("Rating", 
                  strong(h4("Rating", style= "color:red")),
                  min = 1000, max = 3000,
                  value = c(1000, 3000)
      )
      ,
      
      sliderInput("Age", 
                  strong(h4("Age", style= "color:red")),
                  min = 0, max = 100,
                  value = c(0, 100)
      )
      ,

      selectInput("Kfactor",strong(h4("K-factor", style= "color:red")),
                  choices = c(10,20, 40),
                  multiple = TRUE,
                  selected = c(10, 20, 40)
      )

    )
  )
)

body <- dashboardBody(
    tabItems(
    tabItem(tabName = "Graphs",
            tabBox(
              title = "Density curve and Histograms",
              id = "tabset1", height = "300px", width = "500px",
                                  
              tabPanel("Density",
                       selectInput("Rage",
                                   strong(h4("Type of Distribution", style = "color:red")),
                                   choices = c("Age"= "Age", "Rating" = "Rating"),
                                   selected = "Rating"),
                       plotOutput("StandardPlot", width = "100%", height = "500px"))
                                            ,
              tabPanel("Histogram", sliderInput("Sbins",
                                                strong(h4("# of breaks", style= "color:red")),
                                                min = 1,
                                                max = 100,
                                                value = 46), 
                                    selectInput("Rage1",
                                                 strong(h4("Type of Distribution", style = "color:red")),
                                                 choices = c("Age"= "Age", "Rating" = "Rating"),
                                                 selected = "Rating"),
                                    plotOutput("StandardPlot1", width = "100%", height = "500px"))
                                            ,
              tabPanel("Histogram & Density", sliderInput("Sbins1",
                                                          strong(h4("# of breaks", style= "color:red")),
                                                          min = 1,
                                                          max = 100,
                                                          value = 46),
                                               selectInput("Rage2",
                                                         strong(h4("Type of Distribution", style = "color:red")),
                                                         choices = c("Age"= "Age", "Rating" = "Rating"),
                                                         selected = "Rating"),
                                              plotOutput("StandardPlot2", width = "100%", height = "500px"))
            )
    ),
    
    tabItem(tabName = "World",
            tabPanel("World Data", 
              tabBox( 
              title = "World Data",
              id = "tabset2", height = "800px", width = "500px",
                                               
                                   tabPanel("Barplot", plotOutput("StandardPlot4", width = "100%", height = "500px"))
                                     ,
                                   tabPanel("Players counts by Region",
                                            selectInput("Region" , 
                                                        label = "Select a region",
                                                                 choices = unique(region_counts$region),
                                                                 selected = "Eastern Asia",
                                                                 multiple = TRUE ),
                                            verbatimTextOutput("legendDivID1"),
                                            
                                            dygraphOutput("dygraphY"))

              ))
    )
    ,
    tabItem(tabName = "Individual",
            tabPanel("All IM/GMs ratings over time", 
                     tabBox(
                       title = "Time series plot",
                       id = "tabset3", 
                       height = "800px", 
                       width = "500px",
                       tabPanel("Time series", 
                                
                                box(dygraphOutput("dygraph", width = "100%", height = "400px"), width = 12),
                                box(width = 4, verbatimTextOutput("legendDivID"), title = "Legend", collapsible = TRUE),
                                box( side= "right", selectInput("Name", strong(h4("Choose a player", style= "color:red")),
                                                 choices = IMS_GMS[,2],
                                                 selected = "Carlsen, Magnus",
                                                 multiple = TRUE
                                                 )))
                      ))
    ),
    
    tabItem(tabName = "Arbiters",
            tabPanel("Arbiters", 
                     tabBox("Arbiters by country",
                            id = "tabset4",
                            height = "800px", 
                            width = "500px",
                            tabPanel("Arbiters by country", 
                                     selectInput("Federation",
                                                 strong(h4("Federation", style = "color:red")),
                                                 choices = sort(as.character(unique(Other_titles$Fed))),
                                                 selected = c("USA", "RUS"),
                                                 multiple = TRUE),
                                     plotOutput("bargraph"))

                     )))
  )
)

# Put them together into a dashboardPage
dashboardPage( skin = "green",
  dashboardHeader(title = "FIDE Chess Data", titleWidth =  270),
  sidebar,
  body
)
