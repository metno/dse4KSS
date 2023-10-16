# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shiny)
library(shinydashboard)
library(leaflet)


header <- dashboardHeader(
  title = "The Nordic region climate atlas",
  titleWidth = '600px'
)


introbox <- box(
  title = HTML("<font size=+1.5 color='black'><b>About the app</b></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = TRUE,
  collapsed = FALSE,
  htmlOutput("IntroText"),
  br(),
  box(
    title = HTML("<font size=+0 color='black'><i>Details</i></font>"),
    width = '100%' ,
    status = 'primary',
    collapsible = TRUE,
    collapsed = TRUE,
    htmlOutput("HowtoText"),
  ),
  box(
    title = HTML("<font size=+0 color='black'><i>Data and method</i></font>"),
    width = '100%' ,
    status = 'primary',
    collapsible = TRUE,
    collapsed = TRUE,
    htmlOutput("DataText"),
  )
)



Abox <- box(
  title = HTML("<font size=+0.5 color='black'><b>Ensemble A</b></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = FALSE,
  htmlOutput("InfoA")
)

Bbox <- box(
  title = HTML("<font size=+0.5 color='black'><b>Ensemble B</b></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = FALSE,
  htmlOutput("InfoB")
)

timeseries <- box(width=NULL, title=HTML("<font size=+1.5 color='black'><b>Time series</b></font>"),
                  collapsible = TRUE, collapsed=FALSE,
                  fluidRow(
                    column(8,
                           plotlyOutput("timeseries", width = "100%", height = "60%")
                    ),
                    column(4,
                           leafletOutput("map"),#, width="20vw"),
                           absolutePanel(bottom = 30, left = 20, draggable=TRUE,
                                         selectInput("location", width = "120px",
                                                     label = "Location",
                                                     choices = locs[[var0]]$label,#locs[[reg0]][[var0]]$label,
                                                     selected = locs[[var0]]$label[[1]])#locs[[reg0]][[var0]]$label[[1]])
                           )
                    )
                  ),
                  br(),
                  br(),
                  fluidRow(
                    box(
                      title="Ensemble A",
                      width=6,
                      htmlOutput("InfoA")),
                    box(
                      title="Ensemble B",
                      width=6,
                      htmlOutput("InfoB"))
                  ),
                  br(),
                  box(
                    title=HTML("<font size=+0>Plot settings</font>"),
                    width = '100%' ,
                    status = 'primary',
                    collapsible = TRUE,
                    collapsed = TRUE,
                    fluidRow(
                      column(6, offset=0.5,
                             sliderInput("tsrangeA", label="Range of y-axis in time series plot",
                                         min=-30, max=50, step = 1, value=c(-5,20))
                      )
                    )
                  )
                  
)


maps <- box(width=NULL, title=HTML("<font size=+1.5 color='black'><b>Maps</b></font>"),
            collapsible = TRUE, collapsed=FALSE,
            fluidRow(
              box(
                title="Ensemble A",
                width=6,
                htmlOutput("InfoA2")),
              box(
                title="Ensemble B",
                width=6,
                htmlOutput("InfoB2"))
            ),
            fluidRow(
              column(6,
                     #h5("Ensemble A"),
                     #br(),
                     plotOutput("mapA", width = "100%", height = "80%"),
                     downloadButton(label = "save",
                                    outputId = "savemapA"),
                     br(),
                     br(),
                     selectInput("funA",
                                 label = "Time aggregation",
                                 choices=c('mean','trend','max','min','sd'),
                                 selected='trend'),
                     selectInput("datesA",
                                 label="Years",
                                 choices=names(datelist),
                                 selected=names(datelist)[[1]]),
                     br(),
                     br(),
              ),
              column(6, 
                     #h5("Ensemble B"),
                     #br(),
                     plotOutput("mapB", width = "100%", height = "80%"),
                     downloadButton(label = "save",
                                    outputId = "savemapB"),
                     br(),
                     br(),
                     selectInput("funB",
                                 label = "Time aggregation",
                                 choices=c('mean','trend','max','min','sd'),
                                 selected='trend'),
                     selectInput("datesB",
                                 label="Years",
                                 choices=names(datelist),
                                 selected=names(datelist)[[1]]),
                     br(),
                     br()
              )
            ),
            box(
              title=HTML("<font size=+0>Plot settings</font>"),
              width = '100%' ,
              status = 'primary',
              collapsible = TRUE,
              collapsed = TRUE,
              fluidRow(
                column(12, offset=0.5,
                       checkboxInput("landmask",
                                     label = "mask ocean in maps with regional climate model (RCM) results",
                                     value = TRUE),
                       checkboxInput("robustness_map",
                                     label = "show trend robustness (same sign in 90% of ensemble members) in maps",
                                     value = TRUE)
                )
              ),
              fluidRow(
                column(6, offset=0.5,
                       sliderInput("valrangeA", label="Range of colorscale in map A",
                                   min=-30, max=50, step = 1, value=c(-5,20))
                ),
                column(6, 
                       sliderInput("valrangeB", label="Range of colorscale in map B",
                                   min=-30, max=50, step = 1, value=c(-5,20))
                )
              )
            )
)


body <- dashboardBody(
  #fluidRow(
  #  column(6, Abox),
  #  column(6, Bbox)
  #),
  fluidRow(
    tabsetPanel(
      tabPanel("Time series",
               timeseries),
      tabPanel("Maps", 
               maps)
    )
  ),
  fluidRow(
    column(12,
           introbox
    )
  )
)

sideboard <- dashboardSidebar(
  sidebarMenu(
    menuItem("\n Season and variable", tabName = "dashboard", 
             icon = icon("cloud"), tabname="settings",
             selectInput("varA",
                         label = "Variable",
                         choices = as.vector(sapply(unique(cats$var), varname)),
                         selected = varname(var0)),
             selectInput("seasA",
                         label = "Season",
                         choices = as.vector(sapply(unique(cats$it), seasonname)),
                         selected = seasonname(seas0))
    ),
    menuItem(" Ensemble A", icon = icon("list"), tabName = "A", startExpanded = FALSE,
             br(),
             selectInput("srcA",
                         label="Data source",
                         choices = sourcelist,
                         selected = sourcelist[[1]]),
             selectInput("sceA",
                         label = "Scenario",
                         choices = scenarios,
                         selected = scenarioname(sce0)),
             br(),
             actionButton("gcmallA",
                          label = "All simulations",
                          width = '150px'
             ),
             actionButton("gcmoneA",
                          label = "One of each GCM",
                          width = '150px'
             ),
             actionButton("gcmdeselectA",
                          label = "Deselect all",
                          width = '150px'
             ),
             actionButton("gcmsameA",
                          label = "Same as ensemble B",
                          width = '150px'
             ),
             br(),
             checkboxGroupInput("gcmsA",
                                label = "Climate models",
                                choices = gcmnames[[var0]][[sce0]],
                                selected = gcmnames[[var0]][[sce0]],
                                inline=TRUE,
                                width='100%'
             )
    ),
    menuItem("\n Ensemble B", icon = icon("list"), tabname="B", startExpanded = FALSE,
             br(),
             selectInput("srcB",
                         label="Data source",
                         choices = sourcelist,
                         selected = sourcelist[[2]]),
             selectInput("sceB",
                         label = "Scenario",
                         choices = scenarios,
                         selected = scenarioname(sce0)),
             br(),
             actionButton("gcmallB",
                          label = "All simulations",
                          width = '150px'
             ),
             actionButton("gcmoneB",
                          label = "One of each GCM",
                          width = '150px'
             ),
             actionButton("gcmdeselectB",
                          label = "Deselect all",
                          width = '150px'
             ),
             actionButton("gcmsameB",
                          label = "Same as ensemble A",
                          width = '150px'
             ),
             br(),
             checkboxGroupInput("gcmsB",
                                label = "Climate models",
                                choices = gcmnames[[var0]][[sce0]],
                                selected = gcmnames[[var0]][[sce0]],
                                inline=TRUE,
                                width='100%')
    )
  )
)



dashboardPage(
  header,
  sideboard,
  body
)

