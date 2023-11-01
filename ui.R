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
  title = HTML("<font size=+1.5 color='black'><b>About the Nordic climate atlas</b></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = FALSE,
  collapsed = FALSE,
  htmlOutput("IntroText"),
  br(),
  box(
    title = HTML("<font size=+0 color='black'><i>How to use the app</i></font>"),
    width = '100%' ,
    status = 'primary',
    collapsible = FALSE,
    collapsed = FALSE,
    htmlOutput("HowtoText"),
  ),
  box(
    title = HTML("<font size=+0 color='black'><i>Data and method</i></font>"),
    width = '100%' ,
    status = 'primary',
    collapsible = FALSE,
    collapsed = FALSE,
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
                  )
)


maps <- box(width=NULL, title=HTML("<font size=+1.5 color='black'><b>Maps</b></font>"),
            collapsible = TRUE, collapsed=FALSE,
            fluidRow(
              column(6,
                     br(),
                     plotOutput("mapA", width = "100%", height = "80%"),
                     downloadButton(label = "save",
                                    outputId = "savemapA"),
                     br()
              ),
              column(6, 
                     br(),
                     plotOutput("mapB", width = "100%", height = "80%"),
                     downloadButton(label = "save",
                                    outputId = "savemapB"),
                     br()
              )
            )
)



body <- dashboardBody(
  fluidRow(
    tabsetPanel(
      tabPanel("Stations",
               timeseries),
      tabPanel("Maps", 
               maps),
      tabPanel("About the app",
               introbox)
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
    ),
    menuItem("\n Plot settings - Stations", 
             icon = icon("list"), tabname="settings_ts", startExpanded = FALSE,
             br(),
             checkboxInput("normalize_ts",
                           label = "Normalize",
                           value = FALSE),
             selectInput("plottype_station",
                           label = "Type of plot",
                           choices = c("time series", "seasonal cycle"),
                           selected = "time series"),
             sliderInput("tsrange", label="Range of y-axis",
                         min=-30, max=50, step = 1, value=c(-5,30))),
    menuItem("\n Plot settings - Maps", 
             icon = icon("list"), tabname="settings_map", startExpanded = FALSE,
             br(),
             selectInput("funA",
                         label = "A: Time aggregation",
                         choices=c('mean','trend','max','min','sd'),
                         selected='trend'),
             selectInput("datesA",
                         label="A: Years",
                         choices=names(datelist),
                         selected=names(datelist)[[1]]),
             selectInput("funB",
                         label = "B: Time aggregation",
                         choices=c('mean','trend','max','min','sd'),
                         selected='trend'),
             selectInput("datesB",
                         label="B: Years",
                         choices=names(datelist),
                         selected=names(datelist)[[1]]),
             checkboxInput("robustness_map",
                           label = "show trend robustness (same sign in 90% of ensemble members)",
                           value = TRUE),
             checkboxInput("landmask",
                           label = "mask ocean",
                           value = TRUE),
             sliderInput("valrangeA", label="A: Range of colorscale",
                         min=-30, max=50, step = 1, value=c(-5,20)),
             sliderInput("valrangeB", label="B: Range of colorscale",
                         min=-30, max=50, step = 1, value=c(-5,20)))
  )
)



dashboardPage(
  header,
  sideboard,
  body
)

