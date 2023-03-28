# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shiny)
library(shinydashboard)
library(leaflet)


sourcelist <- c("Empirical statistical downscaling (MetNo ESD)", 
                "Dynamical downscaling (CORDEX RCM)")#c("ESD_Nordic", "ESD_Finland", "RCM", "GCM")

header <- dashboardHeader(
  title = "The Nordic region climate atlas",
  titleWidth = '600px'
)


timeseries <- box(width=NULL, title="Time series",
                  collapsible = TRUE, collapsed=FALSE,
                  column(4,
                         leafletOutput("map"),#, width="20vw"),
                         absolutePanel(bottom = 30, left = 20, draggable=TRUE,
                         selectInput("location", width = "120px",
                                       label = "Location",
                                       choices = locs[[var0]]$label,#locs[[reg0]][[var0]]$label,
                                       selected = locs[[var0]]$label[[1]])#locs[[reg0]][[var0]]$label[[1]])
                         )
                  ),
                  column(8,
                         plotlyOutput("figts", width = "100%", height = "60%")
                  )
)


maps <- box(width=NULL, title="Maps",
            collapsible = TRUE, collapsed=FALSE,
            column(6,
                   h5("Ensemble A"),
                   br(),
                   br(),
                   selectInput("fun1",
                               label = "Time aggregation",
                               choices=c('mean','trend','max','min','sd'),
                               selected='trend'),
                   selectInput("dates1",
                               label="Years",
                               choices=names(datelist),
                               selected=names(datelist)[[1]]),
                   br(),
                   br(),
                   htmlOutput("main1"),
                   br(),
                   br(),
                   plotOutput("fig1", width = "100%", height = "80%"),
                   downloadButton(label = "save",
                                  outputId = "savemaps"),
                   br(),
                   br()                   
            ),
            column(6, 
                   h5("Ensemble B"),
                   br(),
                   br(),
                   selectInput("fun2",
                               label = "Time aggregation",
                               choices=c('mean','trend','max','min','sd'),
                               selected='trend'),
                   selectInput("dates2",
                               label="Years",
                               choices=names(datelist),
                               selected=names(datelist)[[1]]),
                   br(),
                   br(),
                   htmlOutput("main2"),
                   br(),
                   br(),
                   plotOutput("fig2", width = "100%", height = "80%"),
                   downloadButton(label = "save",
                                  outputId = "savemaps2"),
                   br(),
                   br()
            )
)


advanced <- box(width=NULL, title="Advanced settings",
                collapsible = TRUE, collapsed=TRUE,
                column(6,
                       sliderInput("valrange1", label="Range of colorscale in map A",
                                   min=-30, max=50, step = 1, value=c(-5,20)),
                       sliderInput("valrange2", label="Range of colorscale in map B",
                                   min=-30, max=50, step = 1, value=c(-5,20)),
                       sliderInput("tsrange1", label="Range of y-axis in time series plot",
                                   min=-30, max=50, step = 1, value=c(-5,20)),
                       checkboxInput("landmask",
                                     label = "mask ocean in maps with regional climate model (RCM) results",
                                     value = TRUE),
                       checkboxInput("robustness_map",
                                     label = "show trend robustness (same sign in 90% of ensemble members) in maps",
                                     value = TRUE)
                )
)


body <- dashboardBody(
  fluidRow(
    column(12,
           timeseries
    )
  ),
  fluidRow(
    column(12,
           maps
    ),
    column(12,
           advanced
    )
  )
)

sideboard <- dashboardSidebar(
  sidebarMenu(
    menuItem("\n Season and variable", tabName = "dashboard", 
             icon = icon("cloud"), tabname="settings",
             selectInput("var1",
                         label = "Variable",
                         choices = as.vector(sapply(unique(cats$var), varname)),
                         selected = varname(var0)),
             selectInput("seas1",
                         label = "Season",
                         choices = as.vector(sapply(unique(cats$it), seasonname)),
                         selected = seasonname(seas0))
    ),
    menuItem(" Ensemble A", icon = icon("list"), tabName = "A", startExpanded = FALSE,
             br(),
             selectInput("src1",
                         label="Data source",
                         choices = sourcelist,
                         selected = sourcelist[[1]]),
             selectInput("sce1",
                         label = "Scenario",
                         choices = scenarios,
                         selected = scenarioname(sce0)),
             br(),
             actionButton("gcmall1",
                          label = "All simulations",
                          width = '150px'
             ),
             actionButton("gcmone1",
                          label = "One of each GCM",
                          width = '150px'
             ),
             actionButton("gcmdeselect1",
                          label = "Deselect all",
                          width = '150px'
             ),
             br(),
             checkboxGroupInput("gcms1",
                                label = "Climate models",
                                choices = gcmnames[[var0]][[sce0]],
                                selected = gcmnames[[var0]][[sce0]],
                                inline=TRUE,
                                width='100%'
             )
    ),
    menuItem("\n Ensemble B", icon = icon("list"), tabname="B", startExpanded = FALSE,
             br(),
             selectInput("src2",
                         label="Data source",
                         choices = sourcelist,
                         selected = sourcelist[[2]]),
             selectInput("sce2",
                         label = "Scenario",
                         choices = scenarios,
                         selected = scenarioname(sce0)),
             br(),
             actionButton("gcmall2",
                          label = "All simulations",
                          width = '150px'
             ),
             actionButton("gcmone2",
                          label = "One of each GCM",
                          width = '150px'
             ),
             actionButton("gcmdeselect2",
                          label = "Deselect all",
                          width = '150px'
             ),
             br(),
             checkboxGroupInput("gcms2",
                                label = "Climate models",
                                choices = gcmnames[[var0]][[sce0]],
                                selected = gcmnames[[var0]][[sce0]],
                                inline=TRUE,
                                width='100%')
    ),
    menuItem(" Plot settings", icon = icon("gear"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2"))
    
  )
)



dashboardPage(
  header,
  sideboard,
  body
)

