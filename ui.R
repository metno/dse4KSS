# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shiny)
library(shinydashboard)
library(leaflet)

## Initial choice of region, variable etc
reg0 <- "Nordic"
var0 <- "pr"
sce0 <- "rcp85"
seas0 <- "djf"

header <- dashboardHeader(
  title = "The Nordic region climate atlas",
  titleWidth = '600px'
)


gcmbox1 <- box(
  title = HTML("<font size=-0.5 color='black'><i>Model selection</i></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = TRUE,
  collapsed = TRUE,
  div(style = "font-size:12px",
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
      br(),
      checkboxGroupInput("gcms1",
                         label = "Climate models",
                         choices = gcmnames[[var0]][[sce0]],
                         selected = gcmnames[[var0]][[sce0]],
                         inline=TRUE,
                         width='100%'
      )
  )
)

gcmbox2 <- box(
  title = HTML("<font size=-0.5 color='black'><i>Model selection</i></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = TRUE,
  collapsed = TRUE,
  div(style = "font-size:12px",
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
      br(),
      checkboxGroupInput("gcms2",
                         label = "Climate models",
                         choices = gcmnames[[var0]][[sce0]],
                         selected = gcmnames[[var0]][[sce0]],
                         inline=TRUE,
                         width='100%')
  )
)

selectVar <- box(width = NULL, status="warning", title="Plot settings",
                 collapsible = TRUE, collapsed=FALSE,
                 div(style = "font-size:12px",
                     selectInput("var1",
                                 label = "Variable",
                                 choices = as.vector(sapply(unique(cats$var), varname)),
                                 selected = varname(var0)),
                     selectInput("seas1",
                                 label = "Season",
                                 choices = as.vector(sapply(unique(cats$it), seasonname)),
                                 selected = seasonname(seas0))
                 )
)

selectA <- box(width = NULL, status="warning", title="Ensemble A",
               collapsible = TRUE, collapsed=FALSE,
               div(style = "font-size:12px",
                   #selectInput("reg1",
                   #            label = "Region",
                   #            choices = unique(cats$region),
                   #            selected = reg0),
                   #selectInput("var1",
                   #            label = "Variable",
                   #            choices = as.vector(sapply(unique(cats$var), varname)),
                   #            selected = varname(var0)),
                   #selectInput("seas1",
                   #            label = "Season",
                   #            choices = as.vector(sapply(unique(cats$it), seasonname)),
                   #            selected = seasonname(seas0)),
                   selectInput("src1",
                               label="Data source",
                               choices = c("ESD_Nordic", "ESD_Finland", "RCM", "GCM"),
                               selected = "ESD_Nordic"),
                   selectInput("sce1",
                               label = "Scenario",
                               choices = unique(cats$sce),
                               selected = sce0),
                   gcmbox1)
)

selectB <- box(width=NULL, status="warning", title = "Ensemble B",
               collapsible = TRUE, collapsed=FALSE,
               div(style = "font-size:12px",
                   #selectInput("reg2",
                   #            label = "Region",
                   #            choices = unique(cats$region),
                   #            selected = reg0),
                   #selectInput("var2",
                   #            label = "Variable",
                   #            choices = as.vector(sapply(unique(cats$var), varname)),
                   #            selected = varname(var0)),
                   #selectInput("seas2",
                   #            label = "Season",
                   #            choices = as.vector(sapply(unique(cats$it), seasonname)),
                   #            selected = seasonname(seas0)),
                   selectInput("src2",
                               label="Data source",
                               choices = c("ESD_Nordic", "ESD_Finland", "RCM", "GCM"),
                               selected = "RCM"),
                   selectInput("sce2",
                               label = "Scenario",
                               choices = unique(cats$sce),
                               selected = sce0),
                   gcmbox2)
)

timeseries <- box(width=NULL, title="Time series",
                  collapsible = TRUE, collapsed=FALSE,
                  column(3,
                         selectInput("location1",
                                     label = "Location",
                                     choices = locs[[reg0]][[var0]]$label,
                                     selected = locs[[reg0]][[var0]]$label[[1]]),
                         leafletOutput("maptsf",width="20vw")
                         #plotOutput("mapts", width="100%", height="100%")
                  ),
                  column(9,
                         plotOutput("figts", width = "100%", height = "50%")
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
                               selected='mean'),
                   selectInput("dates1",
                               label="Years",
                               choices=names(datelist),
                               selected=names(datelist)[[2]]),
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
                               selected='mean'),
                   selectInput("dates2",
                               label="Years",
                               choices=names(datelist),
                               selected=names(datelist)[[length(datelist)]]),
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
    column(4,
           selectVar
    ),
    column(4,
           selectA
    ),
    column(4,
           selectB    
    )
  ),
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



dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

