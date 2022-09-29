# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shiny)
library(shinydashboard)

## Initial choice of region, variable etc
reg0 <- "Nordic"
var0 <- "t2m"
sce0 <- "ssp585"
it0 <- "djf"

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Maps", tabName = "maps"),
    menuItem("Single stations", tabName = "stations"),
    menuItem("Cross validation", tabName = "xval"),
    div(style = "font-size:10px",
      selectInput("reg1",
                  label = "Region",
                  choices = unique(cats$region),
                  selected = reg0),
      selectInput("var1",
                  label = "Variable",
                  choices = as.vector(sapply(unique(cats$var), varname)),
                  selected = varname(var0)),
      selectInput("it1",
                  label = "Season",
                  choices = as.vector(sapply(unique(cats$it), seasonname)),
                  selected = seasonname(it0)),
      selectInput("sce1",
                  label = "Scenario",
                  choices = unique(cats$sce),
                  selected = sce0)
    ),
    menuItem("Ensemble selection", tabName="spread",
             div(style = "font-size:10px",
                 actionButton("gcmall", 
                              label = "All simulations", 
                              width = '150px'
                 ),
                 actionButton("gcmone", 
                              label = "One of each GCM", 
                              width = '150px'
                 ),
                 checkboxGroupInput("gcms",
                                    label = "Climate models",
                                    choices = gcmnames[[var0]][[sce0]],
                                    selected = gcmnames[[var0]][[sce0]],
                                    inline=TRUE,
                                    width='100%'
                 )
             )
    )
  )
)

tab.maps <- tabItem(
  tabName = "maps",
  h2("Maps"),
  plotOutput("maps", width = "100%", height = "80%"),
  downloadButton(label = "save", 
                 outputId = "savemaps"),
  br(),
  br(),
  fluidRow(
    column(4,
           selectInput("fun1", 
                       label = "Time aggregation",
                       choices=c('mean','trend','max','min','sd'),
                       selected='trend')),
    column(4,
           selectInput("funx1",
                       label = "Ensemble aggregation",
                       choices=c('mean','sd','max','min'),
                       selected='mean'))
  ),
  fluidRow(
    column(5,
           selectInput("dates1", 
                       label="Years",
                       choices=names(datelist),
                       selected=names(datelist)[[1]])
    ),
    column(5,
           sliderInput("maprange", label="Range of color scale",
                       min=-30, max=50, step = 1, value=c(-5,20))
    )
  ),
  br(),
  h4("Robustness of trends"),
  fluidRow(
    column(10,
           checkboxInput("robustness_map",
                         label = "show robustness for the period 1950-2100",
                         value = FALSE)
    )
  ),
  fluidRow(
    column(10,
           numericInput("threshold_map",
                        label = "same sign trend in X% of ensemble members",
                        value = 95, min = 50, max = 100, step=1)
    )
  )
)


tab.station <- tabItem(
  tabName = "stations",
  h2("Single location"),
  plotOutput("plot", width = "100%", height = "80%"),
  downloadButton(label = "save", 
                 outputId = "savest"),
  br(),
  fluidRow(
    column(5,
           selectInput("location2",
                      label = "Location",
                      choices = locs[[reg0]][[var0]]$label,
                      selected = locs[[reg0]][[var0]]$label[[1]])
    )
  ),
  fluidRow(
    column(5,
         sliderInput("dates2", "Years",
                     min=1950, max=2100, value= c(1950,2100),sep="")
    )
  ),
  fluidRow(
    column(5,
         sliderInput("tsrange", label="Range of color scale",
                     min=-30, max=50, step = 1, value=c(-5,20))
    )
  )
)


tab.xval <- tabItem(
  tabName = "xval",
  h2("Cross validation"),
  plotOutput("xval", width = "100%", height = "80%"),
)



body <- dashboardBody(
  tabItems(
    tab.maps,
    tab.station,
    tab.xval
  )
)

header <- dashboardHeader(
    title = "The Nordic region climate atlas",
    titleWidth = '600px',
    dropdownMenuOutput("messageMenu")
)

dashboardPage(
    skin = HTML("blue"),
    header,
    sidebar,
    body
)

