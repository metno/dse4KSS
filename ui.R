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
    menuItem("Settings plot 1", tabname="selection1",
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
                             selected = sce0),
                 selectInput("fun1", 
                             label = "Time aggregation",
                             choices=c('mean','trend','max','min','sd'),
                             selected='mean'),
                 selectInput("dates1", 
                             label="Years",
                             choices=names(datelist),
                             selected=names(datelist)[[2]]),
                 sliderInput("maprange", label="Range of color scale",
                             min=-30, max=50, step = 1, value=c(-5,20))
             )
    ),
    menuItem("Settings plot 2", tabname="selection2",
             div(style = "font-size:10px",
                 selectInput("reg2",
                             label = "Region",
                             choices = unique(cats$region),
                             selected = reg0),
                 selectInput("var2",
                             label = "Variable",
                             choices = as.vector(sapply(unique(cats$var), varname)),
                             selected = varname(var0)),
                 selectInput("it2",
                             label = "Season",
                             choices = as.vector(sapply(unique(cats$it), seasonname)),
                             selected = seasonname(it0)),
                 selectInput("sce2",
                             label = "Scenario",
                             choices = unique(cats$sce),
                             selected = sce0),
                 selectInput("fun2", 
                             label = "Time aggregation",
                             choices=c('mean','trend','max','min','sd'),
                             selected='mean'),
                 selectInput("dates2", 
                             label="Years",
                             choices=names(datelist),
                             selected=names(datelist)[[length(datelist)]]),
                 sliderInput("maprange2", label="Range of color scale",
                             min=-30, max=50, step = 1, value=c(-5,20)))
    )
  )
)

tab.maps <- tabItem(
  tabName = "maps",
  h2("Maps"),
  fluidRow(
    column(6,
           plotOutput("maps", width = "100%", height = "80%"),
           downloadButton(label = "save", 
                          outputId = "savemaps"),
           br(),
           textOutput("main1"),
           br(),
           div(style = "font-size:10px",
               checkboxInput("field1",
                             label = "show field",
                             value = TRUE),
               checkboxInput("stations1",
                             label = "show stations",
                             value = TRUE),
               checkboxInput("robustness_map",
                             label = "show trend robustness for the period 1950-2100 (same sign in 90% of ensemble members)",
                             value = FALSE)),
           box(
             title = HTML("<font size=-0.5 color='black'><b>Ensemble selection</b></font>"),
             width = '100%' ,
             status = 'primary',
             collapsible = TRUE,
             collapsed = TRUE,
             div(style = "font-size:10px",
                 actionButton("gcmall",
                              label = "All simulations",
                              width = '150px'
                 ),
                 actionButton("gcmone",
                              label = "One of each GCM",
                              width = '150px'
                 ),
                 br(),
                 checkboxGroupInput("gcms",
                                    label = "Climate models",
                                    choices = gcmnames[[var0]][[sce0]],
                                    selected = gcmnames[[var0]][[sce0]],
                                    inline=TRUE,
                                    width='100%'
                 )))
    ),
    column(6,
           plotOutput("maps2", width = "100%", height = "80%"),
           downloadButton(label = "save", 
                          outputId = "savemaps2"),
           br(),
           textOutput("main2"),
           br(),
           div(style = "font-size:10px",
               checkboxInput("field2",
                             label = "show field",
                             value = TRUE),
               checkboxInput("stations2",
                             label = "show stations",
                             value = TRUE),
               checkboxInput("robustness_map2",
                             label = "show trend robustness for the period 1950-2100 (same sign in 90% of ensemble members)",
                             value = FALSE)),
           box(
             title = HTML("<font size=-0.5 color='black'><b>Ensemble selection</b></font>"),
             width = '100%' ,
             status = 'primary',
             collapsible = TRUE,
             collapsed = TRUE,
             div(style = "font-size:10px",
                 actionButton("gcmall2",
                              label = "All simulations",
                              width = '150px'
                 ),
                 actionButton("gcmone2",
                              label = "One of each GCM",
                              width = '150px'
                 ),
                 br(),
                 checkboxGroupInput("gcms2",
                                    label = "Climate models",
                                    choices = gcmnames[[var0]][[sce0]],
                                    selected = gcmnames[[var0]][[sce0]],
                                    inline=TRUE,
                                    width='100%'
                 )))
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
  #fluidRow(
  #  column(5,
  #         sliderInput("dates2", "Years",
  #                     min=1950, max=2100, value= c(1950,2100),sep="")
  #  )
  #),
  fluidRow(
    column(5,
           sliderInput("tsrange1", label="Range of color scale",
                       min=-30, max=50, step = 1, value=c(-5,20))
    )
  ),
  fluidRow(
    column(5,
           sliderInput("tsrange2", label="Range of color scale",
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

