# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shiny)

## Initial choice of region, variable etc
reg0 <- "Nordic"
var0 <- "pr"
sce0 <- "rcp85"
it0 <- "djf"

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(" ", tabName = "maps"),
    #menuItem("Single stations", tabName = "stations"),
    #menuItem("Cross validation", tabName = "xval"),
    menuItem("Settings Figure 1", tabname="selection1",
             div(style = "font-size:10px",
                 selectInput("reg1",
                             label = "Region",
                             choices = unique(cats$region),
                             selected = reg0),
                 selectInput("fun1", 
                             label = "Time aggregation",
                             choices=c('mean','trend','max','min','sd'),
                             selected='mean'),
                 selectInput("dates1", 
                             label="Years",
                             choices=names(datelist),
                             selected=names(datelist)[[2]]),
                 sliderInput("valrange1", label="Range of colorscale/y-axis",
                             min=-30, max=50, step = 1, value=c(-5,20))
             )
    ),
    menuItem("Settings Figure 2", tabname="selection2",
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
                 selectInput("fun2", 
                             label = "Time aggregation",
                             choices=c('mean','trend','max','min','sd'),
                             selected='mean'),
                 selectInput("dates2", 
                             label="Years",
                             choices=names(datelist),
                             selected=names(datelist)[[length(datelist)]]),
                 sliderInput("valrange2", label="Range of colorscale/y-axis",
                             min=-30, max=50, step = 1, value=c(-5,20))),
                  selectInput("location2",
                         label = "Location",
                         choices = locs[[reg0]][[var0]]$label,
                         selected = locs[[reg0]][[var0]]$label[[1]])
             
    ),
    menuItem("Advanced settings", tabname="advanced", 
             div(style = "font-size:10px",
                 h5("Maps"),
                 checkboxInput("landmask",
                               label = "mask ocean (RCM)",
                               value = TRUE),
                 checkboxInput("robustness_map",
                               label = "show trend robustness (same sign in 90% of ensemble members)",
                               value = TRUE)
             )
    )
  )
)

gcmbox1 <- box(
  title = HTML("<font size=-0.5 color='black'><b>Ensemble selection</b></font>"),
  width = '100%' ,
  status = 'primary',
  collapsible = TRUE,
  collapsed = TRUE,
  div(style = "font-size:10px",
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
      )))

gcmbox2 <- box(
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

tab.figs <- tabItem(
  tabName = "maps",
  #selectInput("plottype",
  #            label = "What to show?",
  #            choices = c("Maps", "Individual stations"),# "Cross-validation (esd)"),
  #            selected = "Maps"),
  selectInput("var1",
              label = "Variable",
              choices = as.vector(sapply(unique(cats$var), varname)),
              selected = varname(var0)),
  selectInput("it1",
              label = "Season",
              choices = as.vector(sapply(unique(cats$it), seasonname)),
              selected = seasonname(it0)),
  fluidRow(
    column(12, h3("Time series"),
           plotOutput("figts", width = "100%", height = "50%"),
           selectInput("location1",
                       label = "Location",
                       choices = locs[[reg0]][[var0]]$label,
                       selected = locs[[reg0]][[var0]]$label[[1]]),
    ),
    column(6, h3("Map ensemble 1"),
           plotOutput("fig1", width = "100%", height = "80%"),
           downloadButton(label = "save", 
                          outputId = "savemaps"),
           br(),
           htmlOutput("main1"),
           br(),
           div(style = "font-size:10px",
               selectInput("src1", 
                           label="Data source",
                           choices = c("ESD", "RCM", "GCM"),
                           selected = "ESD"),
               selectInput("sce1",
                           label = "Scenario",
                           choices = unique(cats$sce),
                           selected = sce0),
               gcmbox1)
    ),
    column(6, h3("Map ensemble 2"),
           plotOutput("fig2", width = "100%", height = "80%"),
           downloadButton(label = "save", 
                          outputId = "savemaps2"),
           br(),
           htmlOutput("main2"),
           br(),
           div(style = "font-size:10px",
               selectInput("src2", 
                           label="Data source",
                           choices = c("ESD", "RCM", "GCM"),
                           selected = "RCM"),
               selectInput("sce2",
                           label = "Scenario",
                           choices = unique(cats$sce),
                           selected = sce0),
               gcmbox2)
    )
  )
)


body <- dashboardBody(
  tabItems(
    tab.figs
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

