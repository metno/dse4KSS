library(shiny)
library(esd)

# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

t2m.locs <- sort(loc(Z4[[1]]$pca))
pre.locs <- sort(loc(Z4[[2]]$pca))
gcmnames <<- names(Z4[[1]])[-c(1,2,length(Z4[[1]]))]
locs2 <- t2m.locs

navbarPage("The Nordic region climate atlas",
           tabPanel("Maps", 
                    plotOutput("maps", width = "100%", height = "80%"),
                    column(2,
                           selectInput("file1", 
                                       label = "Results",
                                       choices = nms,
                                       selected = nms[1])),
                    column(2,
                           sliderInput("lon1", 
                                       label = "Longitudes",
                                       min = 0, max = 30, value = c(0, 30))),
                    column(2,
                           sliderInput("lat1", 
                                       label = "Latitudes",
                                       min = 55, max = 72, value = c(55, 72))),
                    column(2,
                           selectInput("fun1", 
                                       label = "Time aggregation",
                                       choices=c('mean','trend','max','min','sd'),selected='mean')),
                    column(2,
                           selectInput("funx1", 
                                       label = "Ensemble aggregation",
                                       choices=c('mean','sd','max','min'),selected='mean')),
                    column(2,
                           sliderInput("dates1", "Years", 
                                       min=1950, max=2050, value= c(2049,2055),sep=""))
           ),
           tabPanel("Single location", 
                    plotOutput("plot", width = "100%", height = "80%"),
                    column(2,
                           selectInput("file2", 
                                       label = "Results",
                                       choices = nms,
                                       selected = nms[1])),
                    column(2,
                           uiOutput("locations2")),  
column(3,
       sliderInput("dates2", "Years", 
                   min=1950, max=2050, value= c(2050,2050),sep=""))
)
)
