
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)
  
  # Variable name - figure 1
  var1 <- reactive({ varname(input$var1, long=FALSE) })
  
  # Season name - figure 1
  season1 <- reactive({ seasonname(input$it1, long=FALSE) })
  
  # Variable name - figure 2
  var2 <- reactive({ varname(input$var2, long=FALSE) })
  
  # Season name - figure 2
  season2 <- reactive({ seasonname(input$it2, long=FALSE) })
  
  # Date range - figure 1
  it1 <- reactive({
    return(datelist[[input$dates1]])
  })
  
  # Date range - figure 2
  it2 <- reactive({
    return(datelist[[input$dates2]])
  })
  

  # Update ensemble when changing variable or scenario  - figure 1
  observe({
    choices <- gcmnames[[var1()]][[input$sce1]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms1",
                       label = "Climate models",
                       choices = choices,
                       selected = selected)
  })

  # Select only one simulation from each GCM  - figure 1
  observeEvent(input$gcmone1, {
    choices <- gcmnames[[var1()]][[input$sce1]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })

  # Select all simulations - figure 1
  observeEvent(input$gcmall1, {
    choices <- gcmnames[[var1()]][[input$sce1]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations  - figure 1
  observeEvent(input$gcmdeselect1, {
    choices <- gcmnames[[var1()]][[input$sce1]]
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })

  # Update ensemble when changing variable or scenario  - figure 2
  observe({
    choices <- gcmnames[[var2()]][[input$sce2]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms2",
                             label = "Climate models",
                             choices = choices,
                             selected = selected)
  })

  # Select only one simulation from each GCM  - figure 2
  observeEvent(input$gcmone2, {
    choices <- gcmnames[[var2()]][[input$sce2]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Select all simulations  - figure 2
  observeEvent(input$gcmall2, {
    choices <- gcmnames[[var2()]][[input$sce2]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations - figure 2
  observeEvent(input$gcmdeselect2, {
    choices <- gcmnames[[var2()]][[input$sce2]]
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Set default date range to full length (1950-2100) for trend maps  - figure 1
  observe({
    if(grepl("maps", tolower(input$plottype))) if(input$fun1=="trend") {
      updateSelectInput(session, "dates1",
                  label="Years",
                  choices=names(datelist),
                  selected=names(datelist)[[1]])
    }
  })

  # Set default date range to full length (1950-2100) for trend maps  - figure 2
  observe({
    if(grepl("maps", tolower(input$plottype))) if(input$fun2=="trend") {
      updateSelectInput(session, "dates2",
                        label="Years",
                        choices=names(datelist),
                        selected=names(datelist)[[1]])
    }
  })

  # Set default date range to full length (1950-2100) for station plots  - figure 1
  observe({
    if(grepl("station", tolower(input$plottype))) {
      updateSelectInput(session, "dates1",
                        label="Years",
                        choices=names(datelist),
                        selected=names(datelist)[[1]])
    }
  })

  # Set default date range to full length (1950-2100) for station plots  - figure 2
  observe({
    if(grepl("station", tolower(input$plottype))) {
      updateSelectInput(session, "dates2",
                        label="Years",
                        choices=names(datelist),
                        selected=names(datelist)[[1]])
    }
  })

  # Make location selection available for station plots - figure 1
  observe({
    if(grepl("station",input$plottype)) {
      updateSelectInput(session, "location1",
                        choices = locs[[input$reg1]][[var1()]]$label,
                        selected = locs[[input$reg1]][[var1()]]$label[[1]])
    }
  })

  # Make location selection available for station plots - figure 2
  observe({
    if(grepl("station",input$plottype)) {
      updateSelectInput(session, "location2",
                        choices = locs[[input$reg2]][[var2()]]$label,
                        selected = locs[[input$reg2]][[var2()]]$label[[1]])
    }
  })

  # Disable location selection for maps - figure 1
  observe({
    if(grepl("maps",input$plottype)) {
      updateSelectInput(session, "location1",
                        choices = c("-"),
                        selected = "-")
    }
  })

  # Disable location selection for maps - figure 2
  observe({
    if(grepl("maps",input$plottype)) {
      updateSelectInput(session, "location2",
                        choices = c("-"),
                        selected = "-")
    }
  })

  # Update location choices when variable or region is changed - location 1
  observe({
    if(input$location1 %in% locs[[input$reg1]][[var1()]]$label) {
      choices <- locs[[input$reg1]][[var1()]]$label
      sel <- input$location1
    } else if(input$location1!="-") {
      choices <- locs[[input$reg1]][[var1()]]$label
      is <- grep(cleanstr(input$location1, "[0-9]"),
                 cleanstr(choices, "[0-9]"))
      if(length(is)>1) {
        is <- is[[1]]
      } else if(length(is)==0) {
        is <- 1
      }
      sel <- choices[[is]]
    } else {
      choices <- c("-")
      sel <- "-"
    }
    updateSelectInput(session, "location1",
                      choices = choices, # update choices
                      selected = sel) # remove selection
  })

  # Update location choices when variable or region is changed - location 2
  observe({
    if(input$location2 %in% locs[[input$reg2]][[var2()]]$label) {
      choices <- locs[[input$reg2]][[var2()]]$label
      sel <- input$location2
    } else if(input$location2!="-") {
      choices <- locs[[input$reg2]][[var2()]]$label
      is <- grep(cleanstr(input$location2, "[0-9]"),
                 cleanstr(choices, "[0-9]"))
      if(length(is)>1) {
        is <- is[[1]]
      } else if(length(is)==0) {
        is <- 1
      }
      sel <- choices[[is]]
    } else {
      choices <- c("-")
      sel <- "-"
    }
    updateSelectInput(session, "location2",
                      choices = choices, # update choices
                      selected = sel) # remove selection
  })

  # Update range of color scale in map - figure 1
  observe({
    if(grepl("map", tolower(input$plottype))) {
      x <- sliderange(param=var1(), FUN=input$fun1)
    } else {
      x <- sliderange(param=var1(), FUN="mean")
    }
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrange1", label="Range of colorscale/y-axis",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })

  # Update range of color scale in map - figure 2
  observe({
    if(grepl("map", tolower(input$plottype))) {
      x <- sliderange(param=var2(), FUN=input$fun2)
    } else {
      x <- sliderange(param=var2(), FUN="mean")
    }
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrange2", label="Range of colorscale/y-axis",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })

  # Load data for figure 1
  zload_pc <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg1, var1(),
                         season1(), input$sce1))
    return(Z)
  })
  
  # Load data for figure 2
  zload_pc_2 <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg2, var2(),
                         season2(), input$sce2))
    return(Z)
  })
  
  # Load data and transform to station data - figure 1 
  zload_station <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg1, var1(),
                         season1(), input$sce1))
    y <- as.station(Z)
    attr(y, "variable") <- attr(Z, "variable")
    attr(y, "longname") <- attr(Z, "longname")
    attr(y, "unit") <- attr(Z, "unit")
    return(y)
  })

  # Load data and transform to station data - figure 2
  zload_station_2 <- reactive({
    Z <- zload_pc_2()
    y <- as.station(Z)
    attr(y, "variable") <- attr(Z, "variable")
    attr(y, "longname") <- attr(Z, "longname")
    attr(y, "unit") <- attr(Z, "unit")
    return(y)
  })
  
  # Change default model simulations when changing variable and scenario - figure 1
  im1 <- reactive({
    choices <- gcmnames[[var1()]][[input$sce1]]
    im <- choices %in% input$gcms1
    return(im)
  })
  
  # Change default model simulations when changing variable and scenario - figure 2
  im2 <- reactive({
    choices <- gcmnames[[var2()]][[input$sce2]]
    im <- choices %in% input$gcms2
    return(im)
  })
  
  # Caption for figure 1
  output$main1 <- renderText(paste0("<b>", input$fun1, "</b>", 
                                    " of the ",switch(input$src1, 
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "), 
                                    "<b>", input$var1, "</b>"," for the ", 
                                    "<b>", input$it1, "</b>", " season ",
                                    "in the period ", "<b>", input$dates1, "</b>",
                                    ", assuming emission scenario ", 
                                    "<b>", input$sce1, "</b>", 
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms1), " simulations)")))

  # Caption for figure 2
  output$main2 <- renderText(paste0("<b>", input$fun2, "</b>", 
                                    " of the ",switch(input$src2, 
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "), 
                                    "<b>", input$var2, "</b>"," for the ", 
                                    "<b>", input$it2, "</b>", " season ",
                                    "in the period ", "<b>", input$dates2, "</b>",
                                    ", assuming emission scenario ", 
                                    "<b>", input$sce2, "</b>", 
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms2), " simulations)")))
  
  # Reactive plot function for saving - figure 1
  figure1 <- reactive({
    print('output$figure1')
    if(grepl("figure1", tolower(input$plottype))) {
      z <- zload_pc()
      mapgridded(z, im=im1(), it=it1(),
                 FUN=input$fun1, FUNX="mean", MET=input$src1,#input$funx1, 
                 show.field=input$field, show.station=input$stations,
                 colbar=list(breaks=pretty(input$valrange1, n=22)),
                 show.robustness = input$robustness_map,
                 threshold = 0.9,#input$threshold_map/100,
                 trends=T4[[input$reg1]][[var1()]][[input$sce1]][[season1()]])
    } else if(grepl("stations", tolower(input$plottype))) {
      z <- zload_station()
      stplot(z, is=input$location1, it=it1(), im=im1(),
             MET=input$src1, ylim=input$valrange1)
    } else if(grepl("cross", tolower(input$plottype))) {
      z <- zload_pc()
      crossval(z, im=im1())
    }
  })
  
  # Reactive plot function for saving - figure 1
  fig2 <- reactive({
    print('output$fig2')
    if(grepl("maps", tolower(input$plottype))) {
      z <- zload_pc_2()
      mapgridded(z, im=im2(), it=it2(),
                 FUN=input$fun2, FUNX="mean", MET=input$src2,#input$funx1, 
                 show.field=input$field, show.station=input$stations,
                 colbar=list(breaks=pretty(input$valrange2, n=22)),
                 show.robustness = input$robustness_map,
                 threshold = 0.9,#input$threshold_map/100,
                 trends=T4[[input$reg2]][[var2()]][[input$sce2]][[season2()]])
    } else if(grepl("stations", tolower(input$plottype))) {
      z <- zload_station_2()
      stplot(z, is=input$location2, it=it2(), im=im2(),
             MET=input$src2, ylim=input$valrange1)
    } else if(grepl("cross", tolower(input$plottype))) {
      z <- zload_pc_2()
      crossval(z, im=im2())
    }
  })
  


  ## Show map of gridded temperature
  output$fig1 <- renderPlot({
    print('output$fig1')
    if(grepl("maps", tolower(input$plottype))) {
      z <- zload_pc()
      mapgridded(z, im=im1(), it=it1(),
                 FUN=input$fun1, FUNX="mean", MET=input$src1,#input$funx1, 
                 show.field=input$field, show.station=input$stations,
                 colbar=list(breaks=pretty(input$valrange1, n=22)),
                 show.robustness = input$robustness_map,
                 threshold = 0.9,#input$threshold_map/100,
                 trends=T4[[input$reg1]][[var1()]][[input$sce1]][[season1()]])
    } else if(grepl("stations", tolower(input$plottype))) {
      z <- zload_station()
      stplot(z, is=input$location1, it=it1(), im=im1(),
             MET=input$src1, ylim=input$valrange1)
    } else if(grepl("cross", tolower(input$plottype))) {
      z <- zload_pc()
      crossval(z, im=im1())
    }
  }, height=function(){1.0*session$clientData$output_fig1_width})
  
  ## Show map of gridded temperature
  output$fig2 <- renderPlot({
    print('output$selectplot')
    if(grepl("maps", tolower(input$plottype))) {
      z <- zload_pc_2()
      mapgridded(z, im=im2(), it=it2(),
                 FUN=input$fun2, FUNX="mean", MET=input$src2,#input$funx1, 
                 show.field=input$field, show.station=input$stations,
                 colbar=list(breaks=pretty(input$valrange2, n=22)),
                 show.robustness = input$robustness_map,
                 threshold = 0.9,#input$threshold_map/100,
                 trends=T4[[input$reg2]][[var2()]][[input$sce2]][[season2()]])
    } else if(grepl("stations", tolower(input$plottype))) {
      z <- zload_station_2()
      stplot(z, is=input$location2, it=it2(), im=im2(),
             MET=input$src2, ylim=input$valrange1)
    } else if(grepl("cross", tolower(input$plottype))) {
      z <- zload_pc_2()
      crossval(z, im=im2())
    }
  }, height=function(){1.0*session$clientData$output_fig2_width})
  
  output$savefig1 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$src1, input$fun1, input$reg1, var1(),
            input$it1, input$sce1, "ensemblemean",#input$funx1, 
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      fig1()
      dev.off()
    }
  )

  output$savefig2 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$src2, input$fun2, input$reg2, var2(),
            input$it2, input$sce2, "ensemblemean",#input$funx1, 
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      fig2()
      dev.off()
    }
  )    

})
