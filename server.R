
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)

  # Variable name - figure 1
  var1 <- reactive({ varname(input$var1, long=FALSE) })

  # Variable name - figure 2
  #var2 <- reactive({ varname(input$var2, long=FALSE) })
  var2 <- reactive({ varname(input$var1, long=FALSE) })

  # Season name - figure 1
  season1 <- reactive({ seasonname(input$seas1, long=FALSE) })

  # Season name - figure 2
  #season2 <- reactive({ seasonname(input$seas2, long=FALSE) })
  season2 <- reactive({ seasonname(input$seas1, long=FALSE) })
  
  source1 <- reactive({ 
    if(grepl("statistical|esd",tolower(input$src1))) {
      x <- "ESD"
    } else if(grepl("dynamical|rcm",tolower(input$src1))) {
      x <- "RCM"
    } else x <- input$src1
    return(x)
  })
    
  source2 <- reactive({ 
    if(grepl("statistical|esd",tolower(input$src2))) {
      x <- "ESD"
    } else if(grepl("dynamical|rcm",tolower(input$src2))) {
      x <- "RCM"
    } else x <- input$src2
    return(x)
  })
  
  # Emission scenario - figure 1
  scenario1 <- reactive({ scenarioname(input$sce1, long=FALSE) })
  
  # Emission scenario - figure 2
  scenario2 <- reactive({ scenarioname(input$sce2, long=FALSE) })
  
  ensemble1 <- reactive({ 
    if(source1()=="ESD") gcmnames[[var1()]][[scenario1()]] else 
      paste(rcmnames[[var1()]][[scenario1()]]$gcm, 
            rcmnames[[var1()]][[scenario1()]]$rcm, sep=" ")
  })
  
  ensemble2 <- reactive({ 
    if(source2()=="ESD") gcmnames[[var2()]][[scenario2()]] else 
      paste(rcmnames[[var2()]][[scenario2()]]$gcm,
            rcmnames[[var2()]][[scenario2()]]$rcm, sep=" ")
  })

  label1 <- reactive({
    label <- paste0("Ensemble A  (",length(input$gcms1)," simulations)")
  })
  
  label2 <- reactive({
    label <- paste0("Ensemble B  (",length(input$gcms2)," simulations)")
  })

  ## Click on the map marker - this updates the selected station location
  observeEvent(input$map_marker_click,{
    print("observeEvent() - click")
    Y <- locs[[var1()]]#locs[[region1()]][[var1()]]
    event <- input$map_marker_click
    selected <- which(Y$station_id==event$id)
    print(event$id)
    print(selected)
    updateSelectInput(session, "location", label = "Location", 
                      choices = Y$label, 
                      selected = Y$label[[selected]])
    print('---click---')
  })

  # Date range - figure 1
  it1 <- reactive({
    return(datelist[[input$dates1]])
  })

  # Date range - figure 2
  it2 <- reactive({
    return(datelist[[input$dates2]])
  })

  # Longitude and latitude range for the two maps
  xlim1 <- reactive({ maprange("nordic")$lon })#maprange(tolower(region1()))$lon })
  ylim1 <- reactive({ maprange("nordic")$lat })#maprange(tolower(region1()))$lat })
  xlim2 <- reactive({ maprange("nordic")$lon })#maprange(tolower(region1()))$lon })
  ylim2 <- reactive({ maprange("nordic")$lat })#maprange(tolower(region1()))$lat })

  # Update ensemble when changing variable or scenario  - figure 1
  observe({
    choices <- ensemble1()
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms1",
                       label = "Climate models",
                       choices = choices,
                       selected = selected)
  })
  
  # Update ensemble when changing variable or scenario  - figure 2
  observe({
    choices <- ensemble2()#gcmnames[[var2()]][[scenario2()]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms2",
                             label = "Climate models",
                             choices = choices,
                             selected = selected)
  })

  # Select only one simulation from each GCM  - figure 1
  observeEvent(input$gcmone1, {
    choices <- ensemble1()#gcmnames[[var1()]][[scenario1()]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })
  
  # Select only one simulation from each GCM  - figure 2
  observeEvent(input$gcmone2, {
    choices <- ensemble2()#gcmnames[[var2()]][[scenario2()]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Select all simulations - figure 1
  observeEvent(input$gcmall1, {
    choices <- ensemble1()#gcmnames[[var1()]][[scenario1()]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })
  
  # Select all simulations  - figure 2
  observeEvent(input$gcmall2, {
    choices <- ensemble2()#gcmnames[[var2()]][[scenario2()]]
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations  - figure 1
  observeEvent(input$gcmdeselect1, {
    choices <- ensemble1()#gcmnames[[var1()]][[scenario1()]]
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcms1", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations - figure 2
  observeEvent(input$gcmdeselect2, {
    choices <- ensemble2()#gcmnames[[var2()]][[scenario2()]]
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices,
                             selected = selected)
  })

  # Set default date range to full length (1950-2100) for trend maps  - figure 1
  observe({
    if(input$fun1=="trend") {
      updateSelectInput(session, "dates1",
                  label="Years",
                  choices=names(datelist),
                  selected=names(datelist)[[1]])
    }
  })

  # Set default date range to full length (1950-2100) for trend maps  - figure 2
  observe({
    if(input$fun2=="trend") {
      updateSelectInput(session, "dates2",
                        label="Years",
                        choices=names(datelist),
                        selected=names(datelist)[[1]])
    }
  })

  # Update location choices when variable or region is changed - location 1
  observe({
    choices1 <- locs[[var1()]]$label#locs[[region1()]][[var1()]]$label
    choices2 <- locs[[var2()]]$label#locs[[region2()]][[var2()]]$label
    choices <- choices1[choices1 %in% choices2]
    if(input$location!="-") {
      if(input$location %in% choices) {
        is <- which(input$location==choices)
      } else {
        is <- grep(cleanstr(input$location, "[0-9]"),
                   cleanstr(choices, "[0-9]"))
        if(length(is)>1) {
          is <- is[[1]]
        } else if(length(is)==0) {
          is <- 1
        }
      }
      sel <- choices[is]
    }
    updateSelectInput(session, "location",
                      choices = choices, # update choices
                      selected = sel) # remove selection
  })

  # Update range of color scale in map - figure 1
  observe({
    x <- sliderange(param=var1(), FUN=input$fun1)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrange1", label="Range of colorscale",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })

  # Update range of color scale in map - figure 2
  observe({
      x <- sliderange(param=var2(), FUN=input$fun2)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrange2", label="Range of colorscale",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })

  ## Path for finding file 1
  path1 <- reactive({
    if(source1()=="ESD") {
      path.esd 
    } else if(source1()=="RCM") {
      path.rcm
    } else path.data
  })

  ## Path for finding file 2
  path2 <- reactive({
    if(source2()=="ESD") {
      path.esd 
    } else if(source2()=="RCM") {
      path.rcm
    } else path.data
  })
      
  # Load data for figure 1
  zload_pc <- reactive({
    Z <- zload(path=path1(), src=source1(), param=var1(), season=season1(), 
               scenario=scenario1(), FUN=input$fun1)
    attr(Z, "season") <- input$seas1
    attr(Z, "scenario") <- scenario1()
    return(Z)
  })

  # Load data for figure 2
  zload_pc_2 <- reactive({
    Z <- zload(path=path2(), src=source2(), param=var2(), season=season2(), 
               scenario=scenario2(), FUN=input$fun2)
    attr(Z, "season") <- input$seas2
    attr(Z, "scenario") <- scenario2()
    return(Z)
  })

  # Load data and transform to station data - figure 1
  zload_station <- reactive({
    Z <- zload(path=path1(), src=source1(), param=var1(), season=season1(), 
               scenario=scenario1())
    attr(Z, "season") <- input$seas1
    attr(Z, "scenario") <- scenario1()
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path=path1(), param=var1(), src=source1(), 
                    scenario=scenario1(), season=season1(), 
                    FUNX="max")
      ymin <- zload(path=path1(), param=var1(), src=source1(), 
                    scenario=scenario1(), season=season1(), 
                    FUNX="min")
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
    return(y)
  })

  # Load data and transform to station data - figure 2
  zload_station_2 <- reactive({
    Z <- zload(path=path2(), src=source2(), param=var2(), season=season2(), 
               scenario=scenario2())
    attr(Z, "season") <- input$seas2
    attr(Z, "scenario") <- scenario2()
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path=path2(), param=var2(), src=source2(), 
                    scenario=scenario2(), season=season2(), 
                    FUNX="max")
      ymin <- zload(path=path2(), param=var2(), src=source2(), 
                    scenario=scenario2(), season=season2(), 
                    FUNX="min")
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
    return(y)
  })

  pval1 <- reactive({
    if(source1()=="ESD") {
      p <- T4[[var1()]][[scenario1()]][[season1()]]
    } else {
      p <- zload(path=path1(), param=var1(), src=source1(), 
                 scenario=scenario1(), season=season1(), 
                 FUN="trend")
    }
    return(p)
  })

  pval2 <- reactive({
    if(source2()=="ESD") {
      p <- T4[[var2()]][[scenario2()]][[season2()]]
    } else {
      p <- zload(path=path2(), param=var2(), src=source2(), 
                 scenario=scenario2(), season=season2(), 
                 FUN="trend")
    }
    return(p)
  })
  
  # Change default model simulations when changing variable and scenario - figure 1
  im1 <- reactive({
    if(source1()=="ESD") {
      choices <- ensemble1()
      im <- choices %in% input$gcms1
    } else {
      im <- rep(TRUE, length(input$gcms1))
    }
    return(im)
  })

  # Change default model simulations when changing variable and scenario - figure 2
  im2 <- reactive({
    if(source2()=="ESD") {
      choices <- ensemble2()
      im <- choices %in% input$gcms2
    } else {
      im <- rep(TRUE, length(input$gcms2))
    }
    return(im)
  })

  # Caption for figure 1
  output$main1 <- renderText(paste0("<b>", first2upper(input$fun1), "</b>",
                                    " of the ",switch(source1(),
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "),
                                    "<b>", input$var1, "</b>"," for the ",
                                    "<b>", input$seas1, "</b>", " season ",
                                    "in the period ", "<b>", input$dates1, "</b>",
                                    ", assuming the ",
                                    "<b>", input$sce1, "</b>",
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms1), " simulations)")))

  # Caption for figure 2
  output$main2 <- renderText(paste0("<b>", first2upper(input$fun2), "</b>",
                                    " of the ",switch(source2(),
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "),
                                    "<b>", input$var1, "</b>"," for the ",
                                    "<b>", input$seas1, "</b>", " season ",
                                    "in the period ", "<b>", input$dates2, "</b>",
                                    ", assuming the ",
                                    "<b>", input$sce2, "</b>",
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms2), " simulations)")))

  # Reactive plot function for saving - figure 1
  figure1 <- reactive({
    print('output$figure1')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(), verbose=FALSE,
               FUN=input$fun1, FUNX="mean", MET=source1(),
               colbar=list(breaks=pretty(input$valrange1, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim1(), ylim=ylim1(), cex=1.4,
               threshold = 0.9,
               trends=pval1())
  })

  # Reactive plot function for saving - figure 1
  figure2 <- reactive({
    print('output$figure2')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it1(), verbose=FALSE,
               FUN=input$fun2, FUNX="mean", MET=source2(),
               colbar=list(breaks=pretty(input$valrange2, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim2(), ylim=ylim2(), cex=1.4,
               threshold = 0.9, trends=pval2())
  })

  ## Show time series of two data sets at a location
  output$figts <- renderPlotly({
    print('output$fig12')
    z <- zload_station()
    z2 <- zload_station_2()
    stplot12(z, z2, is=input$location, it=c(1950,2100),#it1(),
             im1=im1(), im2=im2(), ylim=input$tsrange1,
             label1=label1(), label2=label2())
  })#, height=function(){0.6*session$clientData$output_figts_width})


  zoom <- reactive({
    2
  })
  
  output$map <- renderLeaflet({
    Y <- locs[[var1()]]#locs[[region1()]][[var1()]]
    selected <- which(Y$label==input$location)
    leaflet() %>% addTiles() %>%
      addCircleMarkers(lng = Y$longitude, # longitude
                       lat = Y$latitude, fill = TRUE, # latitude
                       label = Y$location, 
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = Y$location, popupOptions(keepInView = TRUE),
                       radius = 4, stroke=TRUE, weight = 1, color='black',
                       layerId = Y$station_id,
                       fillOpacity = 0.4) %>%
      addCircleMarkers(lng = Y$longitude[selected], lat = Y$latitude[selected], 
                       fill=TRUE, label=first2upper(Y$label[selected]),
                       radius=4, stroke=TRUE, weight=3, color='red',
                       layerId = Y$station_id[selected], 
                       fillOpacity = 1.0, fillColor = "red") %>%
      setView( lng = mean(range(Y$longitude)), 
               lat = mean(range(Y$latitude)),
               zoom=zoom() )
  })
  
  ## Show map of gridded temperature
  output$fig1 <- renderPlot({
    print('output$fig1')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(), oceanmask=input$landmask,
               FUN=input$fun1, FUNX="mean", MET=source1(),
               colbar=list(breaks=pretty(input$valrange1, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlim1(), ylim=ylim1(), verbose=FALSE,
               threshold = 0.9, trends=pval1())
  }, height=function(){1.0*session$clientData$output_fig1_width})

  ## Show map of gridded temperature
  output$fig2 <- renderPlot({
    print('output$fig2')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it1(), oceanmask=input$landmask,
               FUN=input$fun2, FUNX="mean", MET=source2(),
               colbar=list(breaks=pretty(input$valrange2, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlim2(), ylim=ylim2(), verbose=FALSE,
               threshold = 0.9, trends=pval2())
  }, height=function(){1.0*session$clientData$output_fig2_width})

  output$savefig1 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$src1, input$fun1, #input$reg1, 
            var1(), input$seas1, scenario1(), "ensmean",
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      figure1()
      dev.off()
    }
  )

  output$savefig2 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$src2, input$fun2, #input$reg1, 
            var2(), input$seas1, scenario2(), "ensmean",
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      figure2()
      dev.off()
    }
  )
  
})