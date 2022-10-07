
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)
  
  var1 <- reactive({ varname(input$var1, long=FALSE) })
  
  season1 <- reactive({ seasonname(input$it1, long=FALSE) })
  
  var2 <- reactive({ varname(input$var2, long=FALSE) })
  
  season2 <- reactive({ seasonname(input$it2, long=FALSE) })

  observe({
    choices <- gcmnames[[var1()]][[input$sce1]]
    selected <- choices#input$gcms[input$gcms %in% choices]
    updateCheckboxGroupInput(session, inputId = "gcms",
                       label = "Climate models",
                       choices = choices,
                       selected = selected)
  })
  

  observeEvent(input$gcmone, {
    choices <- gcmnames[[var1()]][[input$sce1]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices, 
                             selected = selected)
  })
  
  observeEvent(input$gcmall, {
    choices <- gcmnames[[var1()]][[input$sce1]]
    #gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices#[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices, 
                             selected = selected)
  })

  observeEvent(input$gcmone2, {
    choices <- gcmnames[[var2()]][[input$sce2]]
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms2", choices = choices, 
                             selected = selected)
  })
  
  observeEvent(input$gcmall2, {
    choices <- gcmnames[[var2()]][[input$sce2]]
    #gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices#[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcms", choices = choices, 
                             selected = selected)
  })
  
  observe({
    val <- FALSE
    if(input$fun1=="trend" & input$dates1=="1950-2100") {
      val <- input$robustness_map
    }
    updateCheckboxInput(session, "robustness_map",
                        label = "show trend robustness for the period 1950-2100 (same sign in 90% of ensemble members)",
                        value = val)
  })
  
  observe({
    val <- FALSE
    if(input$fun2=="trend" & input$dates2=="1950-2100") {
      val <- input$robustness_map
    }
    updateCheckboxInput(session, "robustness_map2",
                        label = "show trend robustness for the period 1950-2100 (same sign in 90% of ensemble members)",
                        value = val)
  })
  
  # run every time data is updated
  observe({
    if(input$location2 %in% locs[[input$reg1]][[var1()]]$label) {
      sel <- input$location2
    } else {
      is <- grep(cleanstr(input$location2, "[0-9]"), 
                 cleanstr(locs[[input$reg1]][[var1()]]$label, "[0-9]"))
      if(length(is)>1) {
        is <- is[[1]] 
      } else if(length(is)==0) {
        is <- 1
      }
      sel <- locs[[input$reg1]][[var1()]]$label[[is]]
    }
    updateSelectInput(session, "location2",
                      choices = locs[[input$reg1]][[var1()]]$label, # update choices
                      selected = sel) # remove selection
  })

  observe({
    x <- sliderange(param=var1(), FUN=input$fun1)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "maprange", label="Range of color scale",
                      min=min(x$minmax), max=max(x$minmax), 
                      step=xstep, value=x$x)
  })
  
  observe({
    x <- sliderange(param=var2(), FUN=input$fun2)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "maprange2", label="Range of color scale",
                      min=min(x$minmax), max=max(x$minmax), 
                      step=xstep, value=x$x)
  })

  observe({
    x <- sliderange(param=var1(), FUN="mean")
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "tsrange", label="range of color scale",
                      min=min(x$minmax), max=max(x$minmax), 
                      step=xstep, value=x$x)
  })
  
  zload_pc <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg1, var1(),
                         season1(), input$sce1))
    return(Z)
  })
  
  zload_pc_2 <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg2, var2(),
                         season2(), input$sce2))
    return(Z)
  })
  
  zload_station <- reactive({
    Z <- zload(pattern=c("dse.kss", input$reg1, var1(),
                         season1(), input$sce1))
    y <- as.station(Z)
    attr(y, "variable") <- attr(Z, "variable")
    attr(y, "longname") <- attr(Z, "longname")
    attr(y, "unit") <- attr(Z, "unit")
    return(y)
  })

  zload_station_2 <- reactive({
    Z <- zload_pc_2()
    y <- as.station(Z)
    attr(y, "variable") <- attr(Z, "variable")
    attr(y, "longname") <- attr(Z, "longname")
    attr(y, "unit") <- attr(Z, "unit")
    return(y)
  })
  
  im1 <- reactive({
    choices <- gcmnames[[var1()]][[input$sce1]]
    im <- choices %in% input$gcms
    return(im)
  })
  
  im2 <- reactive({
    choices <- gcmnames[[var2()]][[input$sce2]]
    im <- choices %in% input$gcms2
    return(im)
  })
  
  it1 <- reactive({
    return(datelist[[input$dates1]])
  })
  
  it2 <- reactive({
    return(datelist[[input$dates2]])
  })
  
  maps <- reactive({
    print('output$maps')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(),
               FUN=input$fun1, FUNX="mean",#input$funx1, 
               show.field=input$field1, show.station=input$stations1,
               colbar=list(breaks=pretty(input$maprange, n=22)),
               show.robustness = input$robustness_map,
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[input$reg1]][[var1()]][[input$sce1]][[season1()]])
  })
  
  maps2 <- reactive({
    print('output$maps')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it2(),
               FUN=input$fun2, FUNX="mean",#input$funx2, 
               show.field=input$field2, show.station=input$stations2,
               colbar=list(breaks=pretty(input$maprange2, n=22)),
               show.robustness = input$robustness_map2,
               threshold = 0.9,#input$threshold_map2/100,
               trends=T4[[input$reg2]][[var2()]][[input$sce2]][[season2()]])
  })
  
  ## Show map of gridded temperature
  output$maps <- renderPlot({
    print('output$maps')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(),
               FUN=input$fun1, FUNX="mean",#input$funx1, 
               show.field=input$field1, show.station=input$stations1,
               colbar=list(breaks=pretty(input$maprange, n=22)),
               show.robustness = input$robustness_map,
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[input$reg1]][[var1()]][[input$sce1]][[season1()]])
  }, height=function(){1.0*session$clientData$output_maps_width})

  ## Show map of gridded temperature
  output$maps2 <- renderPlot({
    print('output$maps2')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it2(),
               FUN=input$fun2, FUNX="mean",#input$funx2, 
               show.field=input$field2, show.station=input$stations2,
               colbar=list(breaks=pretty(input$maprange2, n=22)),
               show.robustness = input$robustness_map2,
               threshold = 0.9,#input$threshold_map2/100,
               trends=T4[[input$reg2]][[var2()]][[input$sce2]][[season2()]])
  }, height=function(){1.0*session$clientData$output_maps2_width})
  
  output$main1 <- renderText(paste0(input$fun1, " of the statistically downscaled ", 
                                    input$var1, " for the ", input$it1, " season ",
                                    "in the period ", input$dates1, 
                                    ", assuming emission scenario ", input$sce1, 
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms), " simulations)")))
  
  output$main2 <- renderText(paste0(input$fun2, " of the statistically downscaled ", 
                                    input$var2, " for the ", input$it2, " season ",
                                    "in the period ", input$dates2, 
                                    ", assuming emission scenario ", input$sce2, 
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms), " simulations)")))
  
  ## Show time series for individual stations
  plotst <- reactive({
    z <- zload_station()
    stplot(z, is=input$location1, it=input$dates1, 
           im=im1(), ylim=input$tsrange1)
  })

  plotst2 <- reactive({
    z <- zload_station_2()
    stplot(z, is=input$location2, it=input$dates2, 
           im=im2(), ylim=input$tsrange2)
  })
  
  output$plot <- renderPlot({
    z <- zload_station()
    stplot(z, is=input$location2, it=input$dates2, im=im(),
           ylim=input$tsrange)
  }, height=function(){0.7*session$clientData$output_plot_width}) #600})
  
  ## Plot cross-validation histograms
  output$xval <- renderPlot({
    z <- zload_pc()
    crossval(z, im=im())
  }, height=function(){0.9*session$clientData$output_xval_width})
  

  output$savemaps <- downloadHandler(
    filename = function() {
      paste('downscaled', input$fun1, input$reg1, var1(),
            input$it1, input$sce1, "ensemblemean",#input$funx1, 
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      maps()
      dev.off()
    }
  )

  output$savemaps2 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$fun1, input$reg1, var1(),
            input$it1, input$sce1, "ensemblemean",#input$funx1, 
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      maps2()
      dev.off()
    }
  )    
  output$savest <- downloadHandler(
    filename = function() {
      paste('timeseries', input$reg1, var1(), input$it1, 
            input$sce1, cleanstr(input$location2), 'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      plotst()
      dev.off()
    }
  )
  
})