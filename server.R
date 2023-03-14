
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)

  # Variable name - figure 1
  var1 <- reactive({ varname(input$var1, long=FALSE) })

  # Season name - figure 1
  season1 <- reactive({ seasonname(input$seas1, long=FALSE) })

  # Variable name - figure 2
  #var2 <- reactive({ varname(input$var2, long=FALSE) })
  var2 <- reactive({ varname(input$var1, long=FALSE) })

  # Season name - figure 2
  #season2 <- reactive({ seasonname(input$seas2, long=FALSE) })
  season2 <- reactive({ seasonname(input$seas1, long=FALSE) })
  
  region1 <- reactive({ 
    if(grepl("ESD",toupper(input$src1))) {
      region <- cleanstr(gsub("ESD","",input$src1))
    } else {
      region <- "Nordic"
    }
    return(region)
  })

  region2 <- reactive({ 
    if(grepl("ESD",toupper(input$src2))) {
      region <- cleanstr(gsub("ESD","",input$src2))
    } else {
      region <- "Nordic"
    }
    return(region)
  })
  
  source1 <- reactive({ 
    if(grepl("ESD",toupper(input$src1))) {
      x <- "ESD"
    } else x <- input$src1
    return(x)
  })
    
  source2 <- reactive({ 
    if(grepl("ESD",toupper(input$src2))) {
      x <- "ESD"
    } else x <- input$src2
    return(x)
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
  xlim1 <- reactive({ maprange(tolower(region1()))$lon })
  ylim1 <- reactive({ maprange(tolower(region1()))$lat })
  xlim2 <- reactive({ maprange(tolower(region1()))$lon })
  ylim2 <- reactive({ maprange(tolower(region1()))$lat })

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
    choices1 <- locs[[region1()]][[var1()]]$label
    choices2 <- locs[[region2()]][[var2()]]$label
    choices <- choices1[choices1 %in% choices2]
    if(input$location1!="-") {
      if(input$location1 %in% choices) {
        is <- which(input$location1==choices)
      } else {
        is <- grep(cleanstr(input$location1, "[0-9]"),
                   cleanstr(choices, "[0-9]"))
        if(length(is)>1) {
          is <- is[[1]]
        } else if(length(is)==0) {
          is <- 1
        }
      }
      sel <- choices[is]
    }
    updateSelectInput(session, "location1",
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


  # Load data for figure 1
  zload_pc <- reactive({
    Z <- zload(path="data", type="field", src=source1(),
          region=region1(), param=var1(), season=season1(),
          scenario=input$sce1, FUN=input$fun1,
          FUNX=NULL, verbose=FALSE)
    return(Z)
  })

  # Load data for figure 2
  zload_pc_2 <- reactive({
    Z <- zload(path="data", type="field", src=source2(),
          region=region2(), param=var2(), season=season2(),
          scenario=input$sce2, FUN=input$fun2,
          FUNX=NULL, verbose=FALSE)
    return(Z)
  })

  # Load data and transform to station data - figure 1
  zload_station <- reactive({
    Z <- zload(path="data", type="field", src=source1(),
               region=region1(), param=var1(), season=season1(),
               scenario=input$sce1, FUN="mean",
               FUNX=NULL, verbose=FALSE)
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path="data", type="field", src=source1(),
                    region=region1(), param=var1(), season=season1(),
                    scenario=input$sce1, FUN="mean",
                    FUNX="max", verbose=FALSE)
      ymin <- zload(path="data", type="field", src=source1(),
                    region=region1(), param=var1(), season=season1(),
                    scenario=input$sce1, FUN="mean",
                    FUNX="min", verbose=FALSE)
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
    return(y)
  })

  # Load data and transform to station data - figure 2
  zload_station_2 <- reactive({
    Z <- zload(path="data", type="field", src=source2(),
               region=region2(), param=var2(), season=season2(),
               scenario=input$sce2, FUN="mean",
               FUNX=NULL, verbose=FALSE)
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path="data", type="field", src=source2(),
                 region=region2(), param=var2(), season=season2(),
                 scenario=input$sce2, FUN="mean",
                 FUNX="max", verbose=FALSE)
      ymin <- zload(path="data", type="field", src=source2(),
                    region=region2(), param=var2(), season=season2(),
                    scenario=input$sce2, FUN="mean",
                    FUNX="min", verbose=FALSE)
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
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
                                    " of the ",switch(source1(),
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "),
                                    "<b>", input$var1, "</b>"," for the ",
                                    "<b>", input$seas1, "</b>", " season ",
                                    "in the period ", "<b>", input$dates1, "</b>",
                                    ", assuming emission scenario ",
                                    "<b>", input$sce1, "</b>",
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms1), " simulations)")))

  # Caption for figure 2
  output$main2 <- renderText(paste0("<b>", input$fun2, "</b>",
                                    " of the ",switch(source2(),
                                                      "ESD" = "statistically downscaled (<b>ESD</b>) ",
                                                      "RCM" = "dynamically downscaled (<b>RCM</b>) ",
                                                      "GCM" = "global climate model output (GCM) of "),
                                    "<b>", input$var1, "</b>"," for the ",
                                    "<b>", input$seas1, "</b>", " season ",
                                    "in the period ", "<b>", input$dates2, "</b>",
                                    ", assuming emission scenario ",
                                    "<b>", input$sce2, "</b>",
                                    paste0(" (ensemble mean of ",
                                           length(input$gcms2), " simulations)")))

  # Reactive plot function for saving - figure 1
  figure1 <- reactive({
    print('output$figure1')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(), verbose=FALSE,
               FUN=input$fun1, FUNX="mean", MET=source1(),#input$funx1,
               #show.field=input$field, show.station=input$stations,
               colbar=list(breaks=pretty(input$valrange1, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim1(), ylim=ylim1(),
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[region1()]][[var1()]][[input$sce1]][[season1()]])
  })

  # Reactive plot function for saving - figure 1
  figure2 <- reactive({
    print('output$figure2')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it1(), verbose=FALSE,
               FUN=input$fun2, FUNX="mean", MET=source2(),#input$funx1,
               #show.field=input$field, show.station=input$stations,
               colbar=list(breaks=pretty(input$valrange2, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim2(), ylim=ylim2(),
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[region1()]][[var1()]][[input$sce2]][[season1()]])
  })

  ## Show time series of two data sets at a location
  output$figts <- renderPlot({
    print('output$fig12')
    z <- zload_station()
    z2 <- zload_station_2()
    stplot12(z, z2, is=input$location1, it=c(1950,2100),#it1(),
             im1=im1(), im2=im2(), ylim=input$tsrange1)
  }, height=function(){0.6*session$clientData$output_figts_width})


  ## Show location of selected station on map
  output$mapts <- renderPlot({
    print('output$fig12')
    z <- zload_station()
    stmap(z, is=input$location1, xlim=xlim1(), ylim=ylim1())
  }, height=function(){1.1*session$clientData$output_mapts_width})

  ## Show map of gridded temperature
  output$fig1 <- renderPlot({
    print('output$fig1')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(), oceanmask=input$landmask,
               FUN=input$fun1, FUNX="mean", MET=source1(),#input$funx1,
               #show.field=input$field, show.station=input$stations,
               colbar=list(breaks=pretty(input$valrange1, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim1(), ylim=ylim1(), verbose=FALSE,
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[region1()]][[var1()]][[input$sce1]][[season1()]])
  }, height=function(){1.0*session$clientData$output_fig1_width})

  ## Show map of gridded temperature
  output$fig2 <- renderPlot({
    print('output$fig2')
    z <- zload_pc_2()
    mapgridded(z, im=im2(), it=it1(), oceanmask=input$landmask,
               FUN=input$fun2, FUNX="mean", MET=source2(),#input$funx1,
               #show.field=input$field, show.station=input$stations,
               colbar=list(breaks=pretty(input$valrange2, n=22)),
               show.robustness = input$robustness_map,
               xlim=xlim2(), ylim=ylim2(), verbose=FALSE,
               threshold = 0.9,#input$threshold_map/100,
               trends=T4[[region2()]][[var2()]][[input$sce2]][[season2()]])
  }, height=function(){1.0*session$clientData$output_fig2_width})

  output$savefig1 <- downloadHandler(
    filename = function() {
      paste('downscaled', input$src1, input$fun1, input$reg1, var1(),
            input$seas1, input$sce1, "ensemblemean",#input$funx1,
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
      paste('downscaled', input$src2, input$fun2, input$reg1, var2(),
            input$seas1, input$sce2, "ensemblemean",#input$funx1,
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      figure2()
      dev.off()
    }
  )

})