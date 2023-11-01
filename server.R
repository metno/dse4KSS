
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)

  # Location
  location <- reactive({ 
    x <- input$location
    stid <- cleanstr(gsub(".*.\\(", "", x))
    attr(x, "station_id") <- stid
    attr(x, "location") <- cleanstr(gsub(stid, "", x))
    i <- locs[[varA()]]$station_id == stid
    attr(x, "longitude") <- locs[[varA()]]$longitude[i]
    attr(x, "latitude") <- locs[[varA()]]$latitude[i]
    attr(x, "altitude") <- locs[[varA()]]$altitude[i]
    return(x)
  })
  
  # Variable name - ensemble A
  varA <- reactive({ varname(input$varA, long=FALSE) })

  # Variable name - ensemble B
  #varB <- reactive({ varname(input$varB, long=FALSE) })
  varB <- reactive({ varname(input$varA, long=FALSE) })

  # Season name - ensemble A
  seasonA <- reactive({ seasonname(input$seasA, long=FALSE) })

  # Season name - ensemble B
  #seasonB <- reactive({ seasonname(input$seasB, long=FALSE) })
  seasonB <- reactive({ seasonname(input$seasA, long=FALSE) })
  
  sourceA <- reactive({ 
    if(grepl("statistical|esd",tolower(input$srcA))) {
      x <- "ESD"
    } else if(grepl("dynamical|rcm",tolower(input$srcA))) {
      x <- "RCM"
    } else x <- input$srcA
    return(x)
  })
    
  sourceB <- reactive({ 
    if(grepl("statistical|esd",tolower(input$srcB))) {
      x <- "ESD"
    } else if(grepl("dynamical|rcm",tolower(input$srcB))) {
      x <- "RCM"
    } else x <- input$srcB
    return(x)
  })
  
  # Emission scenario - ensemble A
  scenarioA <- reactive({ scenarioname(input$sceA, long=FALSE) })
  
  # Emission scenario - ensemble B
  scenarioB <- reactive({ scenarioname(input$sceB, long=FALSE) })
  
  # Ensemble members for source, variable and scenario A 
  ensembleA <- reactive({ 
    if(sourceA()=="ESD") gcmnames[[varA()]][[scenarioA()]] else 
      paste(rcmnames[[varA()]][[scenarioA()]]$gcm, 
            rcmnames[[varA()]][[scenarioA()]]$rcm, sep=" ")
  })
  
  # Ensemble members for source, variable and scenario B 
  ensembleB <- reactive({ 
    if(sourceB()=="ESD") gcmnames[[varB()]][[scenarioB()]] else 
      paste(rcmnames[[varB()]][[scenarioB()]]$gcm,
            rcmnames[[varB()]][[scenarioB()]]$rcm, sep=" ")
  })

  maintitle <- reactive({
    paste0("Seasonal (",toupper(seasonA()),") mean ",input$varA)
  })
  
  # Label for ensemble A
  labelA <- reactive({
    label <- paste0("A: ", input$srcA, "  ",
                    scenarioname(input$sceA, long=FALSE, punct=TRUE),
                    " (", length(input$gcmsA)," simulations)")
  })
  
  labelA2lines <- reactive({
    label <- paste0("A: ", input$srcA, "\n    ",
                    scenarioname(input$sceA, long=FALSE, punct=TRUE),
                    " (", length(input$gcmsA)," simulations)")
  })
  
  # Label for ensemble B
  labelB <- reactive({
    label <- paste0("B: ", input$srcB, "  ",
                    scenarioname(input$sceB, long=FALSE, punct=TRUE),
                    " (", length(input$gcmsB)," simulations)")
  })
  
  labelB2lines <- reactive({
    label <- paste0("B: ", input$srcB, "\n    ",
                    scenarioname(input$sceB, long=FALSE, punct=TRUE),
                    " (", length(input$gcmsB)," simulations)")
  })

  ## Click on the map marker - this updates the selected station location
  observeEvent(input$map_marker_click,{
    print("observeEvent() - click")
    Y <- locs[[varA()]]#locs[[regionA()]][[varA()]]
    event <- input$map_marker_click
    selected <- which(Y$station_id==event$id)
    print(event$id)
    print(selected)
    updateSelectInput(session, "location", label = "Location", 
                      choices = Y$label, 
                      selected = Y$label[[selected]])
    print('---click---')
  })

  # Date range - map A
  itA <- reactive({
    return(datelist[[input$datesA]])
  })

  # Date range - map B
  itB <- reactive({
    return(datelist[[input$datesB]])
  })

  # Longitude and latitude range for the two maps
  xlimA <- reactive({ maprange("nordic")$lon })
  ylimA <- reactive({ maprange("nordic")$lat })
  xlimB <- reactive({ maprange("nordic")$lon })
  ylimB <- reactive({ maprange("nordic")$lat })

  # Update ensemble A when changing variable or scenario
  observe({
    choices <- ensembleA()
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcmsA",
                       label = "Climate models",
                       choices = choices,
                       selected = selected)
  })
  
  # Update ensemble B when changing variable or scenario
  observe({
    choices <- ensembleB()
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcmsB",
                             label = "Climate models",
                             choices = choices,
                             selected = selected)
  })

  # Select only one simulation from each GCM - ensemble A
  observeEvent(input$gcmoneA, {
    choices <- ensembleA()
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcmsA", choices = choices,
                             selected = selected)
  })
  
  # Select only one simulation from each GCM - ensemble B
  observeEvent(input$gcmoneB, {
    choices <- ensembleB()
    gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
    selected <- choices[!duplicated(gcm)]
    updateCheckboxGroupInput(session, inputId = "gcmsB", choices = choices,
                             selected = selected)
  })
  
  # Select only one simulation from each GCM - ensemble A
  # observeEvent(input$gcmsameA, {
  #   choicesA <- ensembleA()
  #   choicesB <- ensembleB()
  #   if(sourceA()=="ESD") {
  #     gcmA <- sapply(choicesA, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
  #     ripA <- sapply(choicesA, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][2])
  #   }
  #   if(sourceB()=="RCM") {
  #     modelB <- sapply(choicesB, function(x) strsplit(x, split=" ")[[1]][1])
  #     gcmB <- sapply(modelB, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
  #     ripB <- sapply(modelB, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][2])
  #     cA <- tolower(cleanstr(choicesA))
  #     cB <- tolower(cleanstr(choicesB))
  #     im_A <- sapply(cA, function(x) sum(grepl(x, cB)))
  #   }
  #   gcm <- sapply(choices, function(x) strsplit(x, split=".r[0-9]{1,3}i[0-9]{1,3}")[[1]][1])
  #   selected <- choices[!duplicated(gcm)]
  #   updateCheckboxGroupInput(session, inputId = "gcmsA", choices = choices,
  #                            selected = selected)
  # })

  # Select all simulations - ensemble A
  observeEvent(input$gcmallA, {
    choices <- ensembleA()
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcmsA", choices = choices,
                             selected = selected)
  })
  
  # Select all simulations  - ensemble B
  observeEvent(input$gcmallB, {
    choices <- ensembleB()
    selected <- choices
    updateCheckboxGroupInput(session, inputId = "gcmsB", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations  - ensemble A
  observeEvent(input$gcmdeselectA, {
    choices <- ensembleA()
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcmsA", choices = choices,
                             selected = selected)
  })

  # Deselect all simulations - ensemble B
  observeEvent(input$gcmdeselectB, {
    choices <- ensembleB()
    selected <- NULL
    updateCheckboxGroupInput(session, inputId = "gcmsB", choices = choices,
                             selected = selected)
  })

  # Set default date range to full length (1950-2100) for trend maps - map A
  observe({
    if(input$funA=="trend") {
      updateSelectInput(session, "datesA",
                  label="A: Years",
                  choices=names(datelist),
                  selected=names(datelist)[[1]])
    }
  })

  # Set default date range to full length (1950-2100) for trend maps - map B
  observe({
    if(input$funB=="trend") {
      updateSelectInput(session, "datesB",
                        label="B: Years",
                        choices=names(datelist),
                        selected=names(datelist)[[1]])
    }
  })

  # Update location choices when variable or region is changed
  observe({
    print("Update location choices when variable or region is changed.")
    choicesA <- locs[[varB()]]$label#locs[[regionA()]][[varB()]]$label
    choicesB <- locs[[varB()]]$label#locs[[regionB()]][[varB()]]$label
    choices <- choicesA[choicesA %in% choicesB]
    if(location()!="-") {
      if(location() %in% choices) {
        is <- which(location()==choices)
      } else {
        is <- grep(cleanstr(location(), "[0-9]"),
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

  # Update range of color scale in map A
  observe({
    x <- sliderange(param=varA(), FUN=input$funA)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrangeA", 
                      label="A: Range of colorscale",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })

  # Update range of color scale in map B
  observe({
      x <- sliderange(param=varB(), FUN=input$funB)
    xstep <- diff(pretty(range(x$minmax), n=22))[1]
    updateSliderInput(session, "valrangeB", 
                      label="B: Range of colorscale",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })
  
  # Update y-axis range in time series
  observe({
    x <- sliderange(param=varA(), FUN="mean")
    if(input$normalize_ts) {
      x$minmax <- x$minmax - mean(x$minmax, na.rm=TRUE)
      x$x <- x$x - mean(x$x, na.rm=TRUE)
    }
    xstep <- 1/ceiling(diff(x$minmax)/100)
    updateSliderInput(session, "tsrange", 
                      label="Range of y-axis",
                      min=min(x$minmax), max=max(x$minmax),
                      step=xstep, value=x$x)
  })
  

  ## Path for finding file A (path.esd and path.rcm are defined in global.R)
  pathA <- reactive({
    if(sourceA()=="ESD") {
      path.esd 
    } else if(sourceA()=="RCM") {
      path.rcm
    } else path.data
  })

  ## Path for finding file B (path.esd and path.rcm are defined in global.R)
  pathB <- reactive({
    if(sourceB()=="ESD") {
      path.esd 
    } else if(sourceB()=="RCM") {
      path.rcm
    } else path.data
  })
      
  # Load data for ensemble A
  zload_A <- reactive({
    Z <- zload(path=pathA(), src=sourceA(), param=varA(), season=seasonA(), 
               scenario=scenarioA(), FUN=input$funA, im=imA())
    attr(Z, "season") <- seasonA()
    attr(Z, "scenario") <- scenarioA()
    return(Z)
  })

  # Load data for ensemble B
  zload_B <- reactive({
    Z <- zload(path=pathB(), src=sourceB(), param=varB(), season=seasonB(), 
               scenario=scenarioB(), FUN=input$funB, im=imB())
    attr(Z, "season") <- seasonB()
    attr(Z, "scenario") <- scenarioB()
    return(Z)
  })


  # Load data and transform to station data - ensemble A
  zload_station_A <- reactive({
    Z <- zload(path=pathA(), src=sourceA(), param=varA(), season=seasonA(), 
               scenario=scenarioA(), im=imA())
    attr(Z, "season") <- seasonA()
    attr(Z, "scenario") <- scenarioA()
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path=pathA(), param=varA(), src=sourceA(), 
                    scenario=scenarioA(), season=seasonA(), 
                    im=imA(), FUNX="max")
      ymin <- zload(path=pathA(), param=varA(), src=sourceA(), 
                    scenario=scenarioA(), season=seasonA(), 
                    im=imA(), FUNX="min")
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
    return(y)
  })

  # Load data and transform to station data - ensemble B
  zload_station_B <- reactive({
    Z <- zload(path=pathB(), src=sourceB(), param=varB(), season=seasonB(), 
               scenario=scenarioB(), im=imB())
    attr(Z, "season") <- seasonB()
    attr(Z, "scenario") <- scenarioB()
    if(inherits(Z,"dsensemble")) {
      y <- as.station(Z)
      attr(y, "variable") <- attr(Z, "variable")
      attr(y, "longname") <- attr(Z, "longname")
      attr(y, "unit") <- attr(Z, "unit")
    } else {
      y <- Z
      ymax <- zload(path=pathB(), param=varB(), src=sourceB(), 
                    scenario=scenarioB(), season=seasonB(), 
                    im=imB(), FUNX="max")
      ymin <- zload(path=pathB(), param=varB(), src=sourceB(), 
                    scenario=scenarioB(), season=seasonB(), 
                    im=imB(), FUNX="min")
      attr(y, "max") <- ymax
      attr(y, "min") <- ymin
    }
    return(y)
  })
  
  # Load data and transform to station data - ensemble A
  zload_station_allseasons_A <- reactive({
    Z <- lapply(c("djf","mam","jja","son"), 
                function(it) zload_station(path=pathA(), src=sourceA(), param=varA(), 
                                           season=it, im=imA(), scenario=scenarioA()))
    
    return(Z)
  })

  zload_station_allseasons_B <- reactive({
    Z <- lapply(c("djf","mam","jja","son"), 
                function(it) zload_station(path=pathB(), src=sourceB(), param=varB(), 
                                           season=it, im=imB(), scenario=scenarioB()))
    return(Z)
  })
  
  ## Load p-value of trends for map A
  pvalA <- reactive({
    if(sourceA()=="ESD") {
      p <- T4[[varA()]][[scenarioA()]][[seasonA()]]
    } else {
      p <- zload(path=pathA(), param=varA(), src=sourceA(), 
                 scenario=scenarioA(), season=seasonA(), 
                 im=imA(), FUN="postrend")#"trend")
    }
    return(p)
  })

  ## Load p-value of trends for map B
  pvalB <- reactive({
    if(sourceB()=="ESD") {
      p <- T4[[varB()]][[scenarioB()]][[seasonB()]]
    } else {
      p <- zload(path=pathB(), param=varB(), src=sourceB(), 
                 scenario=scenarioB(), season=seasonB(), 
                 im=imB(), FUN="postrend")#"trend")
    }
    return(p)
  })
  
  # Change default model simulations when changing variable and scenario - ensemble A
  imA <- reactive({
    if(sourceA()=="ESD") {
      choices <- ensembleA()
      im <- choices %in% input$gcmsA
    } else {
      im <- rep(TRUE, length(input$gcmsA))
    }
    return(im)
  })

  # Change default model simulations when changing variable and scenario - ensemble B
  imB <- reactive({
    if(sourceB()=="ESD") {
      choices <- ensembleB()
      im <- choices %in% input$gcmsB
    } else {
      im <- rep(TRUE, length(input$gcmsB))
    }
    return(im)
  })

  ## Intro text
  output$IntroText  <- renderText({
    paste("This is a tool for presenting and comparing projections from various climate models and ensembles. ",
          "The app is based on global climate model (GCM) data that have been downscaled to a finer spatial resolution using one of two approaches: ",
          "dynamical downscaling using regional climate models (RCM), or ",
          "emprical-statistical downscaling (ESD) in which a statistical relationship is established between large-scale climate patterns and the local response.",
          "<br><br>",
          "Two model ensembles, selected in the sideboard on the left hand side, are displayed alongside each other. ",
          "The downscaled data are presented as <i>'Time series'</i> showing variations of a selected variable at a specific location, and ",
          "<i>'Maps'</i> displaying the mean value or trend of the selected data for the whole region in a map."
    )
  })
  
  ## Data and methodology text
  output$DataText  <- renderText({
    paste("Dynamically downscaled <a href=https://www.euro-cordex.net/060376/index.php.en> EURO-CORDEX</a>... <br><br>",
          "The MetNo ESD ensemble was downscaled using a method developed by ",
          "<a href=https://doi.org/10.3402/tellusa.v67.28326>Benestad et al. 2015</a> at the Norwegian Meteorological Institute (MetNo). ",
          "The method was applied to observational data from stations... reference to observational data. <br><br>"
    )
  })
  
  ## How to use the app text
  output$HowtoText  <- renderText({
    paste("In the control panel on the left hand side, you can chose what to show in the app. ",
          "In <i>'Season and variable'</i>, you set the climatological variable (e.g., temperature or precipitation) and season to focus on. ",
          "In <i>'Ensemble A'</i> and <i>'Ensemble B'</i>, you define the two climate model ensembles to be displayed in the app. ",
          "Here, you can select a <i>data source</i>, chosing between the <a href=https://www.euro-cordex.net/060376/index.php.en> EURO-CORDEX</a> ",
          "which is an ensemble of dynamically downscaled climate simulations, ",
          "and the <a href=https://doi.org/10.3402/tellusa.v67.28326>MetNo ESD</a> ensemble which is downscaled using an ESD method developed by Benestad et. al 2015.<br><br>",
          "You can pick different <i>'Scenarios'</i> that describe the development of greenhouse gas concentrations in the atmosphere in the future. ",
          "These emission scenarios are known as Shared Socioeconomic Pathways (<a href=https://climate-adapt.eea.europa.eu/en/metadata/portals/shared-socioeconomic-pathways-ssps-database>SSPs</a>) ",
          "or Representative Concentration Pathways (<a href=https://pure.iiasa.ac.at/9505>RCPs</a>). ",
          "The SSPs were used as input for climate models in the sixth climate model intercomparison project <a href=https://pcmdi.llnl.gov/CMIP6/>CMIP6</a> ",
          "which provided a basis for the IPCCs sixth assessment report <a href=https://www.ipcc.ch/assessment-report/ar6/>AR6</a> ", 
          "and RCPs for the previous generation climate models and report, <a href=https://pcmdi.llnl.gov/mips/cmip5/>CMIP5</a> and ",
          "<a href=https://www.ipcc.ch/report/ar5/syr/>AR5</a>. <br><br>",
          "A subset of model simulations can be selected by clicking the boxes of the individual simulations or one of the buttons. ",
          "<i>'All models'</i> selects all models, <i>'One of each GCM'</i> picks only one simulation from each climate model, ",
          "<i>'Deselect all'</i> deselects all simulations, and <i>'Same as ensemble B/A'</i> picks the same model simulations for both ensembles. ",
          "As of now, model selection works for MetNo ESD but not for EURO-CORDEX. If different data sources are selected for ensemble A and B, ",
          "the MetNo ESD ensemble will be changed. Since the same GCM simulation can be included in the EURO-CORDEX ensemble several times (downscaled with different RCMs) ",
          "the repeated GCM simulations will be resampled in the MetNo ESD ensemble."
    )
  })

  # Reactive plot function for saving map A
  mapA <- reactive({
    print('mapA')
    z <- zload_A()
    mapgridded(z, it=itA(), oceanmask=input$landmask,
               FUN=input$funA, FUNX="mean", MET=sourceA(),
               colbar=list(breaks=pretty(input$valrangeA, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlimA(), ylim=ylimA(), verbose=FALSE,
               threshold = 0.9, trends=pvalA(),
               main=maintitle(), sub=labelA2lines())
  })

  # Reactive plot function for saving map B
  mapB <- reactive({
    print('mapB')
    z <- zload_B()
    mapgridded(z, it=itB(), oceanmask=input$landmask,
               FUN=input$funB, FUNX="mean", MET=sourceB(),
               colbar=list(breaks=pretty(input$valrangeB, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlimB(), ylim=ylimB(), verbose=FALSE,
               threshold = 0.9, trends=pvalB(),
               main=maintitle(), sub=labelB2lines())
  })

  ## Show time series of two data sets at a location
  output$timeseries <- renderPlotly({
    print('output$timeseries')
    #if(input$plottype_station=="seasonal cycle") {
    #  A <- zload_station_allseasons_A()
    #  B <- zload_station_allseasons_B()
    #  seasoncycles(A, B, is=location(), it=datelist,
    #         ylim=input$tsrange, main=paste(maintitle(), "in", location()),
    #         label1=labelA(), label2=labelB(),
    #         normalize=input$normalize_ts)
    #} else {
      A <- zload_station_A()
      B <- zload_station_B()
      stplot(A, B, is=location(), it=c(1950,2100),
             ylim=input$tsrange, main=paste(maintitle(), "in", location()),
             label1=labelA(), label2=labelB(),
             normalize=input$normalize_ts)
    #}
  })#, height=function(){0.6*session$clientData$output_timeseries_width})


  ## Leaflet map for showing and interactively selecting location
  output$map <- renderLeaflet({
    Y <- locs[[varA()]]
    selected <- which(Y$label==location())
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
               zoom=2 )
  })
  
  ## Show map of ensemble A
  output$mapA <- renderPlot({
    print('output$mapA')
    z <- zload_A()
    mapgridded(z, it=itA(), oceanmask=input$landmask,
               FUN=input$funA, FUNX="mean", MET=sourceA(),
               colbar=list(breaks=pretty(input$valrangeA, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlimA(), ylim=ylimA(), verbose=FALSE,
               threshold = 0.9, trends=pvalA(),
               main=maintitle(), sub=labelA2lines())
  }, height=function(){1.0*session$clientData$output_mapA_width})

  ## Show map of ensemble B
  output$mapB <- renderPlot({
    print('output$mapB')
    z <- zload_B()
    mapgridded(z, it=itB(), oceanmask=input$landmask,
               FUN=input$funB, FUNX="mean", MET=sourceB(),
               colbar=list(breaks=pretty(input$valrangeB, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
               xlim=xlimB(), ylim=ylimB(), verbose=FALSE,
               threshold = 0.9, trends=pvalB(),
               main=maintitle(), sub=labelB2lines())
  }, height=function(){1.0*session$clientData$output_mapB_width})

  ## Save map A
  output$savemapA <- downloadHandler(
    filename = function() {
      paste('downscaled', sourceA(), input$funA, #regionA(), 
            varA(), seasonA(), scenarioA(), "ensmean",
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      mapA()
      dev.off()
    }
  )

  ## Save map B
  output$savemapB <- downloadHandler(
    filename = function() {
      paste('downscaled', sourceB(), input$funB, #input$regA, 
            varB(), seasonB(), scenarioB(), "ensmean",
            'png', sep='.')
    },
    content = function(file) {
      png(file = file)
      mapB()
      dev.off()
    }
  )
  
})