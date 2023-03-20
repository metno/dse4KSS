
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

  ## SELECT STATION FROM MAP
  ## Click on the map marker - this updates the station location in the left panel
  # observeEvent(input$map_marker_click,{
  #   print("<04: observeEvent() - click")
  #   fnames <- updatefilenames()
  #   Y <- updatemetadata()
  #   event <- input$map_marker_click
  #   #print(paste('Updated ',input$location)); print(event$id)
  #   selected <- which(Y$station.id == event$id)
  #   
  #   #if (input$exclude== 'Selected') filter[selected] <- FALSE
  #   updateSelectInput(session,inputId = 'location',label=lab.location[as.numeric(input$lingo)],
  #                     choices=Y$location,selected = Y$location[selected])
  #   #print('---click---')
  # })
  # 
  
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
               xlim=xlim1(), ylim=ylim1(), cex=1.4,
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
               xlim=xlim2(), ylim=ylim2(), cex=1.4,
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

  ## Location of selected station on map but leaflet
  output$maptsf <- renderLeaflet({
    Y <- locs[[region1()]][[var1()]]
    selected <- which(Y$label==input$location1)
    leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(lng=Y$longitude, lat=Y$latitude, 
                 popup=as.list(first2upper(Y$label)),
                 label=as.list(first2upper(Y$label)),
                 labelOptions = labelOptions(direction = "right", textsize = "12px", opacity = 0.6),
                 radius = 3, stroke=TRUE, weight = 1, color='black',
                 layerId = Y$stid, fillOpacity = 0.4, 
                 fillColor="black") %>%
      addCircleMarkers(lng = Y$longitude[selected], lat = Y$latitude[selected], 
                       fill=TRUE, label=first2upper(Y$label[selected]),
                       radius=6, stroke=TRUE, weight=3, color='red',
                       layerId = Y$stid[selected], 
                       fillOpacity = 0.5, fillColor = "red")
  })
  
  ## Show map of gridded temperature
  output$fig1 <- renderPlot({
    print('output$fig1')
    z <- zload_pc()
    mapgridded(z, im=im1(), it=it1(), oceanmask=input$landmask,
               FUN=input$fun1, FUNX="mean", MET=source1(),#input$funx1,
               #show.field=input$field, show.station=input$stations,
               colbar=list(breaks=pretty(input$valrange1, n=22)),
               show.robustness = input$robustness_map, cex=1.4,
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
               show.robustness = input$robustness_map, cex=1.4,
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
  
  ## INTERACTIVE MAP
  # observe({
  #   print('<23: observe - marker click')
  #   
  #   mapid2 <- leafletProxy("mapid") %>% clearPopups()
  #   event <- input$map_marker_click
  #   #print('Data Explorer from map'); print(event$id)
  #   if (is.null(event))
  #     return()
  #   
  #   #print('Click --->'); print(event); print('<--- Click')
  #   isolate({
  #     mapid <- showMetaPopup(mapid2,stid=event$id,lat=event$lat, lng = event$lng,ci = as.numeric(input$ci))
  #   })
  #   
  #   #removeMarker("map",layerId = event$id)
  #   leafletProxy("mapid",data = event) %>%
  #     addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
  #   
  #   #selectedStid <- event$id
  # })
  # 
  
  ## The following are more general reactive expressions
  zoom <- reactive({
    print('<20: reactive - zoom()')
    zoomscale <- 4
    #zoomscale <- switch(input$src,
    #                    'metnod'=5,'ecad'=4,'eustance'=4,'Asia'=4,'Pacific'=4,'LatinAmerica'=4,'Africa'=3,'USA'=3,'Australia'=4,
    #                    'INAM'=6,'CLARIS'=6)
    return(zoomscale)
  })
  
  # observe({
  #   print('<23: observe - marker click')
  #   
  #   mapid2 <- leafletProxy("mapid") %>% clearPopups()
  #   event <- input$map_marker_click
  #   #print('Data Explorer from map'); print(event$id)
  #   if (is.null(event))
  #     return()
  #   
  #   #print('Click --->'); print(event); print('<--- Click')
  #   isolate({
  #     mapid <- showMetaPopup(mapid2, stid = event$id, lat = event$lat, 
  #                            lng = event$lng, ci = as.numeric(input$ci))
  #   })
  #   
  #   #removeMarker("map", layerId = event$id)
  #   leafletProxy("mapid", data = event) %>%
  #     addCircles(lng = event$lng, lat = event$lat, color = 'red', 
  #                layerId = 'selectID', weight = 12)
  #   
  #   #selectedStid <- event$id
  # })
  
  ## The map panel 
  output$leafletmap <- renderLeaflet({
     print('<24: output$map - render')
     Y <- locs[[region1()]][[var1()]]
     filter <- rep(TRUE, length(Y$longitude))
     radius <- 1#rep(input$rad,length(statistic[filter]))
     
     browser()
     map <- leaflet(width = 400, height = 200) %>% 
       addTiles() %>% 
       addMarkers(lng = -123.251,
                  lat = 49.263, 
                  popup = "You are here.")
     
     #map <- leaflet() %>%
     #  addTiles() %>%
     #  addCircleMarkers(lng = Y$longitude[filter], 
     #                   lat = Y$latitude[filter], fill = TRUE, 
     #                   label = paste0(first2upper(Y$location[filter])," (station id: ",Y$stid,")"),
     #                   labelOptions = labelOptions(direction = "right", textsize = "12px", opacity=0.6),
     #                   popup = Y$location[filter], popupOptions(keepInView=TRUE),
     #                   radius = radius, stroke = TRUE, weight = 1, color = 'black',
     #                   layerId = Y$stid[filter],
     #                   fillOpacity = 0.4, fillColor = "black") #%>%
      # setView(lat = xlim1(), lng = ylim1(), zoom=zoom())
     map
  })
})
  #   Y <- updatemetadata()
  #   vids <- updatevarids()
  #   ok <- is.finite(statistic)
  #   statistic <- statistic[ok]
  #   print(length(statistic))
  #   Y <- Y[ok,]
  #   print(dim(Y))
  #   
  #   #print('Stastistic shown on map');print(summary(statistic))
  #   
  #   if (input$country=='All') filter <- rep(TRUE,length(statistic)) else {
  #     filter <- rep(FALSE,length(statistic))
  #     filter[(Y$country == input$country)] <- TRUE
  #   }
  #   
  #   #print('        <<< input$ci is not updated!!! >>>              ')
  #   #isolate({print(paste('Range shown in map',input$statisticrange[1],'-',input$statisticrange[1],' ci=',input$ci))})
  #   filter[statistic < input$statisticrange[1]] <- FALSE
  #   filter[statistic > input$statisticrange[2]] <- FALSE
  #   
  #   highlight <- NULL
  #   lhighlight <- paste0('#',1:10)
  #   if (tolower(input$highlight) == "top 10") 
  #     highlight <- order(statistic[filter],decreasing=TRUE)[1:10] else 
  #       if (tolower(input$highlight) == "low 10") 
  #         highlight <- order(statistic[filter],decreasing=FALSE)[1:10] else 
  #           if (tolower(input$highlight) == "new records") {
  #             #print('--- Higlight new records ---')
  #             ## For a specific day, check against the maximum or minimum
  #             if (tolower(input$statistic)!='specific_day') {
  #               ## If a specific day is chosen, then compare the last day against maximum values 
  #               x <- retrieve.station(updatefile(),it=attr(Y,'period')[2],verbose=verbose)
  #               Z <- c(coredata(x))
  #               dim(Z) <- c(length(Z),1)
  #               ## The stations are sorted according to alphabetic order
  #               Z <- Z[match(Y$station.id,stid(x))]
  #             } else {
  #               ## Else compare the selected day against the maximum.
  #               Z <- statistic
  #             }
  #             #print(paste('Minimum?',is.null(Y$min)))
  #             ## Check if the current day equals the maximum value: if so, then highlight
  #             if (!is.null(Y$wetmean)) 
  #               highlight <- (Z[filter] - Y$max[filter] > -0.001) & is.finite(Z[filter]) else 
  #                 highlight <- ((Z[filter] - Y$max[filter] > -0.001) | 
  #                                 (Z[filter] - Y$min[filter] < +0.001)) & is.finite(Z[filter])
  #               #print('RECORDS?')
  #               highlight[is.na(highlight)] <- FALSE
  #               #print(paste('Number of records on',input$it,'is',sum(highlight,na.rm=TRUE)))
  #               #print(paste(Y$location[filter][highlight],Z[filter][highlight]))
  #               #print(table(highlight)); print(table(filter))
  #               lhighlight <- rep('Record',sum(highlight,nn.rm=TRUE))
  #           } 
  #   
  #   lon.highlight <- Y$longitude[filter][highlight]
  #   lat.highlight <- Y$latitude[filter][highlight]
  #   #if (tolower(input$highlight) != "none") {
  #   #  print(paste('Highlight',input$highlight))
  #   #  print(statistic[filter][highlight]); print(lon.highlight); print(lat.highlight)
  #   #}
  #   if (sum(filter)==0) {
  #     #print(paste(input$ci,esd::varid(y),min(statistic),max(statistic),input$statisticrange[1],input$statisticrange[2]))
  #     filter <- rep(TRUE,length(statistic))  
  #   }
  #   if (sum(is.element(vids[as.numeric(input$ci)],c('precip','sd')))>0) reverse <- TRUE else reverse <- FALSE
  #   if ((input$statistic=="lastrains") | (input$statistic=="mean_drydur")) reverse <- FALSE
  #   if((input$statistic=="trend_wetfreq") | (input$statistic=="trend_wetmean") |
  #      (input$statistic=="wetfreq") | (input$statistic=="wetmean")) reverse <- TRUE
  #   #print(paste('Reverse palette =',reverse)); print(summary(statistic))
  #   print(c(sum(filter),length(filter),length(statistic))); print(summary(statistic))
  #   pal <- colorBin(colscal(pal = 't2m',n=10),
  #                   seq(input$statisticrange[1],input$statisticrange[2],length=10),bins = 10,pretty = TRUE,reverse=reverse)    
  #   
  #   ## Only show good data
  #   good <- is.finite(Y$longitude) & is.finite(Y$latitude) & is.finite(statistic)
  #   Y <- Y[good,]; statistic <- statistic[good]; filter <- filter[good]
  #   print('Check statistics and pallette'); str(statistic[filter]); str(pal(statistic[filter]))
  #   
  #   ## Mark the selected location
  #   is <- which(tolower(Y$location[filter]) == tolower(input$location))[1]
  #   if ( (length(is)==0) | is.na(is) ) is <- 1
  #   radius <- rep(input$rad,length(statistic[filter]))
  #   radius[!is.finite(statistic[filter])] <- 1
  #   
  #   # print(paste('The map is being rendered:','Number of locations shown=',sum(filter),'with',sum(!is.finite(statistic)),
  #   #             'bad points - range of values= [',min(statistic,na.rm=TRUE),max(statistic,na.rm=TRUE),'] - slider:',
  #   #             input$statisticrange[1],'-',input$statisticrange[2],' ci=',input$ci,'is=',is))
  #   # print(summary(statistic)); print(summary(Y$longitude)); print(summary(Y$latitude))
  #   # print(paste('Filter: is=',is,'l=',length(filter),'s=',sum(filter),'ID=',Y$station.id[filter][is],
  #   #           Y$longitude[filter][is],Y$latitude[filter][is],Y$location[filter][is],
  #   #           'n=',length(statistic[filter]),' good=',sum(good)))
  #   # str(Y$longitude[filter]); str(Y$latitude[filter])
  #   # print(paste(Y$location[filter],as.character(round(statistic[filter],digits = 2))))
  #   # str(radius); 
  #   str(Y$station.id[filter])
  #   
  #   leaflet("mapid") %>% 
  #     addCircleMarkers(lng = Y$longitude[filter], # longitude
  #                      lat = Y$latitude[filter],fill = TRUE, # latitude
  #                      label = paste(Y$location[filter],as.character(round(statistic[filter],digits = 2))),
  #                      labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
  #                      popup = Y$location[filter],popupOptions(keepInView = TRUE),
  #                      radius =radius,stroke=TRUE,weight = 1, color='black',
  #                      layerId = Y$station.id[filter],
  #                      fillOpacity = 0.4,fillColor=pal(statistic[filter])) #%>% 
  #     addCircleMarkers(lng = Y$longitude[filter][is], lat = Y$latitude[filter][is],fill=TRUE,
  #                      label = paste(Y$location[filter][is],as.character(round(statistic[filter][is],digits = 2))),
  #                      labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
  #                      radius=6,stroke=TRUE, weight=3, color='green',
  #                      fillOpacity = 0.5,fillColor = pal(statistic[filter])[is],
  #                      layerId = Y$station.id[filter][is]) %>%
  #     addCircleMarkers(lng = lon.highlight, lat = lat.highlight,fill=TRUE,
  #                      label=paste0(lhighlight,': ',Y$location[filter][highlight],' - ',
  #                                   as.character(round(statistic[filter][highlight],digits = 2))),
  #                      labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
  #                      radius=6,stroke=TRUE, weight=3, color='black',
  #                      layerId = Y$station.id[filter][highlight],
  #                      fillOpacity = 0.5,fillColor=pal(statistic[filter])[highlight]) %>%
  #     addLegend("bottomright", pal=pal, values=round(statistic[filter], digits = 2), 
  #               title=legendtitle(),
  #               layerId="colorLegend",labFormat = labelFormat(big.mark = "")) %>%
  #     #addProviderTiles(providers$Esri.WorldStreetMap,
  #     #addProviderTiles(provider$Esri.WorldTopoMap,
  #     #addProviderTiles(providers$Stamen.TonerLite,
  #     #addProviderTiles(providers$Stamen.TerrainBackground,
  #     addProviderTiles(providers$Stamen.Terrain,
  #                      options = providerTileOptions(noWrap = FALSE)) %>% 
  #     setView(lat=Y$latitude[filter][is],lng = Y$longitude[filter][is], zoom = zoom())
  #})

#})