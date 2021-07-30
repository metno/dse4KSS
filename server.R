
shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)
  
  ## Try to get the location names to depend on whether temperature of precipitation stations
  output$location2 <- renderUI({
    ## Get the file names of the data
    z <- Z4[[input$file2]]
    locs <- loc(z$pca)
    #updateSelectInput(session=session,inputId="locations2",choices=locs)
    selectInput("location2", label = "Location",choices = locs,  selected = "OSLO BLINDERN")
  })
  
  
  ## Show map of gridded temperature
  output$maps <- renderPlot({ 
    print('output$maps')
    it <- range(as.numeric(input$dates1))
    print(paste('it=',paste(it,collapse=' - ')))
    z <- Z4[[input$file1]]
    z <- subset(z,it=it)
    z <- xmembers(z)
    print(names(z))
    print(range(index(z[[3]])))
    FUN=input$fun1; FUNX <- input$funx1
    info <- namesplit(input$file1)
    #im <- is.element(gcmnames,input$im)
    ## Extract the DSe results and exclude the info, PCA and EOF elements.
    gcnames <- names(z)[-c(1,2,length(z))]
    im <- is.character(gcmnames)
    main <- paste('Downscaled',FUN,tolower(input$season1),tolower(input$param1),'for',it[1],'-',it[2],
                  'following',toupper(input$rcp1),'based on',sum(im),'model runs')
    
    if ( (info$varid=='t2m') | (info$varid=='tsd') | (FUN=='trend') ) 
      pal <- 't2m' else pal <- 'precip'
    if ( ((info$varid=='mu') | (info$varid=='fw')) & (FUN=='trend') ) 
      rev <- TRUE else rev <- FALSE
    print('expand the data:')
    fun <- FUN
    if (FUNX=='mean') {
      ## Faster response for ensemble mean
      y <- expandpca(z,FUN=FUN,verbose=TRUE)
      FUN <- 'mean'
    } else 
    y <- aggregate.dsensemble(z,FUN=FUNX,verbose=TRUE)
    
    main <- paste(info$varid,info$src,info$nem, info$sce, info$it, FUN)
    print(main); print(str(y))
    m <- map(y,FUN=FUN,plot=FALSE)
    if (fun=='trend') breaks <- pretty(c(-abs(m),abs(m)),n=17) else
      breaks <- pretty(m,n=17)
    colbar <- list(pal=pal,breaks=breaks,rev=rev)
    print(colbar)
    map(m,main=main,type='fill',colbar=colbar,new=FALSE)
  }, height=function(){0.6*session$clientData$output_maps_width})
  
  ## Plot individual station
  output$plot <- renderPlot({
    print('--- Plot individual station ---')
    z <- Z4[[input$file2]]
    it <- range(as.numeric(input$dates2))
    z <- subset(z,it=it)
    z <- xmembers(z)
    #z$pca <- subset(z$pca,is=is)
    locs2 <- loc(z$pca)
    
    if(!is.null(input$location2)) is <- grep(input$location2,locs2,ignore.case = TRUE) else is <- 1
    print('input$location2')
    print(input$location2)
    gcnames <- names(z)[-c(1,2,length(z))]

    z$eof <- NULL
    print(is); print(class(as.station(z))); print(length(as.station(z))); 
    #im <- is.element(gcmnames,input$im)
    y <- as.station(z)[[is]]
    info <- namesplit(input$file2)
    main <- paste(info$varid,info$src,info$nem, info$sce, info$it)
    print(class(y)); print(main); print(dim(y))
    plot(y,main=main,target.show=FALSE,legend.show=FALSE,new=FALSE,
         map.show=TRUE)
  #index(y) <- year(y)
  #lines(y,type='b',lwd=3,cex=1.2)
  #plot(rnorm(100),main=main)
}, height=function(){0.6*session$clientData$output_plot_width}) #600})
  })
