## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(shinydashboard)
library(esd)

varname <- function(x, long=TRUE) {
  if(long) {
    y <- switch(x, 
                "pr" = "precipitation", 
                "mu" = "wet-day mean",
                "fw" = "wet-day frequency",
                "t2m" = "temperature",
                "tsd" = "temperature standard deviation", x)
  } else {
    y <- switch(x, 
                "precipitation" = "pr", 
                "wet-day mean" = "mu",
                "wet-day frequency" = "fw",
                "temperature" = "t2m",
                "temperature standard deviation" = "tsd", x)
  }
  return(y)
}

seasonname <- function(x, long=TRUE) {
  if(long)  {
    y <- switch(x, 
                "djf" = "winter",
                "mam" = "spring",
                "jja" = "summer",
                "son" = "fall", x)
  } else {
    y <- switch(x, 
                "winter" = "djf",
                "spring" = "mam",
                "summer" = "jja",
                "fall" = "son", x)
  }
  return(y)
}


## Remove special characters from string
cleanstr <- function(x, remove=NULL) {
  y <- gsub("[[:punct:]]|\\s", "", x)
  if(!is.null(remove)) y <- gsub(remove, "", y)
  return(y)
}


## Script to extract members from DSensemble with 
xmembers <- function(X,im=NULL,length=NULL,verbose=FALSE) {
  if(verbose) print("xmembers")
  x <- X
  info <- X$info; X$info <- NULL
  pca <- X$pca; X$pca <- NULL; eof <- X$eof; X$eof <- NULL
  gcms <- attr(X, "model_id")
  n <- length(names(X))
  #im <- 20:100
  ## Quality control
  if (verbose) print(paste('Before quality control: original number of members=',n))
  ix <- rep(TRUE, n)
  ix[duplicated(gcms)] <- FALSE
  if(!is.null(im)) {
    if(is.logical(im) & length(im)==length(ix)) {
      ix <- ix & im
    } else if(is.numeric(im) & max(im)<=n) {
      ix[-im] <- FALSE
    }
  }
  for (i in seq(n,1,by=-1)) {
    if (sum(is.finite(X[[i]]))==0) {
      if(verbose) print(paste(i,'Remove bad results'))
      ix[i] <- FALSE
    } else if (max(abs(X[[i]]),na.rm=TRUE) > 10)  {
      if(verbose) print(paste(i,'Remove suspect results'))
      ix[i] < FALSE
    }
  }
  
  memsiz <- rep("?",n)
  for (i in 1:n) 
    memsiz[i] <- paste(dim(X[[i]]),collapse='x')
  memsiztab <- table(memsiz)
  memcats <- rownames(memsiztab)
  if (verbose) {
    print(memsiztab)
    print(paste0('(',1:length(memcats),') ',memcats,collapse=' - '))
  }
  if (is.null(length)) {
    memkeep <- rownames( memsiztab)[as.numeric(memsiztab)==max(as.numeric(memsiztab))]
  } else memkeep <- memcats[grep(paste0(as.character(length),'x'),memcats)]
  if (verbose) print(memkeep)
  ix[!grepl(memkeep,memsiz)] <- FALSE
  exclude <- sort((1:n)[!ix], decreasing=TRUE)
  if (verbose) print(paste("Remove simulations short time series:",
                           paste(exclude,collapse=" ")))
  for (ix in exclude) {
    x[[ix+2]] <- NULL
    gcms <- gcms[-ix]
  }
  attr(x, "model_id") <- gcms
  n <- length(names(x))
  if (verbose) print(paste('New length of X is',n))
  return(x)
}


zload <- function(path="data", 
                  pattern=c("dse.kss","Nordic","t2m","djf","ssp585"), 
                  verbose=FALSE) {
  if(verbose) print("zload")
  files <- list.files(path, pattern=pattern[1], full.names=TRUE)
  i <- eval(parse(text=paste(paste0("grepl('",pattern,"', files)"), 
                             collapse=" & ")))
  if(sum(i)==1) {
    if(verbose) print(paste("load file",files[i]))
    load(files[i])
    Z <- xmembers(Z, verbose=verbose)
    if('mu' %in% pattern) {
      attr(Z, "variable") <- "mu"
      attr(Z, "longname") <- "wet-day mean precipitation"
    } else if('fw' %in% pattern) {
      attr(Z, "variable") <- "fw"
      attr(Z, "longname") <- "wet-day frequency"
      attr(Z, "unit") <- "%"
      if(max(Z, na.rm=TRUE)<=1) {
        cz <- coredata(Z)*100
        coredata(Z) <- cz
      }
    } else if('t2m' %in% pattern) {
      attr(Z, "unit") <- "degree*C"
    }
    attr(Z, "season") <- season(Z$pca)
    return(Z)
  } else return(NULL)
}


sliderange <- function(param=NULL, FUN=NULL) {
  if(is.null(param)) param <- "t2m"
  if(is.null(FUN)) FUN <- "mean"
  if(FUN=="trend") {
    minmax <- switch(param,
                     "pr"=c(-0.8,0.8),
                     #"fw"=c(-0.04,0.04),
                     "fw"=c(-4,4), # % instead of fraction
                     "mu"=c(-0.8,0.8),
                     "t2m"=c(-1.5,1.5),
                     "tsd"=c(0.5,0.5),
                     c(-1,1))
    x <- switch(param,
                "pr"=c(-0.4,0.4),
                #"fw"=c(-0.01,0.01),
                "fw"=c(-1,1), # % instead of fraction
                "mu"=c(-0.1,0.1),
                "t2m"=c(-1,1),
                "tsd"=c(-0.05,0.05),
                c(-0.6,0.6))
  } else {
    minmax <- switch(param,
                     "pr"=c(0,25),
                     #"fw"=c(0,1),
                     "fw"=c(0,100), # % instead of fraction
                     "mu"=c(0,25),
                     "t2m"=c(-40,40),
                     "tsd"=c(0,15),
                     c(-30,50))
    x <- switch(param,
                "pr"=c(0,15),
                #"fw"=c(0,1),
                "fw"=c(0,100), # % instead of fraction
                "mu"=c(0,15),
                "t2m"=c(-15,30),
                "tsd"=c(0,5),
                c(-15,30))
  }
  return(list("x"=x, "minmax"=minmax)) 
}


## Gridded maps
mapgridded <- function(Z, MET='ESD', FUN='mean', FUNX='mean', eof=TRUE,
                       it=NULL, im=NULL, main=NULL, colbar=NULL,
                       show.field=TRUE, show.stations=TRUE, 
                       pch=19, cex=1, lwd=1,
                       show.robustness=TRUE,trends=NULL,threshold=0.9,
                       verbose=FALSE) { 
  if(verbose) print('mapgridded')
  
  if(MET=='ESD') {
  if(is.null(it)) it <- range(year(Z[[3]]))
  if(verbose) print(paste('it=',paste(it,collapse=' - ')))
  z <- subset(Z, it=it, im=im)
  
  if(is.null(main)) main <- " "
    #main <- paste(FUN,tolower(season(Z$pca)[1]),
    #              attr(Z,"variable")[1],'for',it[1],'-',it[2],
    #              '\ndownscaled with', 
    #              gsub("_", " ", 
    #                   gsub("_mon|.nc", "", basename(attr(Z, "predictor_file")))),
    #              '&', toupper(attr(Z, "scenario")), 
    #              paste0('(',length(z)-3),'model runs)')
  
  colbar$show <- TRUE
  if(is.null(colbar$pal)) {
    if ( (attr(Z,"variable")[1]=='t2m') | 
         (attr(Z,"variable")[1]=='tsd') | 
         (FUN=='trend') ) colbar$pal <- 't2m' else colbar$pal <- 'precip'
  }
  if(is.null(colbar$rev)) {
    if ( FUN=='trend' & 
         (attr(Z,"variable")[1]=='mu' | 
          attr(Z,"variable")[1]=='fw' | 
          attr(Z,"variable")[1]=='precip') ) colbar$rev <- TRUE else colbar$rev <- FALSE
  }
  
  xlim <- range(lon(Z$pca)) + diff(range(lon(Z$pca)))*c(-1,1)/5
  ylim <- range(lat(Z$pca)) + diff(range(lat(Z$pca)))*c(-1,1)/5
  if(verbose) print('expand the data:')
  if (FUNX=='mean') {
    ## Faster response for ensemble mean
    y <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=eof,verbose=TRUE)
    m <- map(y,FUN='mean',plot=FALSE)
  } else {
    y <- aggregate.dsensemble(z,FUNX=FUNX,eof=eof,verbose=TRUE)
    m <- map(y,FUN=FUN,plot=FALSE)
  }
  if(verbose) {print(main); print(str(y))}
  ## set breaks here
  if(is.null(colbar$breaks)) {
    if(FUN=='trend') colbar$breaks <- pretty(c(-abs(m),abs(m)),n=17) else 
      colbar$breaks <- pretty(m,n=17)
  }
  if(verbose) print(colbar)
    
  if(FUN=="trend") {
    attr(m, "unit") <- paste0(attr(z, "unit"),"/decade")
    attr(m, "variable") <- paste0(attr(z, "variable")," trend")
  } else {
    attr(m, "unit") <- attr(z, "unit")
    attr(m, "variable") <- attr(z, "variable")
  }
  
  if (FUNX=='mean') {
    ## Faster response for ensemble mean
    y2 <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=FALSE,verbose=TRUE)
    m2 <- map.station(y2,FUN='mean',plot=FALSE) 
    # using map.station to override tendency from esd to redirect to map.default
  } else {
    y2 <- aggregate.dsensemble(z,FUN=FUNX,eof=FALSE,verbose=TRUE)
    m2 <- map.station(y2,FUN=FUN,plot=FALSE)
  }
  if(show.field) {
    map(m,main=main,type="fill",colbar=colbar,FUN="mean",
        xlim=xlim,ylim=ylim,new=FALSE,cex.sub=0.8)
  } else {
    map(m,main=main,colbar=NULL,type="contour",FUN="mean",
        xlim=xlim,ylim=ylim,new=FALSE)
    par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey',
        cex.lab=0.7,cex.axis=0.7)
    axis(2,at=pretty(lat(Z$pca)),col='grey')
    axis(3,at=pretty(lon(Z$pca)),col='grey')
    par(col.axis='black',col.lab='black',
        cex.lab=0.5,cex.axis=0.5)
    image.plot(breaks=colbar$breaks,
               lab.breaks=colbar$breaks,horizontal = TRUE,
               legend.only = TRUE, zlim = range(colbar$breaks),
               col = colbar$col, legend.width = 1,
               axis.args = list(cex.axis = 1,hadj = 0.5,mgp = c(0, 0.5, 0)), border = FALSE)
  }
  if(show.stations) {
    if (FUNX=='mean') {
      ## Faster response for ensemble mean
      y2 <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=FALSE,verbose=TRUE)
      m2 <- map.station(y2,FUN='mean',plot=FALSE) 
      # using map.station to override tendency from esd to redirect to map.default
    } else {
      y2 <- aggregate.dsensemble(z,FUN=FUNX,eof=FALSE,verbose=TRUE)
      m2 <- map.station(y2,FUN=FUN,plot=FALSE)
    }
    ## calculate color 
    colbar <- colbar.ini(m2, colbar=colbar)
    if (verbose) print('Set colour scheme')
    wr <- round(strtoi(paste('0x',substr(colbar$col,2,3),sep=''))/255,2)
    wg <- round(strtoi(paste('0x',substr(colbar$col,4,5),sep=''))/255,2)
    wb <- round(strtoi(paste('0x',substr(colbar$col,6,7),sep=''))/255,2)
    col <- rep(colbar$col[1],length(m2))
    for (i in 1:length(m2)) {
      ii <- round(approx(0.5*(colbar$breaks[-1]+colbar$breaks[-length(colbar$breaks)]),
                         1:length(colbar$col),
                         xout=as.vector(m2)[i],rule=2)$y)
      if (is.finite(ii)) {
        if (ii < 1) ii <- 1
        if (ii > length(colbar$col)) ii <- length(colbar$col)
        col[i] <- rgb(wr[ii],wg[ii],wb[ii],0.7)
      } else col[i] <- rgb(0.5,0.5,0.5,0.2)
    }
    points(lon(y2), lat(y2), col=col, pch=19, cex=cex, lwd=lwd)
    if(show.robustness & FUN=='trend' & !is.null(trends)) {
      sig <- apply(trends[im,], 2, function(x) max(sum(x>0), sum(x<0))/length(x))
      s <- sig>=threshold
      points(lon(y2)[s], lat(y2)[s], col='black', pch=21, cex=cex, lwd=lwd)
    }
  }
  } else if(MET=="RCM") {
    if(is.null(it) | FUN == "trend") it <- c(1950,2100)
    if(verbose) print(paste('it=',paste(it,collapse=' - ')))
    
    param <- attr(Z,"variable")[1]
    if (param == "precip") param <- "pr"
    
    season <- toupper(season(Z$pca))[1]
    scenario <- tolower(attr(Z, "scenario"))
    
    #Map function and parameter to filename strings
    FUNXfn <- FUNX #function in filename
    if(FUNXfn == "sd") FUNXfn <- "var"
    
    paramfn <- varname(param,long=FALSE)
    #if(paramfn %in% c("fw","mu")) paramfn <- "pr"
    # if(paramfn %in% c("t2m","tsd")) paramfn <- "tas"
    if(paramfn %in% c("t2m")) paramfn <- "tas"
    
    
    data.dir <- "data/rcm/" #link to /lustre/storeB/users/andreasd/KiN_2023_data/ESDandRCM4KSS/Nordic/rcm
    filename <- paste0("ens",FUNXfn,"_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
    
    if (FUN =="trend")
    {
      filename <- paste0("ens",FUNXfn,"_trend_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
    }
    
    ncfile <- paste0(data.dir,filename)
    if(verbose) print(paste("Getting",param,"from",ncfile))
    
    nc <- nc_open(ncfile)
    Z = ncvar_get(nc,paramfn)
    nc_close(nc)
    
    if (FUN == "trend") Z <- Z*10 # trends per decade
    if (FUNX == "sd") Z <- sqrt(Z) # var to sd
    
    if (param == "pr") Z <- Z*3600*24 # mm/s to mm/day
    if (param == "t2m" & FUNX != "sd" & FUN != "trend") Z <- Z-273.15 #K to °C
    if (param %in% c("tsd","fw","mu")) Z <- Z*0 # not yet available
    
    scenarios <- c("rcp26","rcp45","rcp85")
    si <- which(scenarios == scenario)
    scenruns <- c(25,23,60) #number of runs in rcp26,rcp45,rcp85
    
    units <- switch(param,
                    "pr" = "mm/day",
                    "t2m"= "°C",
                    "tsd"= "°C",
                    "fw"= "1",
                    "mu"="mm/day"
    )
    
    if (FUN == "trend") units <- paste(units,"per decade")
    
    if(is.null(main)) {
      main <- " "#paste(FUN,tolower(season),param,'for',it[1],'-',it[2],"following",scenario,'scenario\n(Ensemble',FUNX,'of',scenruns[si],'CORDEX runs)')
    }
    
    if(is.null(colbar$pal)) {
      if ( (param=='t2m') | 
           (param=='tsd') | 
           (FUN=='trend') ) colbar$pal <- 't2m' else colbar$pal <- 'precip'
    }
    if(is.null(colbar$rev)) {
      if ( FUN=='trend' & 
           (param=='mu' | 
            param=='fw' | 
            param=='pr') ) colbar$rev <- TRUE else colbar$rev <- FALSE
    }
    
    tidx <- which(1950:2100 %in% it)
    
    if (FUN != "trend") Z <- apply(Z[,,tidx[1]:tidx[2]],c(1,2),FUN)
    
    ## set breaks here
    if(is.null(colbar$breaks)) {
      if(FUN=='trend') colbar$breaks <- pretty(c(-abs(Z),abs(Z)),n=17) else 
        colbar$breaks <- pretty(Z,n=17)
    }
    #if(verbose) print(colbar)
    colours = colscal(n=length(colbar$breaks)-1,pal=colbar$pal)
    if (colbar$rev) colours <- rev(colours)
    
    par(mgp=c(0.5,0.2,0), mar=c(1.5,0.5,1.5,0.5))
    image.plot(1:(dim(Z)[1]),1:(dim(Z)[2]),Z,main=main,col=colours,xaxt="n",yaxt="n",
               zlim=range(colbar$breaks),xlab="",ylab="",horizontal=TRUE)
    mtext(side=1, text=paste0("[",units,"]"),cex=1,font=2)
    
    #coastline
    nc <- nc_open(paste0(data.dir,"remapbil_sftlf_gthalf_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r0i0p0_CLMcom-CCLM4-8-17_v1_fx.nc"))
    cl <- ncvar_get(nc,"sftlf")
    nc_close(nc)
    
    contour(1:(dim(Z)[1]),1:(dim(Z)[2]),cl, levels=1,add=TRUE,drawlabels = F )
    
    if(show.robustness & FUN=='trend')
    {
      filename <- paste0("ensmean_postrend_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
      
      ncfile <- paste0(data.dir,filename)
      if(verbose) print(paste("Getting trend robustness",param,"from",ncfile))
      
      nc <- nc_open(ncfile)
      sig <- ncvar_get(nc,paramfn)
      nc_close(nc)
      
      s <- sig>=threshold | sig <= (1-threshold)
      if(param %in% c("t2m","pr"))
      {
        points(which(s,arr.ind=T),pch=".",col=rgb(0,0,0,0.4))
      }
    }
  }
}


## Plot individual station
stplot <- function(z, is=NULL, it=NULL, im=NULL, main=NULL, 
                   MET="ESD", ylim=NULL, verbose=FALSE) {
  if(verbose) print('--- Plot individual station ---')
  if(!inherits(z, "station")) z <- as.station(z)
  if(is.null(is)) {
    is <- 1
  } else {
    is <- grep(cleanstr(is, "[A-Z]"), stid(z))
    if(length(is)==0) is <- 1
  }
  
  if(is.null(it)) {
    it <- c(1950, 2100)
  } else {
    it <- range(as.numeric(it))
  }
  y <- subset(z, is=is, it=it, im=im)
  if(MET=="ESD") {
    plot(y, target.show=FALSE, legend.show=FALSE, new=FALSE, #main=main,
       xrange=range(attr(z, "longitude"))+c(-5,5), ylim=ylim,
       yrange=range(attr(z, "latitude"))+c(-2,2), map.show=TRUE,
       mar=c(3,5,4,4))
  } else if(MET=="RCM"){
    param <- attr(y,"variable")[1]
    season <- toupper(attr(z, "season"))[1]
    scenario <- tolower(attr(y, "scenario"))     
    
    lonst <- abs(attr(y, "longitude"))
    latst <- abs(attr(y, "latitude"))
    
    paramfn <- varname(param,long=FALSE)
    if (param == "precip") paramfn <- "pr"
    
    if(paramfn %in% c("fw","mu")) paramfn <- "pr"
    if(paramfn %in% c("t2m","tsd")) paramfn <- "tas"
    
    years <- 1950:2100
    tidx <- which(years %in% it)
    yr <- years[years>=min(it) & years<=max(it)]
    
    data.dir <- "data/rcm/" #link to /lustre/storeB/users/andreasd/KiN_2023_data/ESDandRCM4KSS/Nordic/rcm
    ## Mean value
    filename <- paste0("ensmean_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
    ncfile <- paste0(data.dir,filename)
    if(verbose) print(paste("Getting",paramfn,"from",ncfile))
    
    nc <- nc_open(ncfile)
    lonrcm <- ncvar_get(nc,"lon")
    latrcm <- ncvar_get(nc,"lat")
    
    #Calculate the distance to the point for every grid box
    dist <- sqrt((cos(latrcm/180*pi)*(lonrcm-lonst))^2+(latrcm-latst)^2)
    #find the indices with the minimum distance
    idx <- which(dist==min(dist),arr.ind=T)
    
    rcm <- ncvar_get(nc,paramfn,start=c(idx[1],idx[2],tidx[1]),count=c(1,1,tidx[2]-tidx[1]+1))
    nc_close(nc)
    
    ## Max value
    filename <- paste0("ensmax_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
    ncfile <- paste0(data.dir,filename)
    if(verbose) print(paste("Getting",paramfn,"from",ncfile))
    
    nc <- nc_open(ncfile)
    lonrcm <- ncvar_get(nc,"lon")
    latrcm <- ncvar_get(nc,"lat")
    
    #Calculate the distance to the point for every grid box
    dist <- sqrt((cos(latrcm/180*pi)*(lonrcm-lonst))^2+(latrcm-latst)^2)
    #find the indices with the minimum distance
    idx <- which(dist==min(dist),arr.ind=T)
    
    rcm.max <- ncvar_get(nc,paramfn,start=c(idx[1],idx[2],tidx[1]),count=c(1,1,tidx[2]-tidx[1]+1))
    nc_close(nc)
    
    ## Max value
    filename <- paste0("ensmin_remapbil_",paramfn,"_EUR-11_",scenario,"_",season,".nc")
    ncfile <- paste0(data.dir,filename)
    if(verbose) print(paste("Getting",paramfn,"from",ncfile))
    
    nc <- nc_open(ncfile)
    lonrcm <- ncvar_get(nc,"lon")
    latrcm <- ncvar_get(nc,"lat")
    
    #Calculate the distance to the point for every grid box
    dist <- sqrt((cos(latrcm/180*pi)*(lonrcm-lonst))^2+(latrcm-latst)^2)
    #find the indices with the minimum distance
    idx <- which(dist==min(dist),arr.ind=T)
    
    rcm.min <- ncvar_get(nc,paramfn,start=c(idx[1],idx[2],tidx[1]),count=c(1,1,tidx[2]-tidx[1]+1))
    nc_close(nc)
    
    if (paramfn == "pr") {
      rcm <- rcm*3600*24 # mm/s to mm/day
      rcm.max <- rcm.max*3600*24 # mm/s to mm/day
      rcm.min <- rcm.min*3600*24 # mm/s to mm/day
      unit <- "mm/day"
    }
    if (param == "t2m") {
      rcm <- rcm-273.15 #K to °C
      rcm.max <- rcm.max-273.15 #K to °C
      rcm.min <- rcm.min-273.15 #K to °C
      unit <- "deg*C"
    }
    if (param %in% c("tsd","fw","mu")) {
      rcm <- rcm*NA # not yet available
      rcm.max <- rcm.max*NA # not yet available
      rcm.min <- rcm.min*NA # not yet available
      unit <- NA
    }

    rcm <- as.station(as.zoo(rcm, order.by=yr), param = param, unit = unit)
    rcm.max <- as.station(as.zoo(rcm.max, order.by=yr), param = param, unit = unit)
    rcm.min <- as.station(as.zoo(rcm.min, order.by=yr), param = param, unit = unit)
    plot(rcm, new=FALSE, ylim=ylim, mar=c(3,5,4,4),type="l",col="navy",main=loc(y))
    grid()
    lines(rcm.max, col="black", lty=2)
    lines(rcm.min, col="black", lty=2)
  }
  
}
