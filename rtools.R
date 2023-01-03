## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(esd)
#source("~/git/esd/R/as.station.R")
source("~/git/esd/R/retrieve.R")

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

maprange <- function(region) {
  is <- list(lon=switch(tolower(region), "nordic"=c(-15,45),
                        "finland"=c(-15,45), c(-15,45)),
             lat=switch(tolower(region), "nordic"=c(45,75),
                        "finland"=c(45,75), c(45,75)))
  return(is)
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


zload <- function(path="data", type="field", src="ESD",
                  region="Nordic", param="t2m", season="djf",
                  scenario="ssp585", FUNX="mean", FUN="mean", 
                  verbose=FALSE) {
  if(verbose) print("zload")
  
  if(src=="ESD") { 
    pattern <- c("dse.kss", region)
  } else if (src=="RCM") {
    if(is.null(FUNX)) FUNX <- "mean"
    pattern <- c("ens","EUR-11","remapbil",FUNX)
    if(grepl("trend",FUN)) pattern <- c(pattern,FUN) 
    if(param %in% c("fw","mu","precip")) param <- "pr"
    if(param %in% c("t2m","tsd")) param <- "tas"
    path <- file.path(path,"rcm")
  }
  pattern <- c(pattern, param, scenario, season)

  files <- list.files(path, pattern=pattern[1], full.names=TRUE)
  i <- eval(parse(text=paste(paste0("grepl('",tolower(pattern),
                                    "', tolower(files))"), 
                             collapse=" & ")))
  if(!grepl("trend",FUN)) {
    if(sum(i)>1 & !any(grepl("trend",pattern))) i <- i & !grepl("trend",files)
  } else if(any(grepl("trend",pattern))) {
    pattern_trend <- pattern[grepl("trend",pattern)]
    i <- i & grepl(paste0("_",tolower(pattern_trend),"_"), 
                   tolower(files))
  }
  
  if(sum(i)==1) {
    if(grepl(".rda", files[i]) & grepl("dse", files[i])) {
      if(verbose) print(paste("load file",files[i]))
      load(files[i])
      Z <- xmembers(Z, verbose=verbose)
      attr(Z, "season") <- season(Z$pca)
    } else if(grepl(".nc", files[i]) & grepl("ens", files[i])) {
      if(verbose) print(paste("Getting",param,"from",files[i]))
      Z <- retrieve.rcm(files[i], param=param)
    }
    if('mu' %in% pattern) {
      attr(Z, "variable") <- "mu"
      attr(Z, "longname") <- "wet-day mean precipitation"
    } else if('fw' %in% pattern) {
      attr(Z, "variable") <- "fw"
      attr(Z, "longname") <- "wet-day frequency"
      attr(Z, "unit") <- "%"
      if(inherits(Z, "field")) if(max(Z, na.rm=TRUE)<=1) {
        cz <- coredata(Z)*100
        coredata(Z) <- cz
      }
    } else if(any(grepl('t2m|tas',pattern))) {
      attr(Z, "variable") <- "t2m"
      attr(Z, "unit") <- "degree*C"
      if(inherits(Z, c("field","station"))) if(max(Z, na.rm=TRUE)>200) {
        cz <- coredata(Z) - 273.15
        coredata(Z) <- cz
      }
    } else if(any(c("pr","precip","mu") %in% pattern)) {
      if(inherits(Z, c("field","station"))) {
        if(grepl("s-1|kg",attr(Z,"unit")) & max(Z, na.rm=TRUE)<1E-2) {
          cz <- coredata(Z)
          coredata(Z) <- cz*3600*24 # mm/s (or kg m-2 s-1) to mm/day
          attr(Z, "unit") <- "mm/day"
        }
      }
    }
    if ("trend" %in% pattern) {
      cz <- coredata(Z)
      coredata(Z) <- cz*10 # trends per decade
      attr(Z, "unit") <- paste0(attr(Z, "unit"),"/decade")
      attr(Z, "variable") <- paste0(attr(Z, "variable")," trend")
    } 
    attr(Z, "season") <- season
    attr(Z, "scenario") <- scenario
    attr(Z, "param") <- param
    #if (FUNX == "sd") {
    #  cz <- coredata(Z)
    #  coredata(Z) <- sqrt(cz) # var to sd
    #}
  } else {
    Z <- NULL
  }
  return(Z)
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


mask.field <- function(x,land=FALSE) {
  data(etopo5, envir = environment())
  h <- regrid(etopo5,is=x)
  if (!land) {
    h[h < -5] <- NA
  } else {
    h[h > 5] <- NA
  }
  X <- coredata(x)
  X[,is.na(h)] <- NA
  X -> coredata(x)
  return(x)
}

mask.station <- function(x,land=FALSE) {
  data(etopo5, envir = environment())
  h <- subset(etopo5,is=list(lon=range(lon(x)),lat=range(lat(x))))
  hx <- sapply(seq_along(x), function(i) {
    ilon <- which.min(abs(lon(h)-lon(x)[i]))
    ilat <- which.min(abs(lat(h)-lat(x)[i]))
    return(h[ilon,ilat])
  })
  if (!land) {
    hx[hx < -5] <- NA
  } else {
    hx[hx > 5] <- NA
  }
  X <- coredata(x)
  X[,is.na(hx)] <- NA
  X -> coredata(x)
  return(x)
}


## Gridded maps
mapgridded <- function(Z, MET='ESD', FUN='mean', FUNX='mean', eof=TRUE,
                       it=NULL, im=NULL, main=NULL, colbar=NULL,
                       show.field=TRUE, show.stations=FALSE, 
                       oceanmask=TRUE,
                       pch=19, cex=1, lwd=1, xlim=NULL, ylim=NULL,
                       show.robustness=TRUE,trends=NULL,threshold=0.9,
                       new=FALSE, add=FALSE, fig=NULL,
                       verbose=FALSE) { 
  if(verbose) print('mapgridded')
  
  if(inherits(Z, "dsensemble")) {
    if(is.null(it)) it <- range(year(Z[[3]]))
    if(verbose) print(paste('it=',paste(it,collapse=' - ')))
    z <- subset(Z, it=it, im=im)
    
    if (FUNX=='mean') { # Faster response for ensemble mean
      y <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=eof,verbose=verbose)
      m <- map(y,FUN='mean',plot=FALSE)
      if(show.stations | show.robustness) {
        y2 <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=FALSE,verbose=verbose)
        m2 <- map.station(y2,FUN='mean',plot=FALSE) 
      }
    } else {
      y <- aggregate.dsensemble(z,FUNX=FUNX,eof=eof,verbose=verbose)
      m <- map(y,FUN=FUN,plot=FALSE)
      if(show.stations | show.robustness) {
        y2 <- aggregate.dsensemble(z,FUN=FUNX,eof=FALSE,verbose=verbose)
        m2 <- map.station(y2,FUN=FUN,plot=FALSE)
      }
    }
    if(FUN=="trend") {
      attr(m, "unit") <- paste0(attr(z, "unit"),"/decade")
      attr(m, "variable") <- paste0(attr(z, "variable")," trend")
    } else {
      attr(m, "unit") <- attr(z, "unit")
      attr(m, "variable") <- attr(z, "variable")
    }
  } else if(inherits(Z,"station")) {
    if(is.null(it) | FUN == "trend") it <- c(1950,2100)
    if(verbose) print(paste('it=',paste(it,collapse=' - ')))
    
    if(grepl("trend",attr(Z,"variable"))) {
      m <- map.station(Z, FUN="mean", plot=FALSE)
    } else {
      m <- map.station(Z, FUN=FUN, plot=FALSE)
    }
    if(oceanmask) m <- mask.station(m,land=FALSE)
    attr(m, "unit") <- attr(Z, "unit")
    attr(m, "variable") <- attr(Z, "variable")
    m2 <- m
    cex <- cex*0.1
    
    show.field <- FALSE
    show.stations <- TRUE
  } else browser()
  if(is.null(main)) main <- " "
  if(is.null(xlim)) xlim <- range(lon(m)) + diff(range(lon(m)))*c(-1,1)/5
  if(is.null(ylim)) ylim <- range(lat(m)) + diff(range(lat(m)))*c(-1,1)/5
  
  colbar$show <- FALSE
  if(is.null(colbar$pal)) {
    if ( (attr(m,"variable")[1] %in% c('t2m','tsd','tas')) | 
         (FUN=='trend') ) colbar$pal <- 't2m' else colbar$pal <- 'precip'
  }
  if(is.null(colbar$rev)) {
    if ( FUN=='trend' & 
         attr(m,"variable")[1] %in% c('mu','fw','precip','pr') ) {
      colbar$rev <- TRUE 
    } else colbar$rev <- FALSE
  }
  
  if(verbose) {print(main); print(str(m))}
  ## set breaks here
  if(is.null(colbar$breaks)) {
    if(FUN=='trend') colbar$breaks <- pretty(c(-abs(m),abs(m)),n=17) else 
      colbar$breaks <- pretty(m,n=17)
  }
  if(verbose) print(colbar)
  
  ## Set up map
  par(mar=c(3,1,1.5,3), mgp=c(1,0.5,0))
  par0 <- par()
  data(Oslo)
  esd::map(Oslo, cex=0.1, main=main, mar=par0$mar, mgp=par0$mgp,
           xlim=xlim, ylim=ylim, xlab="", ylab="", fig=fig,
           new=new, add=add, cex.sub=0.8)
  
  ## Add field 
  if (show.field) {
    cb <- colbar.ini(m,FUN=NULL,colbar=colbar,verbose=verbose)
    image(lon(m),lat(m),m,xlab="",ylab="",add=TRUE,
          col=cb$col,breaks=cb$breaks,xlim=xlim,ylim=ylim)
  }
  
  ## Add stations
  if (show.stations) {
    cb2 <- colbar.ini(m2,FUN=NULL,colbar=colbar,verbose=verbose)
    icol <- apply(as.matrix(m2),2,findInterval,cb2$breaks)
    col <- cb2$col[icol]
    points(lon(m2), lat(m2), pch = pch,
           bg=cb2$col[icol], col=col, cex=cex)
  }
  
  ## Add colorbar
  par(xaxt="s",yaxt="s",las=1,col.axis='black',col.lab='black',
      cex.lab=0.5,cex.axis=0.5)
  image.plot(breaks=colbar$breaks,
             lab.breaks=colbar$breaks, horizontal=TRUE,
             legend.only=TRUE, zlim=range(colbar$breaks),
             col=colbar$col, legend.width=1, legend.line=2,
             axis.args = list(cex.axis=1, hadj=0.5, mgp=c(0,0.5,0)), 
             border=FALSE)

  ## Add trend robustness
  if(show.robustness & FUN=='trend') { 
    decimals <- 0
    cex.robust <- 2
    if(inherits(Z,"dsensemble") & !is.null(trends)) {
      sig <- apply(trends[im,], 2, function(x) max(sum(x>0), sum(x<0))/length(x))
      s <- sig>=threshold | sig <= (1-threshold)
      #points(lon(y2)[s], lat(y2)[s], col='black', pch=21, cex=cex*0.8, lwd=lwd)
      #points(lon(y2)[s], lat(y2)[s], col='black', pch='.', cex=cex*0.1)
      slonlat <- cbind(round(lon(y2)[s], decimals), 
                       round(lat(y2)[s], decimals))
      slonlat <- slonlat[!duplicated(slonlat),]
      points(slonlat[,1], slonlat[,2], col='black', pch='.', cex=cex.robust)
    } else if(inherits(Z,"station")) {
      sig <- zload(src="RCM", param=attr(Z,"param"), FUN="postrend",
                   season=attr(Z,"season"), scenario=attr(Z,"scenario"))
      s <- (sig>=threshold | sig <= (1-threshold)) & !is.na(m2)
      slonlat <- cbind(round(lon(sig)[s], decimals), 
                      round(lat(sig)[s], decimals))
      slonlat <- slonlat[!duplicated(slonlat),]
      points(slonlat[,1], slonlat[,2], col='black', pch='.', cex=cex.robust)
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


stplot12 <- function(z1, z2, im1=NULL, im2=NULL,
                     #MET1="ESD", MET2="ESD", 
                     is=NULL, it=NULL, 
                     xlim=NULL, ylim=NULL, 
                     xlab=NULL, ylab=NULL, main=NULL,
                     ylim2=NULL, ylab2=NULL,
                     mar=c(2,2,2,1), mgp=c(2.5,1,0.5),
                     cex.axis=1, cex.lab=1.2, cex.main=1.2,
                     new=FALSE, add=FALSE, verbose=FALSE, ...) {
  if(verbose) print('--- Plot individual station ---')
  if(!inherits(z1, "station")) z1 <- as.station(z1)
  if(!inherits(z2, "station")) z2 <- as.station(z2)
  if(is.null(is)) {
    is <- 1
  } else {
    is <- grep(cleanstr(is, "[A-Z]"), stid(z1))
    if(length(is)==0) is <- 1
  }
  
  if(is.null(it)) {
    it <- c(1950, 2100)
  } else {
    it <- range(as.numeric(it))
  }
  
  y1 <- subset(z1, is=is, it=it, im=im1)
  y2 <- subset(z2, is=is, it=it, im=im2)
  
  if(is.null(xlim)) xlim <- range(c(index(y1), index(y2)))
  if(is.null(ylim)) {
    ylim <- range(c(range(y1), range(y2)))
    ylim <- ylim + c(-1,1)*diff(ylim)*0.1
  }
  if(is.null(xlab)) {
    if(is.years(index(y1))) {
      xlab <- "Years"
    } else if(is.dates(index(y1))) {
      xlab <- "Dates"
    } else xlab <- " "
  } 
  if(is.null(ylab)) ylab <- paste0(attr(y1,"longname"), 
                                   "  (", attr(y1, "unit"), ")")
  if(is.null(main)) main <- " "
  col1 <- "darkorange3"
  col2 <- "blueviolet"

  if(inherits(y1, "dsensemble")) {
    mean_1 <- apply(y1, 1, mean)
    min_1 <- apply(y1, 1, min)
    max_1 <- apply(y1, 1, max)
    q5_1 <- apply(y1, 1, q5)
    q95_1 <- apply(y1, 1, q95)
  } else {
    mean_1 <- y1
    min_1 <- attr(Z, "min")
    max_1 <- attr(Z, "max")
    q5_1 <- rep(NULL, length(mean_1))
    q95_1 <- rep(NULL, length(mean_1))
  }
  
  if(inherits(y2, "dsensemble")) {
    mean_2 <- apply(y2, 1, mean)
    min_2 <- apply(y2, 1, min)
    max_2 <- apply(y2, 1, max)
    q5_1 <- apply(y2, 1, q5)
    q95_1 <- apply(y2, 1, q95)
  } else {
    mean_2 <- y2
    min_2 <- subset(attr(y2, "min"), is=is, it=it)
    max_2 <- subset(attr(y2, "max"), is=is, it=it)
    q5_2 <- rep(NA, length(mean_2))
    q95_2 <- rep(NA, length(mean_2))
  }
  
  data <- data.frame(date = c(index(y1), index(y2)),
                     ensemble = c(rep("A", length(mean_1)), 
                                  rep("B", length(mean_2))),
                     mean = c(mean_1, mean_2), 
                     min = c(min_1, min_2), 
                     max = c(max_1, max_2), 
                     q5 = c(q5_1, q5_2), 
                     q95 = c(q95_1, q95_2))
  cols <- c("darkgreen", "orange")
  p <- ggplot(data = data, aes(x = date, y = mean), size = 2) + 
    geom_ribbon(aes(ymin = min, ymax = max, fill = ensemble), alpha=0.5) +
    geom_line(aes(x = date, y = mean, color=ensemble), size = 1) + 
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) + 
    xlab(xlab) + ylab(ylab) + ggtitle(main) +
    theme_minimal()
  if(attr(y1, "unit")!=attr(y2, "unit")) {
    if(is.null(ylab2)) ylab2 <- paste0(attr(y2, "longname")[1], 
                                       "  (", attr(y2,"unit")[1], ")")
    if(is.null(ylim2)) ylim2 <- diff(range(y2))/diff(range(y1))
    p <- p + scale_y_continuous(
      name = ylab, limits = ylim,
      sec.axis = sec_axis(~.*ylim2, name=ylab2)) +
      theme(axis.text.y.right =  element_text(color = cols[2]),
            axis.title.y.right = element_text(color= cols[2])
    )
  }
  p

}