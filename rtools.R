## Kajsa Parding, Met Norway, 2023-04-24
## R-shiny app that presents downscaled results. The results include the CMIP5 and CMIP6 ensembles simulations
## for a number of stations and for the four different seasons. The empirical-statistical downscaled results 
## used PCAs and EOFs to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs 
## to corresponding information in the form of station series or gridded maps.

## Remove special characters from string
cleanstr <- function(x, remove=NULL) {
  y <- gsub("[[:punct:]]|\\s", "", x)
  if(!is.null(remove)) y <- gsub(remove, "", y)
  return(y)
}

## Capitalize the first letter in a word (or all words in a sentence if all=TRUE) 
first2upper <- function(x, lower=TRUE, all=TRUE) {
  if(lower) {
    up <- function(a) if(nchar(a)==1 | grepl("\\b[I|V|X|L|C|D|M]{1,20}\\b",a)) return(toupper(a)) else 
                          return(paste0(toupper(substr(a,1,1)), tolower(substr(a,2,nchar(a)))))
  } else {
    up <- function(a) if(nchar(a)==1 | grepl("\\b[I|V|X|L|C|D|M]{1,20}\\b",a)) return(toupper(a)) else 
                          return(paste0(toupper(substr(a,1,1)), substr(a,2,nchar(a))))
  }
  allup <- function(b) paste(sapply(unlist(strsplit(b," ")), up), collapse=" ")
  if(all) y <- sapply(x, allup) else y <- sapply(x, up)
  return(y)
}

## Long and short variable names
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

## Long and short season names
seasonname <- function(x, long=TRUE) {
  if(grepl("winter|djf",tolower(x))) {
    y <- switch(as.numeric(long)+1, "1"="djf", "2"="winter (Dec - Feb)")
  } else if(grepl("spring|mam",tolower(x))) {
    y <- switch(as.numeric(long)+1, "1"="mam", "2"="spring (Mar - may)")
  } else if(grepl("summer|jja",tolower(x))) {
    y <- switch(as.numeric(long)+1, "1"="jja", "2"="summer (Jun - Aug)")
  } else if(grepl("fall|autumn|son",tolower(x))) {
    y <- switch(as.numeric(long)+1, "1"="son", "2"="fall (Sep - Nov)")
  } else if(grepl("annual|all|",tolower(x))) {
    y <- "annual"
  } else y <- NULL
  return(y)
}

## Long and short emission scenario names
scenarioname <- function(x, long=TRUE) {
  y <- cleanstr(tolower(x))
  if(grepl("ssp",y)) {
    i <- regexpr("ssp", y)
    n <- 5
    y <- substr(y, i, i+n)
  } else if(grepl("rcp",y)) {
    i <- regexpr("rcp", y)
    n <- 4
    y <- substr(y, i, i+n)
  }
  if(long) y <- switch(y, 
                "rcp26"="low emission scenario (RCP2.6)",
                "rcp45"="medium emission scenario (RCP4.5)",
                "rcp85"="high emission scenario (RCP8.5)",
                "ssp119"="very low emission scenario (SSP1 1.9)",
                "ssp126"="low emission scenario (SSP1 2.6)",
                "ssp245"="medium emission scenario (SSP2 4.5)",
                "ssp370"="medium-high emission scenario (SSP3 7.0)",
                "ssp585"="high emission scenario (SSP5 8.5)",
                x)
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

patterns <- function(param="t2m", src="ESD", scenario="rcp85", 
                     season="djf", FUNX="mean", FUN=NULL,
                     pattern.esd="dse.kss.Nordic",
                     pattern.rcm=c("ens","EUR-11","remapbil"))  {
  if(grepl("esd|statistical",tolower(src))) {
    pattern <- pattern.esd
  } else if (grepl("rcm|dynamical",tolower(src))) {
    pattern <- pattern.rcm
    if(param %in% c("fw","mu","precip")) param <- "pr"
    if(param %in% c("t2m","tsd")) param <- "tas"
    if(!is.null(FUNX)) pattern <- c(pattern, FUNX)
    if(!is.null(FUN)) pattern <- c(pattern, FUN)
  }
  pattern <- c(pattern, param, scenario, season)
  return(pattern)
}


zload <- function(path="data/esd", type="field", 
                  src="ESD", param="t2m", season="djf", scenario="ssp585",
                  FUNX="mean", FUN=NULL, 
                  pattern.esd="dse.kss.Nordic",
                  pattern.rcm=c("ens","EUR-11","remapbil"),
                  verbose=FALSE) {
  if(verbose) print("zload")
  pattern <- patterns(src=src, param=param, season=season, scenario=scenario,
                      FUNX=FUNX, FUN=FUN, pattern.esd=pattern.esd,
                      pattern.rcm=pattern.rcm)
  files <- list.files(path, pattern=pattern[1], full.names=TRUE)
  i <- eval(parse(text=paste(paste0("grepl('",tolower(pattern),
                                    "', tolower(files))"), 
                             collapse=" & ")))
  if(any(grepl("trend",pattern))) {
    pattern_trend <- pattern[grepl("trend",pattern)]
    i <- i & grepl(paste0("_",tolower(pattern_trend),"_"), 
                   tolower(files))
  } else {
    if(sum(i)>1) i <- i & !grepl("trend",files)
  }
  
  ## KMP 2023-10-16: Hacky solution to identifying multiple files
  ## ...dse.kss.Nordic.t2m.ecad.ERA5t2m... 
  ## ...dse.kss.Nordic.tsd.ecad.ERA5t2m...
  ## when pattern includes t2m and this is in both files.
  if(sum(i)>1 & "t2m" %in% pattern) i <- i & grepl("\\.t2m\\.", tolower(files))
  
  if(sum(i)==1) {
    if(grepl(".rda", files[i]) & grepl("dse", files[i])) {
      if(verbose) print(paste("load file",files[i]))
      load(files[i])
      Z <- xmembers(Z, verbose=verbose)
      attr(Z, "season") <- season(Z$pca)
    } else if(grepl(".nc", files[i]) & grepl("ens", files[i])) {
      if(verbose) print(paste("Getting",param,"from",files[i]))
      if(param %in% c("fw","mu","precip")) param <- "pr"
      if(param %in% c("t2m","tsd")) param <- "tas"
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
    #if (FUNX == "sd") {
    #  cz <- coredata(Z)
    #  coredata(Z) <- sqrt(cz) # var to sd
    #}
  } else {
    Z <- NULL
    if(verbose) print(paste("Could not figure out what or how to open the",
                  length(i), "files", paste(files[i], collapse=", ")))
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

mask.station <- function(x,land=FALSE,verbose=FALSE) {
  if(verbose) print("mask.station")
  data(etopo5, envir = environment())
  h <- subset(etopo5,is=list(lon=range(lon(x)),lat=range(lat(x))))
  hx <- sapply(seq_along(lon(x)), function(i) {
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
        y_B <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=FALSE,verbose=verbose)
        m2 <- map.station(y_B,FUN='mean',plot=FALSE) 
      }
    } else {
      y <- aggregate.dsensemble(z,FUNX=FUNX,eof=eof,verbose=verbose)
      m <- map(y,FUN=FUN,plot=FALSE)
      if(show.stations | show.robustness) {
        y_B <- aggregate.dsensemble(z,FUN=FUNX,eof=FALSE,verbose=verbose)
        m2 <- map.station(y_B,FUN=FUN,plot=FALSE)
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
  cb <- colbar.ini(m,FUN=NULL,colbar=colbar,verbose=verbose)
  
  ## Set up map
  par0 <- par()
  par(xaxt="n",yaxt="n",bty="n")
  plot(range(lon(m)),range(lat(m)),type="n",xlim=xlim,ylim=ylim,new=FALSE,
       xlab="",ylab="")
  
  if(diff(range(xlim))>10) {
    dlon <- round(round(diff(range(xlim))/4)/5)*5
  } else if(diff(range(xlim))<2) {
    dlon <- round(diff(range(xlim))/3, 2)
  } else {
    dlon <- round(diff(range(xlim))/4)
  }
  if(diff(range(ylim))>10) {
    dlat <- round(round(diff(range(ylim))/4)/5)*5
  } else if(diff(range(ylim))<2) {
    dlat <- round(diff(range(ylim))/3, 2)
  } else {
    dlat <- round(diff(range(ylim))/4)
  }
  par(xaxt="s",yaxt="s",las=1,col.axis='grey',col.lab='grey',
      cex.lab=1,cex.axis=1,xpd=FALSE)
  axis(3,seq(floor(par("xaxp")[1]/dlon)*dlon,par("xaxp")[2],by=dlon),col='grey')
  axis(4,seq(floor(par("yaxp")[1]/dlat)*dlat,par("yaxp")[2],by=dlat),col='grey')
  grid()
  
  data("geoborders", envir = environment())
  lines(geoborders$x,geoborders$y,col='grey', lwd=1.5)
  lines(attr(geoborders,'border')$x,attr(geoborders,'border')$y,
        col=adjustcolor('grey', alpha.f=0.7), lwd=0.75)
  
  ## Add field 
  if (show.field) {
    image(lon(m),lat(m),m,xlab="",ylab="",add=TRUE,
          col=cb$col,breaks=cb$breaks,xlim=xlim,ylim=ylim)
  }
  
  ## Add stations
  if (show.stations) {
    icol <- apply(as.matrix(m2),2,findInterval,cb$breaks)
    col <- cb$col[icol]
    #points(lon(m2), lat(m2), pch = pch,
    #       bg=cb$col[icol], col=col, cex=cex)
    xyz <- cbind(as.vector(lon(m2)), 
                 as.vector(lat(m2)), 
                 as.vector(m2))
    colnames(xyz) <- c('X', 'Y', 'Z')
    e <- extent(xyz[,1:2])
    r <- raster(e, ncol=160, nrow=160)
    m2r <- rasterize(xyz[, 1:2], r, xyz[,3], fun=mean)
    image(m2r, xlab="", ylab="", add=TRUE,
          col=cb$col, breaks=cb$breaks, xlim=xlim, ylim=ylim)
  }
  
  varnm <- attr(m,"variable")
  varnm <- gsub(" ","~",varnm)
  varnm <- switch(varnm, "precip"="Precipitation",
                  "pr"="Preciptiation", "fw"="Wet-day~frequency",
                  "mu"="Wet-day~mean~precipitation",
                  "t2m"="Temperature", "tas"="Temperature", varnm)
  unitx <- gsub(" ","~",attr(m, "unit"))
  label <- eval(parse(text=paste('expression(',varnm,'~(',unitx,')',')')))

  ## Add colorbar
  dy <- diff(ylim)*0.07
  below <- c(min(xlim), min(ylim),#min(ylim)-dy/2, 
             max(xlim), min(ylim)+dy)#min(ylim)+dy/2)
  rect(below[1], below[2]-2*dy, below[3], below[4], 
       col = "white", border = "white")
  col.bar(below[1],below[2],below[3],below[4],
          cb$breaks, horiz=TRUE, pch=15, v=1, h=1,
          col=cb$col, cex=2, cex.lab=1,
          type="r", verbose=FALSE, vl=1, border=FALSE)
  title(sub = label, line = 0.75, cex.sub = 1)

  ## Add trend robustness
  if(show.robustness & FUN=='trend') { 
    decimals <- 0
    cex.robust <- 2
    if(inherits(Z,"dsensemble") & !is.null(trends)) {
      sig <- apply(trends[im,], 2, function(x) max(sum(x>0), sum(x<0))/length(x))
      s <- sig>=threshold | sig <= (1-threshold)
      slonlat <- cbind(round(lon(y_B)[s], decimals), 
                       round(lat(y_B)[s], decimals))
      slonlat <- slonlat[!duplicated(slonlat),]
      points(slonlat[,1], slonlat[,2], col='black', pch='.', cex=cex.robust)
    } else if(inherits(Z,"station")) {
      sig <- trends
      s <- (sig>=threshold | sig <= (1-threshold)) & !is.na(m2)
      slonlat <- cbind(round(lon(sig)[s], decimals), 
                      round(lat(sig)[s], decimals))
      slonlat <- slonlat[!duplicated(slonlat),]
      points(slonlat[,1], slonlat[,2], col='black', pch='.', cex=cex.robust)
    }
  }
}



stplot <- function(z1, z2, im1=NULL, im2=NULL,
                     #MET1="ESD", MET2="ESD", 
                     is=NULL, it=NULL, 
                     xlim=NULL, ylim=NULL, 
                     xlab=NULL, ylab=NULL, main=NULL,
                     ylim2=NULL, ylab2=NULL,
                     label1=NULL, label2=NULL,
                     mar=c(2,2,2,1), mgp=c(2.5,1,0.5),
                     cex.axis=1, cex.lab=1.2, cex.main=1.2,
                     new=FALSE, add=FALSE, verbose=FALSE, ...) {
  if(verbose) print('--- Plot individual station ---')
  if(!inherits(z1, "station")) z1 <- as.station(z1)
  if(!inherits(z2, "station")) z2 <- as.station(z2)
  
  if(is.null(is)) {
    is <- 1
  } else {
    is <- grep(cleanstr(tolower(is), "[a-z]"), stid(z1))
    if(length(is)==0) is <- 1
  }
  
  if(is.null(it)) {
    it <- c(1950, 2100)
  } else {
    it <- range(as.numeric(it))
  }
  
  y_A <- subset(z1, is=is, it=it, im=im1)
  y_B <- subset(z2, is=is, it=it, im=im2)
  
  if(is.null(xlim)) xlim <- range(c(index(y_A), index(y_B)))
  if(is.null(ylim)) {
    ylim <- range(c(range(y_A), range(y_B)))
    ylim <- ylim + c(-1,1)*diff(ylim)*0.1
  }
  if(is.null(xlab)) {
    if(is.years(index(y_A))) {
      xlab <- "Years"
    } else if(is.dates(index(y_A))) {
      xlab <- "Dates"
    } else xlab <- " "
  } 
  if(is.null(ylab)) ylab <- paste0(gsub("_"," ",attr(y_A,"longname")), 
                                   "  (", attr(y_A, "unit"), ")")
  if(is.null(main)) main <- " "
  
  if(is.null(label1)) label1 <- "Ensemble A"
  if(is.null(label2)) label2 <- "Ensemble B"
    
  cols <- c("red", "royalblue","yellow")

  if(inherits(y_A, "dsensemble")) {
    mean_A <- apply(y_A, 1, mean)
    min_A <- apply(y_A, 1, min)
    max_A <- apply(y_A, 1, max)
    q5_A <- apply(y_A, 1, q5)
    q95_A <- apply(y_A, 1, q95)
  } else {
    mean_A <- y_A
    min_A <- attr(Z, "min")
    max_A <- attr(Z, "max")
    q5_A <- rep(NULL, length(mean_A))
    q95_A <- rep(NULL, length(mean_A))
  }
  
  if(inherits(y_B, "dsensemble")) {
    mean_B <- apply(y_B, 1, mean)
    min_B <- apply(y_B, 1, min)
    max_B <- apply(y_B, 1, max)
    q5_B <- apply(y_B, 1, q5)
    q95_B <- apply(y_B, 1, q95)
  } else {
    mean_B <- y_B
    min_B <- subset(attr(y_B, "min"), is=is, it=it)
    max_B <- subset(attr(y_B, "max"), is=is, it=it)
    q5_B <- rep(NA, length(mean_B))
    q95_B <- rep(NA, length(mean_B))
  }
  
  d <- unique(index(y_A), index(y_B))
  sm <- data.frame(matrix(NA, ncol=7, nrow=length(d)))
  colnames(sm) <- c("date","mean_A","min_A","max_A",
                    "mean_B","min_B","max_B")
  i1 <- index(y_A) %in% d; i2 <- index(y_B) %in% d
  sm[,1] <- d
  sm[,2] <- smooth.spline(index(y_A), mean_A)$y[i1]
  sm[,3] <- smooth.spline(index(y_A), min_A)$y[i1]
  sm[,4] <- smooth.spline(index(y_A), max_A)$y[i1]
  sm[,5] <- smooth.spline(index(y_B), mean_B)$y[i2]
  sm[,6] <- smooth.spline(index(y_B), min_B)$y[i2]
  sm[,7] <- smooth.spline(index(y_B), max_B)$y[i2]
  plot_ly(x = sm$date, y = sm$mean_A, type = "scatter", 
          mode = "lines", color = I(cols[1]), name=label1,
          line=list(color=I(cols[1]), width=4)) %>%
  add_ribbons(x=sm$date, ymin=sm$min_A, ymax=sm$max_A, 
              name="min-max", color=I(cols[1]),
              line=list(color=I(cols[[1]]), opacity=0.4, width=0)) %>%
  add_trace(x=sm$date, y=sm$mean_B, color=I(cols[2]), 
            mode="lines", name=label2, 
            line=list(color=I(cols[2]), width=4, dash="dash")) %>%
  add_ribbons(p, x=sm$date, ymin=sm$min_B, ymax=sm$max_B, 
              name="min-max", 
              color=I(cols[2]), 
              line=list(color=I(cols[2]), opacity=0.4, width=0)) %>%
  layout(title = main, plot_bgcolor = "white", xaxis = list(title = xlab), 
         yaxis = list(title = ylab, range = ylim),
         legend = list(x=0, y=.95, traceorder="normal",
                       font = list(family = "sans-serif", size=12, color="black")))

}

