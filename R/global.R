## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(shinydashboard)
library(esd)
#source("~/git/esd/R/crossval.R")
#source("~/git/esd/R/plot.R")
#source("~/git/esd/R/trend.dsensemble.R")
#source("~/git/esd/R/as.station.R")
#source("~/git/esd/R/subset.R")
#source("~/git/esd/R/expandpca.R")
#source("~/git/esd/R/aggregate.dsensemble.R")

datelist <- list("1950-2100" = c(1950,2100),
                 "1950-1979" = c(1950,1979),
                 "1981-2010" = c(1981,2010),
                 "2031-2060" = c(2031,2060),
                 "2071-2100" = c(2071,2100))

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
      attr(Z, "unit") <- "fraction of days"
    }
    return(Z)
  } else return(NULL)
}

sliderange <- function(param=NULL, FUN=NULL) {
  if(is.null(param)) param <- "t2m"
  if(is.null(FUN)) FUN <- "mean"
  if(FUN=="trend") {
    minmax <- switch(param,
                     "pr"=c(-0.8,0.8),
                     "fw"=c(-0.04,0.04),
                     "mu"=c(-0.8,0.8),
                     "t2m"=c(-1.5,1.5),
                     "tsd"=c(0.5,0.5),
                     c(-1,1))
    x <- switch(param,
                "pr"=c(-0.4,0.4),
                "fw"=c(-0.01,0.01),
                "mu"=c(-0.1,0.1),
                "t2m"=c(-1,1),
                "tsd"=c(-0.05,0.05),
                c(-0.6,0.6))
  } else {
    minmax <- switch(param,
                     "pr"=c(0,25),
                     "fw"=c(0,1),
                     "mu"=c(0,25),
                     "t2m"=c(-40,40),
                     "tsd"=c(0,15),
                     c(-30,50))
    x <- switch(param,
                "pr"=c(0,15),
                "fw"=c(0,1),
                "mu"=c(0,15),
                "t2m"=c(-15,30),
                "tsd"=c(0,5),
                c(-15,30))
  }
  return(list("x"=x, "minmax"=minmax)) 
}


## Gridded maps
mapgridded <- function(Z, FUN='mean', FUNX='mean', eof=TRUE,
                       it=NULL, im=NULL, main=NULL, colbar=NULL,
                       show.stations=TRUE, pch=19, cex=1, lwd=1,
                       show.robustness=TRUE,trends=NULL,threshold=0.9,
                       verbose=FALSE) { 
  if(verbose) print('output$maps')
  if(is.null(it)) it <- range(year(Z[[3]]))
  if(verbose) print(paste('it=',paste(it,collapse=' - ')))
  z <- subset(Z, it=it, im=im)
  if(verbose) print(names(z))
  
  if(is.null(main)) {
    main <- paste(FUN,tolower(season(Z$pca)[1]),
                  attr(Z,"variable")[1],'for',it[1],'-',it[2],
                  '\ndownscaled with', 
                  gsub("_", " ", 
                       gsub("_mon|.nc", "", basename(attr(Z, "predictor_file")))),
                  '&', toupper(attr(Z, "scenario")), 
                  paste0('(',length(z)-3),'model runs)')
  }
  
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
  map(m,main=main,type='fill',colbar=colbar,FUN="mean",new=FALSE)
  if(show.stations) {
    if (FUNX=='mean') {
      ## Faster response for ensemble mean
      y <- expandpca(z,FUN=FUN,FUNX=FUNX,eof=FALSE,verbose=TRUE)
      m <- map(y,FUN='mean',plot=FALSE)
    } else {
      y <- aggregate.dsensemble(z,FUN=FUNX,eof=FALSE,verbose=TRUE)
      m <- map(y,FUN=FUN,plot=FALSE)
    }
    ## calculate color 
    colbar <- colbar.ini(m, colbar=colbar)
    if (verbose) print('Set colour scheme')
    wr <- round(strtoi(paste('0x',substr(colbar$col,2,3),sep=''))/255,2)
    wg <- round(strtoi(paste('0x',substr(colbar$col,4,5),sep=''))/255,2)
    wb <- round(strtoi(paste('0x',substr(colbar$col,6,7),sep=''))/255,2)
    col <- rep(colbar$col[1],length(m))
    for (i in 1:length(m)) {
      ii <- round(approx(0.5*(colbar$breaks[-1]+colbar$breaks[-length(colbar$breaks)]),
                         1:length(colbar$col),
                         xout=as.vector(m)[i],rule=2)$y)
      if (is.finite(ii)) {
        if (ii < 1) ii <- 1
        if (ii > length(colbar$col)) ii <- length(colbar$col)
        col[i] <- rgb(wr[ii],wg[ii],wb[ii],0.7)
      } else col[i] <- rgb(0.5,0.5,0.5,0.2)
    }
    points(lon(y), lat(y), col=col, pch=19, cex=cex, lwd=lwd)
    if(show.robustness & FUN=='trend' & !is.null(trends)) {
      sig <- apply(trends[im,], 2, function(x) max(sum(x>0), sum(x<0))/length(x))
      s <- sig>=threshold
      points(lon(y)[s], lat(y)[s], col='black', pch=21, cex=cex, lwd=lwd)
    }
  }
}


## Plot individual station
stplot <- function(z, is=NULL, it=NULL, im=NULL, main=NULL, 
                   ylim=NULL, verbose=FALSE) {
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
  plot(y, target.show=FALSE, legend.show=FALSE, new=FALSE, #main=main,
       xrange=range(attr(z, "longitude"))+c(-5,5), ylim=ylim,
       yrange=range(attr(z, "latitude"))+c(-2,2), map.show=TRUE,
       mar=c(3,5,4,4))
}


## splits the list names into different categories
## e.g. t2m.ecad.107rcp45.mam or
##      Finland.fw.ecad.rcp26.djf
namesplit <- function(x) {
  info <- unlist(strsplit(gsub("dse.kss.|.rda", "", x), "\\.|\\_"))
  it <- info[[length(info)]]
  sce <- info[[length(info)-1]]
  if(grepl(paste0("[0-9]{1,3}",c("rcp","ssp"), collapse="|"), x)) {
    nem <- gsub("rcp.*.|ssp.*.", sce)
    sce <- substr(sce, regexpr("rcp|ssp"), nchar(sce))
  } else nem <- NULL
  src <- info[[length(info)-2]]
  varid <- info[[length(info)-3]]
  if(length(info)<5) {
    region <- NULL
  } else {
    region <- info[[length(info)-4]]
  }
  return(list(region=region,varid=varid,src=src,nem=nem,sce=sce,it=it))
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

## Preparations - get a list of available results
files.all <- list.files(path='data',pattern='dse.kss.',full.names = TRUE)
files <- files.all

file.trends <- list.files(path='data',pattern='trends.rda',full.names = TRUE)
load(file.trends)
T4 <- trends
for(region in names(trends)) {
  for(var in names(trends[[region]])) {
    for(scenario in names(trends[[region]][[var]])) {
      for(it in names(trends[[region]][[var]][[scenario]])) {
        if(! any(grepl(region, files) & grepl(var, files) & 
                 grepl(scenario, files) & grepl(it, files)) ) {
          T4[[region]][[var]][[scenario]][[it]] <- NULL
        }
      }
    }
  }
}
rm("trends")

nms <- gsub('data/dse.kss.','',files)
nms <- gsub('_[0-9]{4}-[0-9]{4}.rda','',nms)
names(files) <- nms

verbose <- FALSE
if(verbose) print('List names:')
if(verbose) print(nms)
n <- length(nms)
regs <- rep('?',n); vars <- rep('?',n); srcs <- vars; 
nems <- vars; sces <- vars; its <- vars
for (i in 1:n) {
  regs[i] <- namesplit(nms[i])$region
  vars[i] <- namesplit(nms[i])$varid
  srcs[i] <- namesplit(nms[i])$src
  sces[i] <- namesplit(nms[i])$sce
  its[i] <- namesplit(nms[i])$it
  #nems[i] <- length(Z4[[i]])-3#namesplit(nms[i])$nem
  nems[i] <- length(attr(T4[[regs[i]]][[vars[i]]][[sces[i]]][[its[i]]], "model_id"))
}
#print('Categories for UI selection:')
cats <- data.frame(region=regs,var=vars,src=srcs,nem=nems,sce=sces,it=its)

locs <- list()
for(region in unique(regs)) {
  for(var in unique(vars)) {
    locs[[region]][[var]][["loc"]] <- loc(T4[[region]][[var]][[1]][[1]])
    locs[[region]][[var]][["stid"]] <- stid(T4[[region]][[var]][[1]][[1]])
    locs[[region]][[var]][["label"]] <- paste0(loc(T4[[region]][[var]][[1]][[1]]),
                                    " (",stid(T4[[region]][[var]][[1]][[1]]),")")
  }
}

gcmnames <- list()
for(scenario in unique(sces)) {
  for(var in unique(vars)) {
    gcmnames[[var]][[scenario]] <- attr(T4[[1]][[var]][[scenario]][[1]], "model_id")
  }
}

scenarios <- unique(sces)
scenarios <- paste(toupper(scenarios), 
                   c("(CMIP5)","(CMIP6)")[as.numeric(grepl("SSP", scenarios))+1])
names(scenarios) <- unique(sces)

