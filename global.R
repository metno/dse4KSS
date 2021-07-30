## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)

## Estimate the probabilities for trend in observation is within the population trends based on of downscaled results
## zoo objects are slow so extract the core data
trendscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  ty <- trend.coef(y)
  tX <- apply(X,2,FUN='trend.coef')
  score <- pnorm(ty,mean=mean(tX),sd=sd(tX))
  return(c(score,lon(x),lat(x)))
}

## Estimate the probabilities for observed values are within the 90% conf. int. of population of downscaled results
## zoo objects are slow so extract the core data
varscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  nX <- sum(apply(cbind(y,X),1,FUN=function(x) x[1] < quantile(x[-1],probs=0.05) | x[1] > quantile(x[-1],probs=0.95)))
  score <- pbinom(nX,size=length(y),prob=0.1)
  return(c(score,lon(x),lat(x)))
}

## splits the list names into different categories
## e.g. t2m.ecad.107rcp45.mam
namesplit <- function(x) {
  dots <- gregexpr('.',x, fixed = TRUE)[[1]]
  varid <- substr(x,1,dots[1]-1)
  src <- substr(x,dots[1]+1,dots[2]-1)
  if (regexpr('rcp',x)>0) {
    sce <- substr(x,dots[3]-5,dots[3]-1)
    nem <- as.integer(substr(x,dots[2]+1,dots[3]-6))
  } else if (regexpr('ssp',x)>0) {
    sce <- substr(x,dots[3]-6,dots[3]-1)
    nem <- as.integer(substr(x,dots[2]+1,dots[3]-7))
  }
  it <- substr(x,dots[3]+1,nchar(x))
  return(list(varid=varid,src=src,nem=nem,sce=sce,it=it))
}

## Script to extract members from DSensemble with 
xmembers <- function(X,length=NULL,verbose=TRUE) {
  x <- X
  info <- X$info; X$info <- NULL
  pca <- X$pca; X$pca <- NULL; eof <- X$eof; X$eof <- NULL
  n <- length(names(X))
  ## Quality control
  if (verbose) print(paste('Before quality control: original number of members=',n))
  for (i in seq(n,1,by=-1)) {
    #print(range(X[[i]],na.rm=TRUE)); print(dim(X[[i]]))
    if (sum(is.finite(X[[i]]))==0) {
      print(paste(i,'Remove bad results')); X[[i]] <- NULL 
    } else if (max(abs(X[[i]]),na.rm=TRUE) > 10)  {
      print(paste(i,'Remove suspect results')); X[[i]] <- NULL
    }
  }
  n <- length(X)
  if (verbose) print(paste('After quality control: new number of members=',n))
  memsiz <- rep("?",n)
  for (i in 1:n) 
    memsiz[i] <- paste(dim(X[[i]]),collapse='x')
  memsiztab <- table(memsiz)
  memcats <- rownames(memsiztab)
  if (verbose) {print(memsiztab); print(paste0('(',1:length(memcats),') ',memcats,collapse=' - '))}
  
  if (is.null(length)) {
    if (verbose) memkeep <- memcats[memsiztab == max(memsiztab)][1] else
      memkeep <- rownames( memsiztab)[as.numeric(memsiztab)==max(as.numeric(memsiztab))]
  } else memkeep <- memcats[grep(paste0(as.character(length),'x'),memcats)]
  if (verbose) print(memkeep)
  im <- sort((1:n)[-grep(memkeep,memsiz)],decreasing = TRUE)
  if (verbose) print(im)
  for (ix in im) x[[ix+2]] <- NULL
  n <- length(names(x))
  if (verbose) print(paste('New length of X is',n))
  return(x)
}


## Preparations - get a list of available results
files <- list.files(path='data',pattern='dse.kss.',full.names = TRUE)
#print('Results files:')
#print(files)

Z4 <- list()
# load('data/dse.kss.t2m.rcp45.djf.eof.rda')
# Z4$t2m.djf.45 <- Z
#dse.kss.fw.ecad.65rcp26.jja_1950-2100.rda     
#dse.kss.t2m.ecad.81rcp85.jja_1950-2100.rda
for (i in 1:length(files)) {
  load(files[i])
  nm <- sub('data/dse.kss.','',files[i])
  nm <- sub('_1950-2100.rda','',nm)
  nm <- sub('_1950-2055.rda','',nm)
  Z4[[nm]] <- Z
}

nms <- names(Z4)

print('List names:')
print(nms)
n <- length(nms)
vars <- rep('?',n); srcs <- vars; nems <- vars; sces <- vars; its <- vars
for (i in 1:n) {
  vars[i] <- namesplit(nms[i])$varid
 srcs[i] <- namesplit(nms[i])$src
 nems[i] <- namesplit(nms[i])$nem
 sces[i] <- namesplit(nms[i])$sce
 its[i] <- namesplit(nms[i])$it
}
#print('Categoris for UI selection:')
cats <- data.frame(var=vars,src=srcs,nem=nems,sce=sces,it=its)
#print(cats)

## Here are the choices for UI
params.ui <- rownames(table(vars))
# for (i in 1:length(params.ui)) params.ui[i] <- switch(params.ui[i],'t2m'='mean temperature',
#                    'tsd'='standard deviation',
#                    'mu'='wet-day mean precipitation','fw'='wet-day frequency')
src.ui <- toupper(rownames(table(srcs)))
sces.ui <- toupper(rownames(table(sces)))
its.ui <- rownames(table(its))
## Need to include textOutput("statisticslabel")

# load('data/t2m.nordic.rda')
# load('data/rr.nordic.rda')

