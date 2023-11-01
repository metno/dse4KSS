## K Parding, Met Norway, 2022-09-27
## R-shiny app that presents empirical-statistical downscaled results.

library(shiny)
library(shinydashboard)
library(esd)
library(ggplot2)
library(leaflet)
library(plotly)
library(raster)
source("~/git/esd/R/distAB.R")

## Initial choice of region, variable etc
var0 <- "t2m"
sce0 <- "rcp45"
seas0 <- "jja"
path.data <- "data"
path.esd <- file.path(path.data, "esd")
path.rcm <- file.path(path.data, "rcm")
pattern.esd <- "dse.kss.Nordic"
pattern.rcm <- c("ens","EUR-11","remapbil")

source("rtools.R")
source("calculate.trends.R")

datelist <- list(c(1951,2100), c(1951,1980), c(1991,2010),
                 c(2041,2070), c(2071,2100))
names(datelist) <- sapply(datelist, function(x) paste0(x[1],"-",x[2]))

sourcelist <- c("empirical statistical downscaling (MetNo ESD)", 
                "dynamical downscaling (CORDEX RCM)")

## Meta data from RCM files 
files.rcm <- list.files(path.rcm, pattern=c(pattern.rcm))
files.rcm <- files.rcm[grepl("ensmean",files.rcm) & 
                         !grepl("trend",tolower(files.rcm)) &
                         grepl("djf",tolower(files.rcm))]
rcmnames <- list()
for(f in files.rcm) {
  nc <- nc_open(file.path(path.rcm, f))
  var <- names(nc$var)[length(nc$var)]
  h <- ncatt_get(nc, 0, "history")$value
  h <- unlist(strsplit(h, " "))
  h <- h[grepl(paste0(var,".*.nc"), h) &
           grepl(paste0(var,".*.r[0-9]{1,3}i[0-9]{1,3}p[0-9]{1,3}.*."), h) &
           grepl(paste0(var,".*.rcp[0-9]{2}.*."), h) &
           !grepl(paste0("ensmean"), h)]
  scenario <- sapply(h, function(x) substr(x, regexpr("rcp[0-9]{2}",x), regexpr("rcp[0-9]{2}",x)+4))
  gcm <- sapply(h, function(x) substr(x, regexpr("EUR-11_",x)+7 , regexpr("rcp[0-9]{2}",x)-2))
  rip <- sapply(h, function(x) substr(x, regexpr("r[0-9]{1,3}i[0-9]{1,3}p[0-9]{1,3}",x), 
                                      regexpr("r[0-9]{1,3}i[0-9]{1,3}p[0-9]{1,3}",x)+4))
  rcm <- sapply(h, function(x) substr(x, regexpr("r[0-9]{1,3}i[0-9]{1,3}p[0-9]{1,3}",x) + 
                                        attr(regexpr("r[0-9]{1,3}i[0-9]{1,3}p[0-9]{1,3}",x), "match.length") + 1, 
                                      regexpr("v[0-9]{1}",x)+1))
  nc_close(nc)
  i <- !duplicated(paste(gcm, rip, rcm, sep="_"))
  var <- switch(var, "tas"="t2m", "tsd"="t2m", "fw"="pr", "mu"="pr", "precip"="pr", var)
  rcmnames[[var]][[scenario[[1]]]] <- list("rcm"=rcm[i], "gcm"=paste(gcm[i], rip[i], sep="."))
}

## Preparations - get a list of available results
files.all <- list.files(path=path.esd, pattern=pattern.esd, full.names = TRUE)
files <- files.all

## Remove trends that do not match available data
file.trends <- list.files(path=path.esd, pattern='trends.rda', full.names = TRUE)
load(file.trends)
T4 <- trends
for(var in names(trends)) {
  for(scenario in names(trends[[var]])) {
    for(it in names(trends[[var]][[scenario]])) {
      if(! any(grepl(var, files) &
               grepl(scenario, files) & grepl(it, files)) ) {
        T4[[var]][[scenario]][[it]] <- NULL
      }
    }
  }
}
rm("trends")

nms <- gsub(paste0(file.path(path.esd, pattern.esd),'.'),'',files)
nms <- gsub('.rda','',nms)
names(files) <- nms

## namesplit splits the list names into different categories
## e.g. t2m.ecad.107rcp45.mam or
##      Finland.fw.ecad.rcp26.djf
namesplit <- function(x) {
  info <- unlist(strsplit(gsub("dse.kss.|.rda", "", x), "\\.|\\_"))
  varid <- info[info %in% names(T4)]
  period <- info[grepl("[0-9]{4}-[0-9]{4}", info)]
  sce <- info[grepl("ssp|rcp", info)]
  nem <- info[grepl("ngcm[1-9]{1,3}", info)]
  seas <- info[grepl("djf|mam|jja|son|annual", info)]
  reanalysis <- info[grepl("era|ncep|merra|carra|cera|ora|cams|macc|asr|cfsr|jra|noaa", 
                            tolower(info))]
  info2 <- info[!info %in% c(varid, period, sce, nem, seas, reanalysis)]
  src <- info2[[length(info2)]]
  return(list(varid=varid,period=period,sce=sce,nem=nem,
              season=seas,src=src,reanalysis=reanalysis))
}

verbose <- FALSE
if(verbose) print('List names:')
if(verbose) print(nms)
n <- length(nms)
vars <- srcs <- seas <- sces <- nems <- rep('?',n)
for (i in 1:n) {
  vars[i] <- namesplit(nms[i])$varid
  srcs[i] <- namesplit(nms[i])$src
  seas[i] <- namesplit(nms[i])$season
  sces[i] <- namesplit(nms[i])$sce
  nems[i] <- length(attr(T4[[vars[i]]][[sces[i]]][[seas[i]]], "model_id"))
}

cats <- data.frame(var=vars,src=srcs,nem=nems,sce=sces,it=seas)
locs <- list()
for(var in unique(vars)) {
  locs[[var]][["location"]] <- loc(T4[[var]][[1]][[1]])
  locs[[var]][["station_id"]] <- stid(T4[[var]][[1]][[1]])
  locs[[var]][["label"]] <- paste0(first2upper(loc(T4[[var]][[1]][[1]])),
                                             " (",stid(T4[[var]][[1]][[1]]),")")
  locs[[var]][["longitude"]] <- lon(T4[[var]][[1]][[1]])
  locs[[var]][["latitude"]] <- lat(T4[[var]][[1]][[1]])
  locs[[var]][["altitude"]] <- alt(T4[[var]][[1]][[1]])
}

gcmnames <- list()
for(scenario in unique(sces)) {
  for(var in unique(vars)) {
    gcmnames[[var]][[scenario]] <- attr(T4[[var]][[scenario]][[1]], "model_id")
  }
}

scenarios <- unique(sces)
scenarios <- as.vector(sapply(scenarios, scenarioname))


