## K Parding, Met Norway, 2022-09-27
## R-shiny app that presents empirical-statistical downscaled results.

library(shiny)
library(shinydashboard)
library(esd)
library(ggplot2)
library(leaflet)
library(plotly)

source("rtools.R")
source("calculate.trends.R")

## Initial choice of region, variable etc
#reg0 <- "Nordic"
var0 <- "pr"
sce0 <- "rcp85"
seas0 <- "djf"
path.data <- "data"
path.esd <- file.path(path.data, "esd")
path.rcm <- file.path(path.data, "rcm")
pattern.esd <- "dse.kss.Nordic"
pattern.rcm <- c("ens","EUR-11","remapbil")

datelist <- list("1951-2100" = c(1951,2100),
                 "1951-1980" = c(1951,1980),
                 "1981-2010" = c(1981,2010),
                 "2031-2060" = c(2031,2060),
                 "2071-2100" = c(2071,2100))

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
  rcmnames[[var]][[scenario[[1]]]] <- list("rcm"=rcm[i], "gcm"=paste(gcm[i], rip[i], sep="."))
}

## Preparations - get a list of available results
files.all <- list.files(path=path.esd, pattern=pattern.esd, full.names = TRUE)
files <- files.all


file.trends <- list.files(path=path.esd, pattern='trends.rda', full.names = TRUE)
load(file.trends)
T4 <- trends
#for(region in names(trends)) {
  for(var in names(trends)) {#[[region]])) {
    for(scenario in names(trends[[var]])) {#[[region]][[var]])) {
      for(it in names(trends[[var]][[scenario]])) {#[[region]][[var]][[scenario]]) {
        if(! any(grepl(var, files) &
                 #grepl(region, files) & 
                 grepl(scenario, files) & grepl(it, files)) ) {
          #T4[[region]][[var]][[scenario]][[it]] <- NULL
          T4[[var]][[scenario]][[it]] <- NULL
        }
      }
    }
  }
#}
rm("trends")

nms <- gsub(file.path(path.esd, pattern.esd),'',files)
nms <- gsub('_[0-9]{4}-[0-9]{4}.rda','',nms)
names(files) <- nms

## namesplit splits the list names into different categories
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
  return(list(varid=varid,src=src,nem=nem,sce=sce,it=it))
  #if(length(info)<5) {
  #  region <- NULL
  #} else {
  #  region <- info[[length(info)-4]]
  #}
  #return(list(region=region,varid=varid,src=src,nem=nem,sce=sce,it=it))
}

verbose <- FALSE
if(verbose) print('List names:')
if(verbose) print(nms)
n <- length(nms)
#regs <- rep('?',n); 
vars <- rep('?',n); srcs <- vars;
nems <- vars; sces <- vars; its <- vars
for (i in 1:n) {
  #regs[i] <- namesplit(nms[i])$region
  vars[i] <- namesplit(nms[i])$varid
  srcs[i] <- namesplit(nms[i])$src
  sces[i] <- namesplit(nms[i])$sce
  its[i] <- namesplit(nms[i])$it
  nems[i] <- length(attr(T4[[vars[i]]][[sces[i]]][[its[i]]], "model_id"))
  #nems[i] <- length(attr(T4[[regs[i]]][[vars[i]]][[sces[i]]][[its[i]]], "model_id"))
}
#print('Categories for UI selection:')
cats <- data.frame(var=vars,src=srcs,nem=nems,sce=sces,it=its)
#cats <- data.frame(region=regs,var=vars,src=srcs,nem=nems,sce=sces,it=its)

locs <- list()
#for(region in unique(regs)) {
  for(var in unique(vars)) {
    locs[[var]][["location"]] <- loc(T4[[var]][[1]][[1]])
    locs[[var]][["station_id"]] <- stid(T4[[var]][[1]][[1]])
    locs[[var]][["label"]] <- paste0(first2upper(loc(T4[[var]][[1]][[1]])),
                                               " (",stid(T4[[var]][[1]][[1]]),")")
    locs[[var]][["longitude"]] <- lon(T4[[var]][[1]][[1]])
    locs[[var]][["latitude"]] <- lat(T4[[var]][[1]][[1]])
    locs[[var]][["altitude"]] <- alt(T4[[var]][[1]][[1]])
    #locs[[region]][[var]][["location"]] <- loc(T4[[region]][[var]][[1]][[1]])
    #locs[[region]][[var]][["station_id"]] <- stid(T4[[region]][[var]][[1]][[1]])
    #locs[[region]][[var]][["label"]] <- paste0(first2upper(loc(T4[[region]][[var]][[1]][[1]])),
    #                                " (",stid(T4[[region]][[var]][[1]][[1]]),")")
    #locs[[region]][[var]][["longitude"]] <- lon(T4[[region]][[var]][[1]][[1]])
    #locs[[region]][[var]][["latitude"]] <- lat(T4[[region]][[var]][[1]][[1]])
    #locs[[region]][[var]][["altitude"]] <- alt(T4[[region]][[var]][[1]][[1]])
  }
#}

gcmnames <- list()
for(scenario in unique(sces)) {
  for(var in unique(vars)) {
    gcmnames[[var]][[scenario]] <- attr(T4[[var]][[scenario]][[1]], "model_id")#[[1]][[var]][[scenario]][[1]], "model_id")
  }
}

scenarios <- unique(sces)
scenarios <- as.vector(sapply(scenarios, scenarioname))
#scenarios <- paste(toupper(scenarios),
#                   c("(CMIP5)","(CMIP6)")[as.numeric(grepl("SSP", scenarios))+1])
#names(scenarios) <- unique(sces)




