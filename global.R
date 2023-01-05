## K Parding, Met Norway, 2022-09-27
## R-shiny app that presents empirical-statistical downscaled results.

library(shiny)
library(shinydashboard)
library(esd)
library(ggplot2)

source("rtools.R")
source("calculate.trends.R")

datelist <- list("1951-2100" = c(1951,2100),
                 "1951-1980" = c(1951,1980),
                 "1981-2010" = c(1981,2010),
                 "2031-2060" = c(2031,2060),
                 "2071-2100" = c(2071,2100))

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
  if(length(info)<5) {
    region <- NULL
  } else {
    region <- info[[length(info)-4]]
  }
  return(list(region=region,varid=varid,src=src,nem=nem,sce=sce,it=it))
}

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

