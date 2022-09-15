## calculate.trends.R
library(esd)

## Set path to repository
#path.dse4KSS <- "~/git/dse4KSS"
path.dse4KSS <- getwd()

## This is the path to the downscaled ensemble data 
path.data <- file.path(path.dse4KSS, "data")

## Support functions
path.r <- file.path(path.dse4KSS, "R")
source(file.path(path.r, "rtools.R"))

## New or updated files can be put elsewhere (not in path.data)
## and checked for errors before moving them into path.data
#path.out <- file.path(path.dse4KSS, "test") 
path.out <- path.data

file.trends <- file.path(path.out, "trends.rda")
if(file.exists(file.trends)) {
  load(file.trends)
} else {
  trends <- list()
}
files.dse <- list.files(path=path.data, pattern="dse.kss")
regions <- unique(gsub("[[:punct:]].*.","",gsub("dse.kss.","",files.dse)))
variables <- unique(gsub("[[:punct:]].*.","",
                  gsub(paste0("dse.kss.",regions,".",collapse="|"), 
                       "", files.dse)))
scenarios <- unique(gsub("[[:punct:]].*.","",
                    substr(files.dse, regexpr("rcp|ssp", files.dse), 
                           nchar(files.dse)) ))
seasons <- unique(substr(files.dse, regexpr("djf|mam|jja|son|ann", files.dse), 
                         regexpr("djf|mam|jja|son|ann", files.dse) + 2))
Z <- NULL
for(region in regions) {
  print(region)
  for(param in variables) {#[grepl("t2m|pr|fw",variables)]) {
    print(param)
    for(rcp in scenarios) {
      print(rcp)
      for(season in seasons) {
        print(season)
        i.select <- grepl(paste0(".*",region,".*",param,".*",rcp,".*",season,".*"),
                          files.dse)
        if(any(i.select)) {
          print('Calculating ensemble statistics of trends')
          if(is.null(trends[[region]][[param]][[rcp]][[season]])) {
            Z <- try(zload(path.data, pattern=c("dse.kss",region,param,rcp,season)))
            if(inherits(Z, "try-error")) {
              print(paste("Could not load dse.kss",
                        region, param, rcp, season, sep="."))
            } else if(!is.null(Z)) {
              trends[[region]][[param]][[rcp]][[season]] <- trend.dsensemble(Z)
              save(file=file.trends, trends)
              Z <- NULL
            }
          }
        }
      }
    }
  }
}

show.plots <- FALSE
if(show.plots) {
  for(region in names(trends)) {
    for(variable in c("fw")) {
      cb <- switch(variable, 
                   "pr"=list(pal="burd", rev=TRUE, breaks=seq(-0.12,0.12,0.01)),
                   "fw"=list(pal="burd", rev=TRUE, breaks=seq(-0.12,0.12,0.01)),
                   "t2m"=list(pal="burd", rev=FALSE, breaks=seq(-0.6,0.6,0.02)),
                   NULL)
      for(scenario in c("rcp85", "ssp585")) {
        for(season in c("djf")) {
          trendmap.dsensemble(trends[[region]][[variable]][[scenario]][[season]], 
                              significance="f", threshold=0.95, 
                              colbar=cb, new=TRUE)
          mtext(text=paste(region, scenario, season), side=3, line=0, adj=1)
        }
      }
    }
  }
}
