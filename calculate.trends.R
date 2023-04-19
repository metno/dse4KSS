## calculate.trends.R

## Support functions
source("rtools.R")

## This is the path to the downscaled ensemble data - set in global.R
#path.esd <- "data/esd"
#pattern.esd <- "dse.kss.Nordic"

## file name format: pattern.variable...scenario...season...rda
## where pattern is defined above, scenario is e.g. rcp45 or ssp585,
## and season is annual, djf, mam, jja or son

## New or updated files can be put elsewhere (not in path.data)
## and checked for errors before moving them into path.data
path.out <- path.esd

file.trends <- file.path(path.out, "trends.rda")
if(file.exists(file.trends)) {
  load(file.trends)
} else {
  trends <- list()
}

files.dse <- list.files(path=path.esd, pattern=pattern.esd)
files.dse <- files.dse[grepl("djf|mam|jja|son|ann",files.dse) & 
                         grepl("rcp|ssp",files.dse)]
variables <- unique(gsub("[[:punct:]].*.","",
                  gsub(paste0(pattern.esd,".",collapse="|"), 
                       "", files.dse)))
scenarios <- unique(gsub("[[:punct:]].*.","",
                    substr(files.dse, regexpr("rcp|ssp", files.dse), 
                           nchar(files.dse)) ))
seasons <- unique(substr(files.dse, regexpr("djf|mam|jja|son|ann", files.dse), 
                         regexpr("djf|mam|jja|son|ann", files.dse) + 2))
Z <- NULL
for(param in variables) {#[grepl("t2m|pr|fw",variables)]) {
  print(param)
  for(rcp in scenarios) {
    print(rcp)
    for(season in seasons) {
      print(season)
      i.select <- grepl(paste0(".*",param,".*",rcp,".*",season,".*"),
                        files.dse)
      if(any(i.select)) {
        print('Calculating ensemble statistics of trends')
        if(is.null(trends[[param]][[rcp]][[season]])) {
          Z <- try(zload(path=path.esd, src="ESD", param=param, 
                         season=season, scenario=rcp,
                         FUNX="mean", pattern.esd=pattern.esd))
          if(inherits(Z, c("try-error","NULL"))) {
            print(paste("Could not load ESD data for",
                        param, rcp, season, sep=" "))
          } else if(inherits(Z, "dsensemble")) {
            trends[[param]][[rcp]][[season]] <- trend.dsensemble(Z)
            save(file=file.trends, trends)
            Z <- NULL
          }
        }
      }
    }
  }
}

# show.plots <- FALSE
# if(show.plots) {
#   for(region in names(trends)) {
#     for(variable in c("fw")) {
#       cb <- switch(variable, 
#                    "pr"=list(pal="burd", rev=TRUE, breaks=seq(-0.12,0.12,0.01)),
#                    "fw"=list(pal="burd", rev=TRUE, breaks=seq(-0.12,0.12,0.01)),
#                    "t2m"=list(pal="burd", rev=FALSE, breaks=seq(-0.6,0.6,0.02)),
#                    NULL)
#       for(scenario in c("rcp85", "ssp585")) {
#         for(season in c("djf")) {
#           trendmap.dsensemble(trends[[region]][[variable]][[scenario]][[season]], 
#                               significance="f", threshold=0.95, 
#                               colbar=cb, new=TRUE)
#           mtext(text=paste(region, scenario, season), side=3, line=0, adj=1)
#         }
#       }
#     }
#   }
# }
