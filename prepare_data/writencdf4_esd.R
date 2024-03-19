## Test making plot with RCM and ESD

library(esd)

## Set variable, scenario etc.
path.data <- "/path/to/esddata"
pattern.esd <- "dse.kss.Nordic"
path.rcm <- "/path/to/rcmdata"
files.esd <- list.files(path=path.data, pattern=".rda")
verbose <- FALSE

region <- "nordic"
variables <- c("t2m","pr")#,"fw")
scenarios <- c("rcp26","rcp85","rcp45",
               "ssp585","ssp370","ssp245","ssp126")
seasons <- c("son","djf","mam","jja")

writeindividual <- TRUE
writeensemble <- TRUE
writestats <- FALSE
force <- FALSE

## Write ESD data to netcdf files
path.esd.out <- "/output/path/for/esddata"

## Functions
esdlabel <- function(param="tas", season="djf", 
                     region="nordic", scenario="rcp85",
                     language="en") {
  if(grepl("en",language)) {
    lab <- "Empirical-statistically downscaled"
    param <- tolower(gsub("_"," ",param))
    if(param %in% c("tas","t2m","temp","temperature")) {
      param <- "temperature"
    } else if (param %in% c("pr","precip")) {
      param <- "daily precipitation amount"
    } else if (grepl("fw|wet-freq", param)) {
      param <- "wet-day frequency"
    } else if (grepl("mu|wet-day mean", param)) {
      param <- "wet-day mean precipitation"
    } else if (param=="tsd") {
      param <- "standard deviation of the temperature"
    }
    season <- switch(season,
                     "djf"="for the winter season (DJF)", 
                     "mam"="for the spring season (MAM)",
                     "jja"="for the summer season (JJA)",
                     "son"="for the fall season (SON)",
                     "annual"="for the whole year (annual mean)",
                     season)
    scenario <- paste("emission scenario",toupper(scenario))
  } else if(grepl("no",language)) {
    lab <- "Empirisk-statistisk nedskalert"
    param <- tolower(gsub("_"," ",param))
    if(grepl("tas|t2m|temp",param) & !grepl("tsd|stdev|standard",param)) {
      param <- "gjennomsnittlig temperatur"
    } else if (grepl("pr",param)) {
      param <- "gjennomsnittlig daglig nedbørsmengde"
    } else if (grepl("fw|wet-freq|wet-day frequency", param)) {
      param <- "gjennomsnittlig nedbørsfrekvens"
    } else if (grepl("mu|wet-day mean", param)) {
      param <- "gjennomsnittlig nedbørsintensitet"
    } else if (param=="tsd") {
      param <- "standardavvik for temperatur"
    }
    season <- switch(season,
                     "djf"="for vintersesongen (desember - februar)", 
                     "mam"="for vårsesongen (mars - mai)",
                     "jja"="for sommersesongen (juni - august)",
                     "son"="for høstsesongen (september - november)",
                     "annual"="for hele året (årsgjennomsnitt)",
                     season)
    scenario <- paste("utslippsscenario",toupper(scenario))
  }
  lab <- paste0(lab, " ", param, " ", season, ", ", scenario)
  return(lab)
}

## Attributes for netcdf files
distribution_statement <- "Free"
institution <- "Norwegian Meteorological Institute"
institution_short_name <- "MetNo"
license <- "https://data.norge.no/nlod/no/2.0"
processing_level <- "Scientific"
source <- "Empirical statistical downscaling"
publisher_name <- "Kajsa Parding"
publisher_email <- "kajsamp@met.no"
publisher_url <- NA
publisher_type <- "person"
publisher_institution <- "Norwegian Meteorological Institute"
creator_name <- "Kajsa Parding"
creator_email <- "kajsamp@met.no"
creator_url <- NA
creator_institution <- "Norwegian Meteorological Institute"
creator_type <- "person"
references <- "Benestad, R.E., Chen, D., Mezghani, A., Fan, L. and Parding, K., 2015. On using principal components to represent stations in empirical–statistical downscaling. Tellus A: Dynamic Meteorology and Oceanography, 67(1), p.28326.DOI: https://doi.org/10.3402/tellusa.v67.28326"
project="Klima i Norge 2100 (2024)"
project_short_name <- "KIN-2024" 
dataset_production_status <- "in-progress"
iso_topic_category <- "climatologyMeteorologyAtmosphere"
keywords <- NA # generated for each file
product_version <- "v1"
acknowledgment <- NA
comment <- NA
conventions <- NA
id <- NA # generated for each file
naming_authority <- "Norwegian Meteorological Institute"
standard_name_vocabulary <- NA
additional_attributes <- NA

for(param in variables) {
  keywords <- "EARTH SCIENCE > ATMOSPHERE"
  if(grepl("pr|fw|mu",param)) {
    keywords <- paste(keywords, "PRECIPITATION", sep=" > ")
    if(param %in% c("pr","precip")) keywords <- paste(keywords, "PRECIPITATION AMOUNT", sep=" > ")
  } else if (grepl("t2m|tas|temp|tsd",param)) {
    keywords <- paste(keywords, "ATMOSPHERIC TEMPERATURE > SURFACE TEMPERATURE", sep=" > ")
  }
  for(scenario in scenarios) {
    if(grepl("rcp",scenario)) cmip <- "CMIP5" else cmip <- "CMIP6"
    if(!dir.exists(file.path(path.esd.out, cmip))) dir.create(file.path(path.esd.out, cmip))
    if(!dir.exists(file.path(path.esd.out, cmip, scenario))) dir.create(file.path(path.esd.out, cmip, scenario))
    
    ## RCM ensemble member names
    if(writestats) {
      files.rcm <- list.files(path=file.path(path.rcm,"calculations"), pattern=".nc*", 
                              full.names = TRUE)
      i <- eval(parse(text=paste(paste0("grepl('",tolower(c("remapbil", param.rcm, scenario)),
                                        "', tolower(files.rcm))"),collapse=" & ")))
      files.rcm <- files.rcm[i]
    }
    
    for(season in seasons) {
      print(paste(param, cmip, scenario, season))
      ## Generate titles for data set in English and Norwegian
      title <- esdlabel(param=param, season=season, scenario=scenario,
                        region=region, language="en")
      title_no <- esdlabel(param=param, season=season, scenario=scenario,
                           region=region, language="no")
      
      ## Find and load rda-file with the selected dsensemble
      i <- grepl(paste0("_",tolower(param),"_"), tolower(gsub("[.]","_",files.esd))) &
        grepl(tolower(season), tolower(files.esd)) &
        grepl(tolower(scenario), tolower(files.esd)) &
        grepl(tolower(region), tolower(files.esd))
      
      if(sum(i)==0) {
        print(paste("No files for ", param, season, scenario, region))
      } else {
        file.i <- files.esd[i]
        print(file.i)
        load(file.path(path.data, file.i))
        
        zmean <- sapply(Z[grepl("i[0-9]",names(Z))], function(x) mean(x[,1]))
        iqr <- quantile(zmean, probs = 0.75) - quantile(zmean, probs = 0.25)
        suspect <- abs(zmean) > (median(zmean) + 10*iqr)
        if(any(suspect)) {
          print(paste("Excluding suspicious results:", paste(names(Z)[suspect]), sep=" "))
          Z <- subset(Z, im=which(!suspect))
        }
        
        if(writeensemble) {
          print("Write downscaled results to file")
          type <- "dsensemble"
          id <- uuid::UUIDgenerate()
          write2ncdf4(Z, path=file.path(path.esd.out, cmip, scenario), 
                      type=type, conventions=conventions, region=region,
                      id=id,naming_authority=naming_authority,
                      title=title, title_no=title_no,
                      source=source,processing_level=processing_level,
                      comment=comment,acknowledgment=acknowledgment,license=license,
                      standard_name_vocabulary=standard_name_vocabulary,
                      creator_name=creator_name,creator_email=creator_email,creator_url=creator_url,
                      creator_institution=creator_institution,creator_type=creator_type,
                      institution=institution,institution_short_name=institution_short_name,
                      project=project,project_short_name=project_short_name,
                      publisher_name=publisher_name,publisher_email=publisher_email,publisher_url=publisher_url,
                      publisher_type=publisher_type,publisher_institution=publisher_institution,
                      product_version=product_version,references=references,
                      additional_attributes=additional_attributes,
                      force=force,verbose=verbose)
        }
        
        if(writeindividual) {
          print("Write data for each model to files")
          type <- c("model", "field")
          for(im in seq(1, length(attr(Z, "model_id")))) {
            id <- uuid::UUIDgenerate()
            print(paste("Write data for model",im,"of",length(attr(Z,"model_id")),
                        " (",attr(Z, "model_id")[im],")"))
            write2ncdf4(Z, path=file.path(path.esd.out, cmip, scenario), 
                        type=type, im=im, conventions=conventions, region=region,
                        id=id,naming_authority=naming_authority,
                        title=title, title_no=title_no,
                        source=source,processing_level=processing_level,
                        comment=comment,acknowledgment=acknowledgment,license=license,
                        standard_name_vocabulary=standard_name_vocabulary,
                        creator_name=creator_name,creator_email=creator_email,creator_url=creator_url,
                        creator_institution=creator_institution,creator_type=creator_type,
                        institution=institution,institution_short_name=institution_short_name,
                        project=project,project_short_name=project_short_name,
                        publisher_name=publisher_name,publisher_email=publisher_email,publisher_url=publisher_url,
                        publisher_type=publisher_type,publisher_institution=publisher_institution,
                        product_version=product_version,
                        references=references,
                        additional_attributes=additional_attributes,
                        force=force,verbose=verbose)
          }
        }
        
        if(writestats) {
          print("Calculate ensemble statistics and write to file")
          it <- c(1950,2100)
          qc <- FALSE
          type <- "ensemblestatistics"
          param.rcm <- switch(param, "t2m"="tas", param)
          
          if(length(files.rcm)>0) {
            models_rcm <- gsub(".*.EUR-11_|_sem.nc","",files.rcm)
            i.rip <- regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}|r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0-9]{1,2}",models_rcm)
            i.rcp <- regexpr("rcp[0-9]{2}|ssp[0-9]{3}",models_rcm)
            gcm.rcm <- substr(models_rcm, 1, i.rcp-2)
            rip.rcm <- substr(models_rcm, i.rip, i.rip+attr(i.rip,"match.length")-1)
            rcm <- substr(models_rcm, i.rip+attr(i.rip,"match.length")+1, nchar(models_rcm))
            rcmmodels <- paste(gcm.rcm, rip.rcm, sep="_")
          }
          
          for(season in seasons) {
            print(paste(param, scenario, season))
            
            ## Generate titles for data set in English and Norwegian
            title <- esdlabel(param=param, season=season, scenario=scenario,
                              region=region, language="en")
            title_no <- esdlabel(param=param, season=season, scenario=scenario,
                                 region=region, language="no")
            
            ## Find and load rda-file with the selected dsensemble
            i <- grepl(paste0("_",tolower(param),"_"), tolower(gsub("[.]","_",files.esd))) &
              grepl(tolower(season), tolower(files.esd)) &
              grepl(tolower(scenario), tolower(files.esd)) &
              grepl(tolower(region), tolower(files.esd))
            
            file.i <- files.esd[i]
            print(file.i)
            load(file.path(path.data, file.i))
            
            ## ESD ensemble member names
            i.rip <- regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}|r[0-9]{1,2}i[0-9]{1,2}p[0-9]{1,2}f[0,9]{1,2}",
                             attr(Z,"model_id"))
            gcm.esd <- gsub("\\.","-",substr(attr(Z,"model_id"),1,i.rip-2))
            rip.esd <- substr(attr(Z,"model_id"),i.rip,i.rip+attr(i.rip,"match.length"))
            esdmodels <- paste(gcm.esd,rip.esd,sep="_")
            
            print("Write ensemble statistics to file")
            ensemblename <- NULL
            im <- NULL
            id <- uuid::UUIDgenerate()
            write2ncdf4(Z, im=im, it=it, path=file.path(path.esd.out, cmip, scenario), 
                        type=type, conventions=conventions, region=region,
                        ensemblename=ensemblename,
                        id=id,naming_authority=naming_authority,
                        title=title, title_no=title_no,qc=qc,
                        source=source,processing_level=processing_level,
                        comment=comment,acknowledgment=acknowledgment,license=license,
                        standard_name_vocabulary=standard_name_vocabulary,
                        creator_name=creator_name,creator_email=creator_email,creator_url=creator_url,
                        creator_institution=creator_institution,creator_type=creator_type,
                        institution=institution,institution_short_name=institution_short_name,
                        project=project,project_short_name=project_short_name,
                        publisher_name=publisher_name,publisher_email=publisher_email,publisher_url=publisher_url,
                        publisher_type=publisher_type,publisher_institution=publisher_institution,
                        product_version=product_version,references=references,
                        additional_attributes=additional_attributes,
                        force=force,verbose=verbose)
            
            print("Subset only one ESD simulation from each GCM (first available rip)")
            ensemblename <- "onerun"
            im <- which(!duplicated(gcm.esd))
            names(im) <- esdmodels[im]
            id <- uuid::UUIDgenerate()
            write2ncdf4(Z, im=im, it=it, path=file.path(path.esd.out, cmip, scenario), 
                        type=type, conventions=conventions, region=region,
                        ensemblename=ensemblename,
                        id=id,naming_authority=naming_authority,
                        title=title, title_no=title_no,qc=qc,
                        source=source,processing_level=processing_level,
                        comment=comment,acknowledgment=acknowledgment,license=license,
                        standard_name_vocabulary=standard_name_vocabulary,
                        creator_name=creator_name,creator_email=creator_email,creator_url=creator_url,
                        creator_institution=creator_institution,creator_type=creator_type,
                        institution=institution,institution_short_name=institution_short_name,
                        project=project,project_short_name=project_short_name,
                        publisher_name=publisher_name,publisher_email=publisher_email,publisher_url=publisher_url,
                        publisher_type=publisher_type,publisher_institution=publisher_institution,
                        product_version=product_version,references=references,
                        additional_attributes=additional_attributes,
                        force=force,verbose=verbose)
            
            print("Subset the CORDEX ensemble from the ESD ensemble")
            if(length(files.rcm)>0) {
              ensemblename <- "cordexensemble"
              im <- unlist(sapply(seq_along(esdmodels), 
                                  function(i) rep(i, sum(grepl(esdmodels[i], rcmmodels)))))
              ok <- sapply(rcmmodels, function(x) any(sapply(esdmodels, function(y) grepl(y, x))))
              if(any(!ok)) for(nok in rcmmodels[!ok]) {
                if(any(sapply(gcm.esd, function(x) grepl(x, nok)))) {
                  im <- c(im, which(sapply(gcm.esd, function(x) grepl(x, nok)))[1])
                }
              }
              im <- sort(im)
              names(im) <- esdmodels[im]
              id <- uuid::UUIDgenerate()
              write2ncdf4(Z, im=im, it=it, path=file.path(path.esd.out, cmip, scenario), 
                          type=type, conventions=conventions, region=region,
                          ensemblename=ensemblename, qc=qc,
                          id=id,naming_authority=naming_authority,
                          title=title, title_no=title_no,
                          source=source,processing_level=processing_level,
                          comment=comment,acknowledgment=acknowledgment,license=license,
                          standard_name_vocabulary=standard_name_vocabulary,
                          creator_name=creator_name,creator_email=creator_email,creator_url=creator_url,
                          creator_institution=creator_institution,creator_type=creator_type,
                          institution=institution,institution_short_name=institution_short_name,
                          project=project,project_short_name=project_short_name,
                          publisher_name=publisher_name,publisher_email=publisher_email,publisher_url=publisher_url,
                          publisher_type=publisher_type,publisher_institution=publisher_institution,
                          product_version=product_version,references=references,
                          additional_attributes=additional_attributes,
                          force=force,verbose=verbose)
            }
          }
        }
      }
    }
  }
}

