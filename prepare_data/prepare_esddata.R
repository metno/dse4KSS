# prepare_esddata.R

path_in <- "/path/to/esddata"
path_out <- "/path/to/local/data"
path_shape <- "/path/to/shapefiles"

shape_norway <- list.files(path=path_shape, pattern="^Norway.*.OnSeNorgeGrid.*.nc", full.names=TRUE)
shape_regions <- list.files(path=path_shape, pattern="^FylkerOnSeNorgeGrid.*.nc", full.names=TRUE)

pattern <- "metnoESD_nordic.*.nc4"
scenario <- "rcp45"
if(grepl("rcp", tolower(scenario))) cmip <- "CMIP5" else cmip <- "CMIP6"

path_data <- file.path(path_in, cmip, scenario)
files_all <- list.files(path_data, pattern=pattern, full.names = TRUE)
files_in <- files_all[!grepl("fldmean|trend",files_all)]
variables <- c("precip", "t2m")

force <- FALSE
periods <- list(c(1951, 2100), c(1951, 1980), 
                c(1985, 2014), c(2041, 2070), c(2071, 2100))
is <- list(lon=c(2, 31), lat=c(54, 75))

for(file in files_in) {
  print(basename(file))
  path_file <- dirname(file)
  param <- variables[sapply(variables, function(x) grepl(x, file))]
  
  ## Regrid shape files
  shape_norway_regridded <- file.path("~/Desktop", gsub("OnSeNorgeGrid",paste0("ESD",param,"Grid"),basename(shape_norway)))
  shape_regions_regridded <- file.path("~/Desktop", gsub("OnSeNorgeGrid",paste0("RCM",param,"Grid"),basename(shape_regions)))
  if(!file.exists(shape_norway_regridded)) system(paste0("cdo remapbil,", file, " ", shape_norway, " ", shape_norway_regridded))
  if(!file.exists(shape_regions_regridded)) system(paste0("cdo remapbil,", file, " ", shape_regions, " ", shape_regions_regridded))
  file_fldmean <- file.path(path_out, cmip, scenario, paste0("fldmean_", basename(file)))
  if(force) file.remove(file_fldmean)
  
  nc <- ncdf4::nc_open(shape_regions_regridded)
  regions <- names(nc$var)
  regions <- regions[!grepl("lon|lat|rotated", regions)]
  ncdf4::nc_close(nc)
  
  for(it in periods) {
    file_timmean <- file.path(path_out, cmip, scenario,
                              paste0("timmean_", gsub("[0-9]{4}-[0-9]{4}.nc|.nc", 
                                                      paste0(paste(it, collapse="-"),".nc"), 
                                                      basename(file))))
    file_trend <- file.path(path_out, cmip, scenario,
                            paste0("trend_", gsub("[0-9]{4}-[0-9]{4}.nc|.nc", 
                                                   paste0(paste(it, collapse="-"),".nc"), 
                                                   basename(file))))

    if(force) { 
      file.remove(file_timmean)
      file.remove(file_trend)
    }
    if(!file.exists(file_timmean) | !file.exists(file_trend)) {
      nc <- ncdf4::nc_open(file)
      info <- esd::check.ncdf4(nc)
      ncdf4::nc_close(nc)
      years <- esd::year(info$time$vdate)
      step1 <- which.min(abs(min(it)-years))
      step2 <- which.min(abs(max(it)-years))
      
      if(!file.exists(file_trend)) {
        print(paste("Calculate trends for", paste(it, collapse="-")))
        cmd <- paste0("cdo trend -seltimestep,", step1, ",", step2, 
                      " -select,name=",param, " ", 
                      file, " offset.nc ", file_trend)
        system(cmd)
      }
      
      if(!file.exists(file_timmean)) {
        print(paste("Calculate time mean for", paste(it, collapse="-")))
        cmd <- paste0("cdo timmean -seltimestep,", step1, ",", step2, 
                      " -select,name=", param, " ", file, " ", file_timmean)
        system(cmd)
      }
    }
  }

  if(!file.exists(file_fldmean)) {
    print(paste("Calculate field mean for Norway for", basename(file)))
    file_norway <- gsub(".nc", "_Norway.nc", file_fldmean)
    system(paste("cdo ifthen", shape_norway_regridded, file, "tmp.nc"))
    system(paste("cdo -fldmean tmp.nc tmp_fldmean.nc"))
    system(paste0("cdo chname,", param, ",", paste0(param,"_Norway"), 
                  " tmp_fldmean.nc ", file_norway))
    for(region in regions) {
      print(paste("Calculate field mean for Norway", region, "for", basename(file)))
      system(paste0("cdo select,name=", region, " ", shape_regions_regridded, " tmp_shape.nc"))
      system(paste("cdo ifthen tmp_shape.nc", file, " tmp_masked.nc"))
      file_region <- gsub(".nc", paste0("_",region,".nc"), file_fldmean)
      system(paste("cdo -fldmean tmp_masked.nc tmp_fldmean.nc"))
      system(paste0("cdo chname,", param, ",", paste0(param,"_",region), 
                    " tmp_fldmean.nc ", file_region))
    }
    system(paste("cdo merge", gsub(".nc", "_*.nc", file_fldmean), file_fldmean))
    system(paste("rm", gsub(".nc", "_*.nc", file_fldmean)))
    system("rm tmp*.nc")
  }
}


