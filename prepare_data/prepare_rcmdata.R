# prepar_rcmdata.R
library(esd)

path_lustre <- "/path/to/data"
path_local <- "/path/to/local/data"
path_shape <- "/path/to/shapefiles"

force <- FALSE
periods <- list(c(1971,2100), c(1971,2000), c(1985,2014),
                c(2041,2070), c(2071,2100))
is <- list(lon=c(2, 31), lat=c(54, 75))

# RCM file that will be used for regridding
file_remapbil <- "/path/to/file/tas_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-ETH-COSMO-crCLIM-v1-1_v1_sem.nc" 

projects <- c("CMIP6", "CMIP5")
for(cmip in projects) {
  models <- list.dirs(file.path(path_lustre, cmip), recursive=FALSE, full.names=FALSE)
  for(model in models) {
    scenarios <- list.dirs(file.path(path_lustre, cmip, model), recursive=FALSE, full.names=FALSE)
    for(scenario in scenarios) {
      variables <- list.dirs(file.path(path_lustre, cmip, model, scenario), recursive=FALSE, full.names=FALSE)
      for(param in variables) {
        print(paste(cmip, model, scenario, param))
        path_in <- file.path(path_lustre, cmip, model, scenario, param)
        path_out <- file.path(path_local, cmip, model, scenario, param)
        if(!dir.exists(file.path(path_local, cmip, model))) dir.create(file.path(path_local, cmip, model))
        if(!dir.exists(file.path(path_local, cmip, model, scenario))) dir.create(file.path(path_local, cmip, model, scenario))
        if(!dir.exists(file.path(path_local, cmip, model, scenario, param))) dir.create(file.path(path_local, cmip, model, scenario, param))
        
        files <- list.files(path=path_out, pattern=".nc")
        if(sum(grepl("^remapbil_.*.sem_[A-Z]", files))<4) {
          files_mon <- list.files(path=file.path(path_in, "mon"), pattern=".nc", full.names = TRUE)
          files_mon <- files_mon[grepl("_mon", files_mon) & grepl(scenario, files_mon) &
                                 !grepl("trend|fldmean|remapbil|mergetime", files_mon)]
          regexpr_years <- regexpr("[0-9]{4,6}-[0-9]{4,6}|[0-9]{4,6}.nc", files_mon)
          years_mon <- substr(files_mon, regexpr_years, regexpr_years + attr(regexpr_years, "match.length")-1)
          y1 <- substr(strsplit(years_mon[[1]], split="-")[[1]][1], 1, 4)
          y2 <- substr(strsplit(years_mon[[length(years_mon)]], split="-")[[1]][2], 1, 4)
          if(grepl("ssp|rcp", scenario)) {
            path_hist <- file.path(path_lustre, cmip, model, "hist", param, "mon")
            files_hist <- list.files(path=path_hist, pattern=".*._mon.*..nc", full.names = TRUE)
            files_hist <- files_hist[!grepl("remapbil", files_hist) & !grepl("fldmean", files_hist)]
            file_last <- files_hist[length(files_hist)]
            nc <- ncdf4::nc_open(file_last)
            dimnames <- sapply(nc$var[[param]]$dim, function(x) x$name)
            ntime <- nc$var[[param]]$dim[[grep("time",dimnames)]]$len
            ncdf4::nc_close(nc)
            file_out <- gsub("_monmean_", "_mon_", file_last)
            file_out <- file.path(path_local, cmip, model, scenario, param, 
                                  gsub("_mon_[0-9]{4,6}-","_mon_",basename(file_out)))
            cmd <- paste0("cdo seltimestep,",ntime,"/",ntime," ",file_last," ",file_out)
            if(force & file.exists(file_out)) file.remove(file_out)
            if(!file.exists(file_out)) system(cmd)
            files_mon <- c(file_out, files_mon)
          }
          
          if(length(files_mon)>1) {
            print("Merge files from different time steps")
            file_out <- gsub("[0-9]{4,6}-[0-9]{4,6}", paste0(y1,"-",y2), files_mon[length(files_mon)])
            file_out <- file.path(path_out, paste0("mergetime_",basename(file_out)))
            cmd <- paste("cdo mergetime", 
                         paste(paste0("-sellonlatbox,",is$lon[1],",",is$lon[2],
                                      ",",is$lat[1],",",is$lat[2]), files_mon, collapse=" "), file_out)
            if(force & file.exists(file_out)) file.remove(file_out)
            if(!file.exists(file_out)) system(cmd)
          } else file_out <- files_in
          
          print("Regrid data with remapbil")
          file_in <- file_out
          file_out <- file.path(path_out, paste0("remapbil_", basename(file_in)))
          file_out <- gsub("mergetime_", "", file_out)
          cmd <- paste0("cdo remapbil,", file_remapbil, " ", file_in, " ", file_out)
          #if(file.exists(file_out) & force) {
          #  file.remove(file_out)
          #} else {
            file_lustre <- file.path(path_out, basename(file_out))
            if(file.exists(file_lustre)) file_out <- file_lustre
          #}
          if(!file.exists(file_out)) system(cmd)
          
          print("Calculate the seasonal mean")
          file_in <- file_out
          file_out <- file.path(path_out, gsub("monmean|mon", "sem", basename(file_out)))
          nc <- ncdf4::nc_open(file_in)
          dimnames <- sapply(nc$var[[param]]$dim, function(x) x$name)
          ntime <- nc$var[[param]]$dim[[grep("time",dimnames)]]$len
          ncdf4::nc_close(nc)
          step2 <- ntime - 1
          if(grepl("hist", scenario)) step1 <- 3 else step1 <- 1
          cmd <- paste0("cdo seasmean -seltimestep,",step1,"/",step2," ",file_in," ",file_out)
          if(force & file.exists(file_out)) file.remove(file_out)
          if(!file.exists(file_out)) system(cmd)
          
          print("Split seasons")
          file_in <- file_out
          file_out <- gsub("_sem_[0-9].*", "_sem_", file_out)
          cmd <- paste("cdo splitseas", file_in, file_out)
          if(!file.exists(file_out) | force) system(cmd)
          files_out <- list.files(path=dirname(file_out), pattern=paste0(basename(file_out), "[A-Z]"),
                                  full.names=TRUE)
          if(force) sapply(files_out, file.remove)
          if(sum(file.exists(files_out))<4) system(cmd)
        }
        
        print("Remove some intermediate files")
        files <- list.files(path=path_out, pattern=".nc", full.names=TRUE)
        if(any(grepl("mergetime|sem_[0-9]{4}-[0-9]{4}.nc|_mon", files))) {
          sapply(files[grepl("mergetime|sem_[0-9]{4}-[0-9]{4}.nc|_mon", files)], 
                 function(x) {if(file.exists(x)) file.remove(x)}) 
        }

        if(grepl("ssp|rcp", scenario)) {
          files <- list.files(path=path_out, pattern="sem_[A-Z]", full.names = TRUE)
          files <- files[!grepl("trend|timmean|fldmean", files)]
          for(file in files) {
            ## For scenarios, merge historical seasonal mean data with
            ## the scenario data
            nc <- ncdf4::nc_open(file)
            ncinfo <- esd::check.ncdf4(nc)
            ncdf4::nc_close(nc)
            season <- substr(gsub(".*._sem_", "", basename(file)), 1, 3)
            if(min(year(ncinfo$time$vdate))>2010) {
              path_hist <- file.path(path_local, cmip, model, "hist", param)
              file_hist <- list.files(path=path_hist, pattern=paste0("^remapbil.*._sem_",season),
                                      full.names=TRUE)
              if(length(file_hist)==0) file_hist <- 
                  file.path(path_lustre, cmip, model, "hist", param, "sem", basename(file_hist))
              if(file.exists("tmp.nc")) file.remove("tmp.nc")
              system(paste("cdo mergetime", file, file_hist, "tmp.nc"))
              system(paste("mv tmp.nc", file))
            }
          }
        }
        
        if(grepl("rcp|ssp", scenario)) {
          
          files_in <- list.files(path=path_out, pattern="_sem_[A-Z]", full.names=TRUE)
          files_in <- files_in[!grepl("trends|timmean|fldmean",  files_in) & grepl("remapbil", files_in)]
          
          ## Shape: Norway
          shape_in <- list.files(path=path_shape, pattern="^Norway.*.OnSeNorgeGrid.*.nc", full.names=TRUE)
          shape_norway <- file.path("~/Desktop", gsub("OnSeNorgeGrid",paste0("RCM",param,"Grid"),basename(shape_in)))
          system(paste0("cdo remapbil,", files_in[1], " ", shape_in, " ", shape_norway))
          
          ## Shape: Fylker
          shape_in <- list.files(path=path_shape, pattern="^FylkerOnSeNorgeGrid.*.nc", full.names=TRUE)
          shape_regions <- file.path("~/Desktop", gsub("OnSeNorgeGrid",paste0("RCM",param,"Grid"),basename(shape_in)))
          system(paste0("cdo remapbil,", files_in[1], " ", shape_in, " ", shape_regions))
          
          nc <- ncdf4::nc_open(shape_regions)
          regions <- names(nc$var)
          regions <- regions[!grepl("lon|lat|rotated", regions)]
          ncdf4::nc_close(nc)
          
          for(file in files_in) {
            nc <- ncdf4::nc_open(file)
            info <- esd::check.ncdf4(nc)
            ncdf4::nc_close(nc)
            years <- year(info$time$vdate)
          
            for(it in periods) {
              print(paste("Calculate trends for", paste(it, collapse="-")))
              step1 <- which.min(abs(min(it)-years))
              step2 <- which.min(abs(max(it)-years))
              file_out <- paste0(dirname(file), "/", 
                                 paste0("trends_", gsub(".nc", 
                                                        paste0("_", years[step1], "-", years[step2],".nc"), 
                                                        basename(file)) ))
              cmd <- paste0("cdo trend -seltimestep,", step1, ",", step2, 
                            " -select,name=",param, " ", 
                            file, " offset.nc ", file_out)
              if(force & file.exists(file_out)) file.remove(file_out)
              if(!file.exists(file_out)) system(cmd)
              
              print(paste("Calculate time mean for", paste(it, collapse="-")))
              file_out <- paste0(dirname(file), "/", 
                                 paste0("timmean_", gsub(".nc", 
                                                         paste0("_", years[step1], "-", years[step2],".nc"), 
                                                         basename(file)) ))
              cmd <- paste0("cdo timmean -seltimestep,", step1, ",", step2, 
                            " -select,name=", param, " ", file, " ", file_out)
              if(force & file.exists(file_out)) file.remove(file_out)
              if(!file.exists(file_out)) system(cmd)
            }

            file_out <- file.path(path_out, paste0("fldmean_", basename(file)))
            if(force) file.remove(file_out)
            if(!file.exists(file_out)) {
              file_norway <- gsub(".nc", "_Norway.nc", file_out)
              system(paste("cdo ifthen", shape_norway, file, "tmp.nc"))
              system(paste("cdo -fldmean tmp.nc tmp_fldmean.nc"))
              system(paste0("cdo chname,", param, ",", paste0(param,"_Norway"), 
                            " tmp_fldmean.nc ", file_norway))
              for(region in regions) {
                system(paste0("cdo select,name=", region, " ", shape_regions, " tmp_shape.nc"))
                system(paste("cdo ifthen tmp_shape.nc", file, " tmp_masked.nc"))
                file_region <- gsub(".nc", paste0("_",region,".nc"), file_out)
                system(paste("cdo -fldmean tmp_masked.nc tmp_fldmean.nc"))
                system(paste0("cdo chname,", param, ",", paste0(param,"_",region), 
                              " tmp_fldmean.nc ", file_region))
              }
              system(paste("cdo merge", gsub(".nc", "_*.nc", file_out), file_out))
              system(paste("rm", gsub(".nc", "_*.nc", file_out)))
              system("rm tmp*.nc")
            }
          }
        }
      }
    }
  }
}
