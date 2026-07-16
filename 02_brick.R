library(ncdf4)
library(tidyverse)
library(sf)
library(reshape2)
library(stringr)
library(raster)

#import_surf_meteo <- function(path = 'mod_files/output', pattern = "_meteo_", variable = 'temper', write_out = FALSE,  output_crs = 4326){

varz <- data.frame(ecmwf_var = c("ozone", "nitrogen_dioxide", "nitrogen_monoxide", "ammonia", "sulphur_dioxide", "na","carbon_monoxide", "methane", "pm25", "pm10","pm10_wildfires", "non_methane_vocs"),
                   formula = c("o3", "no2", "no", "nh3", "so2", 'hno3', "co", "ch4", "tpm25", "tpm10","pm10", "tnmvoc"),
                   units = c(10^9*2,10^9*2, 10^9*2,10^9*2,10^9*2, 10^9*2,10^9*2,10^9*2, 10^9,10^9,10^9,10^9),
                   max = c(300, 120, 50, 100, 100, 100, 40, 50, 50, 80,80, 60),
                   seq = c(20, 20, 5, 10, 10, 10, 10, 5, 5, 10,10, 5),
                   title = c("'O'[3]*' ('* mu*'g/m'^3*')'", "'NO'[2]*' ('* mu*'g/m'^3*')'","'NO ('* mu*'g/m'^3*')'",
                             "'NH'[3]*' ('* mu*'g/m'^3*')'","'SO'[2]*' ('* mu*'g/m'^3*')'","'HNO'[3]*' ('* mu*'g/m'^3*')'",
                             "'CO'*' ('* mu*'g/m'^3*')'", "'CH'[4]*' ('* mu*'g/m'^3*')'","'PM'[2.5]*' ('* mu*'g/m'^3*')'",
                             "'PM'[10]*' ('* mu*'g/m'^3*')'","'PM'[10]*' ('* mu*'g/m'^3*')'","'NMVOC'*' ('* mu*'g/m'^3*')'"))

vars <- c("particulate_matter_2.5um","particulate_matter_10um","pm10_wildfires")  

  path = 'X:/ECMWF/ads/'
  pattern = vars
  variable = varz$formula[varz$ecmwf_var == pattern]
  write_out = FALSE
  output_crs = 4326
  
  files_p <- list.files(path, full.names = FALSE)
  files <- files_p[!grepl('metout', files_p)] ## metout files are generated as part of defualt model run
  files <- files[grepl('.nc', files)]
  ## open one file to get species
  lefile <- nc_open(paste0(path, files[1]))
  
  longitude <- ncvar_get(lefile, "longitude")
  latitude <- ncvar_get(lefile, "latitude")
  lon_res <- (longitude[2]-longitude[1])/2
  lat_res <- (latitude[1]-latitude[2])/2
  
  ##create min x and y
  x_min <- round(min(longitude[longitude>100])-(longitude[2]-longitude[1]),2)-360
  x_max <- round(max(longitude[longitude>100])-(longitude[2]-longitude[1]),2)-360
  y_min <- round(min(latitude)-(latitude[2]-latitude[1]),2)
  y_max <- round(max(latitude)+(latitude[2]-latitude[1]),2)
  ##determine number of lat and lon points
  n_y <- NROW(latitude)
  n_x <- NROW(longitude)
  
  ##work out number of x and y cells
  res_x <- (x_max-x_min)/n_x
  res_y <- (y_max-y_min)/n_y
  
  ##create a data frame to setup polygon generation
  df <- data.frame(X = c(x_min, x_max, x_max, x_min),
                   Y = c(y_max, y_max, y_min, y_min))
  
  ##generate a polygon of the area
  vgt_area <- df %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    dplyr::summarise(data = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  nc_close(lefile)
  #rstz <- list()
  for (f in files){
    
    lefile <- nc_open(paste0(path, f))
    
    fname <- colsplit(f, "_", c("a", "b", "c","d", "date"))
    TIME_str = ncatt_get(lefile,"time")
    TIME = gsub("FORECAST time from ","",TIME_str$long_name)

    d8 = lubridate::ymd(TIME) + lubridate::hours(lefile$dim$time$vals)
    
    variable <- names(lefile$var)
    
    var_in <- ncvar_get(lefile, variable, start = c(1,1,1,1), count = c(n_x, n_y, 1, NROW(d8)))
    
    ##generate raster from it
    var <- t(brick(var_in[1:n_x,1:n_y, 1:NROW(d8)]))
    
    #plot(subset(var,200))
    
    ##define the extent
    crs(var) <- 4326
    bb <- extent(vgt_area)
    extent(var) <- bb
    
    plot(var[[1]])

    names(var) <- as.character(d8)
    
    nam_out <- TIME
    
    terra::writeRaster(var, filename=paste0("bricks/",fname$a, "_", variable,"_",nam_out, ".TIF"), overwrite = TRUE)
    
    #rstz[[nam_out]] <- var
    
    print(paste(nam_out, variable))
    
    
  }
  
  #all_bricks <- brick(rstz)
  
  #wm <- calc(rstz[[1]], which.max)
  
  
  
  
  ab <- brick(all_bricks)
  
  sapply(rstz, function(x) length(getValues(x)))
  
  b1 <- rstz[[1]]
  

    
    dir.create('bricks/', recursive = TRUE)
    ## convert to a terra geospatial raster and use terra writeRaster function to preserve layer names
    
    

