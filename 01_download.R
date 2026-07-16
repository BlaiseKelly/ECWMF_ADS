## script for downloading the free ECMWF reanalysis (ra) data. There are two variations shown below, downloading the 'land' (9km2) and the 'single layer' (30km2)

library(dplyr)
library(sf)
library(ecmwfr)

## define domain
min_lon <- -5
max_lon <- 5

min_lat <- 52
max_lat <- 56

# put in format for the function
ecmwf_land_area <- c(max_lat, min_lon, min_lat, max_lon)

##output path, don't put a / at the end or will return an error
path_out <- "X:/ECMWF/ads/"

##input ecmwf user id
api_key <- ""

wf_set_key(key = api_key)

##define variables to download. list is available here: https://ads.atmosphere.copernicus.eu/datasets/cams-europe-air-quality-forecasts?tab=download
variables <- c("alder_pollen",
               "ammonia",
               "birch_pollen",
               "carbon_monoxide",
               "formaldehyde",
               "glyoxal",
               "grass_pollen",
               "mugwort_pollen",
               "nitrogen_dioxide",
               "nitrogen_monoxide",
               "non_methane_vocs",
               "olive_pollen",
               "ozone",
               "particulate_matter_2.5um",
               "pm2.5_ammonium",
               "pm2.5_nitrate",
               "residential_elementary_carbon",
               "secondary_inorganic_aerosol",
               "pm2.5_sulphate",
               "total_elementary_carbon",
               "pm2.5_total_organic_matter",
               "particulate_matter_10um",
               "dust",
               "pm10_sea_salt_dry",
               "pm10_wildfires",
               "peroxyacyl_nitrates",
               "ragweed_pollen",
               "sulphur_dioxide")

# variable for this run
variables <- variables[c(14,22,25)]

# all the models
models <-  c('chimere', 'dehm', 'emep','ensemble', 'euradim', 'gemaq','lotos', 'match', 'minni','mocage', 'monarch', 'silam')

# which one
#models <- models[4]

# define the year
yrz <- c("2026")
mnths <- sprintf("%02d", seq(1:12))
mnths = 7
##downloads to a directory 'data' at the same level as the script is saved
#dir.create("dat/")
#path_out <- "dat/"


##ECMWF data
for(v in unique(variables)){
  
  for (mod in models){
    
    for (y in yrz){
      
      for (m in mnths){
    
print(paste("getting ", v,mod,y,m))
      
      request <- list(
        date = paste0(y, "-",m,"-12/", y, "-",m,"-18"),
        format = "netcdf",
        variable = v,
        time = "00:00",
        leadtime_hour = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
        level = '0',
        model = mod,
        type = "forecast",
        area = ecmwf_land_area,
        dataset_short_name = "cams-europe-air-quality-forecasts",
        target = paste0(mod, "_", v, "_",m,"_", y, ".nc")
      )
      
      # If you have stored your user login information
      # in the keyring by calling cds_set_key you can
      # call:
      file <- wf_request(  # user ID (for authentication)
        request  = request,  # the request
        transfer = TRUE,     # download the file
        path     = path_out       # store data in current working directory
      )
      
    }
    
  }
  
}

}
