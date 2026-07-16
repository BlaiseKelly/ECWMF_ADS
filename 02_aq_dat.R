library(openair)
library(terra)
library(dplyr)

select = dplyr::select()

area_r = rast("bricks/dehm_pm10_conc_20260712.TIF")

bbox = st_bbox(area_r[[1]]) |> 
  st_as_sfc() |> 
  st_as_sf()


aq_sites = openair::importMeta(source = "all", all = TRUE) |>
  filter(site_type %in% c("Urban Background","Rural Background") & variable == "PM10") |>
  mutate(across(c(start_date, end_date), as.Date)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  filter(start_date < as.Date("2026-07-12"),
         is.na(end_date) | end_date > as.Date("2026-07-15"))


aq_pm10 = aq_sites[bbox,]

mapview(aq_in)

u_sites = aq_pm10$code
sites = list()
for (w in u_sites){
  tryCatch({
    site_df <- filter(aq_in, code == w)
    
    if(NROW(site_df) >1){
      site_df = site_df[1,]
    }
    
    start_yr = 2026
    end_yr = 2026
    
    if(site_df$source == "aurn"){
      
      site_dat = importAURN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "local"){
      
      site_dat = importLocal(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "aqe"){
      
      site_dat = importAQE(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "saqn"){
      
      site_dat = importSAQN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "waqn"){
      
      site_dat = importWAQN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "ni"){
      
      site_dat = importNI(site_df$code,year = start_yr:end_yr)
      
    }
    
    
    df = site_dat |> 
      ungroup() |> 
      filter(date>"2026-07-11") |> 
      transmute(code,date,pm10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    sites[[w]] = df
    print(w)
}

all_pm10 = do.call(rbind,sites)



aq_sites = openair::importMeta(source = "all", all = TRUE) |>
  filter(site_type %in% c("Urban Background","Rural Background") & variable == "PM2.5") |>
  mutate(across(c(start_date, end_date), as.Date)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  filter(start_date < as.Date("2026-07-12"),
         is.na(end_date) | end_date > as.Date("2026-07-15"))


aq_pm25 = aq_sites[bbox,]

mapview(aq_in)

u_sites = aq_pm25$code
sites_pm25 = list()
for (w in u_sites){
  tryCatch({
    site_df <- filter(aq_in, code == w)
    
    if(NROW(site_df) >1){
      site_df = site_df[1,]
    }
    
    start_yr = 2026
    end_yr = 2026
    
    if(site_df$source == "aurn"){
      
      site_dat = importAURN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "local"){
      
      site_dat = importLocal(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "aqe"){
      
      site_dat = importAQE(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "saqn"){
      
      site_dat = importSAQN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "waqn"){
      
      site_dat = importWAQN(site_df$code,year = start_yr:end_yr)
      
    }
    
    if(site_df$source == "ni"){
      
      site_dat = importNI(site_df$code,year = start_yr:end_yr)
      
    }
    
    
    df = site_dat |> 
      ungroup() |> 
      filter(date>"2026-07-11") |> 
      transmute(code,date,pm2.5)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  sites_pm25[[w]] = df
  print(w)
}

all_pm25 = do.call(rbind,sites_pm25)
dir.create("dat")
save(all_pm10,all_pm25,aq_pm25,aq_pm10,file = "dat/aq_dat.RData")    

