library(openair)
library(dplyr)
library(terra)
library(reshape2)
library(lubridate)
library(ggplot2)
library(worldmet)
library(sf)

load("dat/aq_dat.RData")

# get brick paths
bs <- list.files("bricks/", full.names = FALSE)
bs = bs[!grepl("meteo",bs)]

# extract modelled data for specific sites
modelled_aq = list()
for (b_r in bs){

    b <- rast(paste0("bricks/",b_r))
    
    lyr_nams = colsplit(names(b),"_",c("model","species","species1","start_date","hour"))
    
    start_date <-  paste0(lyr_nams$start_date[1], " 00:00")
    
    d8s_1 = lubridate::ymd_hm(start_date) + lubridate::hours(as.numeric(lyr_nams$hour)-1)
    
    d8s_df = data.frame(date = d8s_1,
                        hour = seq(1:NROW(d8s_1)))
    
    d8s = format(d8s_1)
    
    all_pm2p5 = all_pm25
    aq_pm2p5 = aq_pm25
    
    if(!lyr_nams$species[1] == "pmwf"){
      
    
      aq_sites = get(paste0("aq_",lyr_nams$species[1])) |> 
        select(code,geometry)
      
    
    model_dat = extract(b,aq_sites) |> 
      mutate(code = aq_sites$code) |> 
      select(-ID) |> 
      reshape2::melt("code") |> 
      mutate(reshape2::colsplit(variable,"_", c("model","species","conc", "date","hour"))) |> 
      select(-date) |> 
      left_join(d8s_df,by = "hour") |> 
      transmute(date,
                model,
                code,
                species,
                conc = value)
    
                
    modelled_aq[[b_r]] = model_dat
    
    }
  
    print(b_r)  
        
}

all_models = do.call(rbind,modelled_aq)

# combine with observations
mod_pm10 = filter(all_models,species == "pm10" & model == "ensemble") |> 
  left_join(all_pm10, by = c("date","code")) |> 
  mutate(dow = wday(date,label = TRUE, abbr = FALSE)) |> 
    distinct(date,code,.keep_all = TRUE) |> 
  select(date,code,mod = conc,obs = pm10) |> 
  filter(!is.na(obs)) |> 
  filter(!is.na(mod)) 

sc12 = filter(mod_pm10,code == "SC12")

mod_pm10_mean = mod_pm10 |> 
  group_by(code) |> 
  summarise(obs = max(obs,na.rm = TRUE))

# pm10 site with data (wd)
aq_pm10_wd = filter(aq_pm10,code %in% mod_pm10$code) |> 
  left_join(mod_pm10_mean, by = "code") |> 
  arrange(desc(obs))

tmap_mode("view")

conc_bks = seq(0,80,10)
leg_txt = "'PM'[10]*' ('* mu*'g/m'^3*')'"
## define palette for population density
conc_pal <- cols4all::c4a("tol.incandescent", n = NROW(conc_bks))

bmap = st_bbox(b) |> 
  st_as_sfc() |> 
  st_as_sf() |> 
  st_buffer(1000)

# get basemap
bg = basemaps::basemap_raster(bmap, map_res = 0.99, map_service = "carto", map_type = "dark")

tm1 = tm_shape(bg)+
  tm_rgb()+
  tm_shape(aq_pm10_wd) +
  tm_dots(fill = "obs",
          size = 0.6, 
          col = "grey", 
          fill.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks), 
          fill_alpha = 0.6,fill.legend = tm_legend(title = parse(text = leg_txt))) +
  tm_text("code",size = 0.5,col = "white", ymod = 1)+ 
  tm_layout(frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
  tm_legend(position = c(0.80,0.988), frame = FALSE)

tmap_save(tm1,"plots/obs_sites.png")


# time plot with all sites
timePlot(mod_pm10, pollutant = c("mod","obs"), type = "code", group = TRUE, date.breaks = 4, ylab = "PM10 (ug/m3)")

ggsave("plots/all_obs.png", units = "px",width = 2200, height = 2000, dpi = 200)

# sites that have spikes
sites_peak = filter(aq_pm10, code %in% aq_pm10_wd$code[1:7] & code %in% mod_pm10$code) |> 
  select(code,geometry)
# all the rest
sites_no_peak = filter(aq_pm10, !code %in% aq_pm10_wd$code[1:7] & code %in% mod_pm10$code)

# get the bb to find met sites nearby
sites_peak_bb = st_bbox(sites_peak) |> 
  st_as_sfc() |> 
  st_as_sf() |> 
  st_buffer(10000)

met_sites = import_ghcn_stations(country = "UK") |> 
  st_as_sf(coords = c("lng", "lat"),crs = 4326) 

met_sites = met_sites[sites_peak_bb,]

# so many met sites have no data, just import all, and match the ones that have data to the obs sites
met_list = list()
for(s in met_sites$id){
  tryCatch({

  met_dat = worldmet::import_ghcn_hourly(station = s, year = 2026) |> 
    select(date,ws,wd, air_temp,rh,cl_baseht) |> 
    mutate(id = s)
  
  print(s)
  met_list[[s]] = met_dat
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# stack together
all_met = do.call(rbind,met_list)

# only consider met sites that had data
met_sites_in = filter(met_sites, id %in% unique(all_met$id))

# match sites with peaks to nearest sites
sites_peak$met_site = met_sites_in$id[st_nearest_feature(sites_peak,met_sites_in)]

# pick out obs codes and the met codes
sites_met_code = sites_peak |> 
  st_set_geometry(NULL) |> 
  left_join(aq_pm10,by = "code") |> 
  select(code,met_site,site,site_type,source) 
  

# join them to the data and the met data
mod_pm10_met = mod_pm10 |> 
  left_join(sites_met_code, by = "code") |> 
  filter(!is.na(met_site)) |> 
  left_join(all_met, c("date", "met_site" = "id"))

for (s in unique(mod_pm10_met$code)){

site1 = filter(mod_pm10_met,code == s) |> 
  mutate(`Ensemble Median` = mod,
         observations = obs)


timePlot(site1, 
         c("Ensemble Median", "observations", "rh", "air_temp"),
         #linewidth = 1,
         subtitle = paste0(site1$site[1], " ", site1$site_type[1]),
         ylab = "temperature (°C),relative humidity (%),observations: PM10 & Ensemble median (ug/m3)",
         scales = "free_y")

ggsave(paste0("plots/",s,"_tp.png"), height = 2500, width = 2000, units = "px")

}

save(mod_pm10_met,aq_pm10_wd,mod_pm10,sites_peak,sites_no_peak, file = "dat/model_data.RData")
