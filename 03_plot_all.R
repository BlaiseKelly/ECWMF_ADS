library(terra)
library(raster)
library(lubridate)
library(dplyr)
library(tmap)
library(reshape2)
library(sf)

load("dat/aq_dat.RData")

varz <- data.frame(ecmwf_var = c("ozone", "nitrogen_dioxide", "nitrogen_monoxide", "ammonia", "sulphur_dioxide", "na","carbon_monoxide", "methane", "pm25", "pm10","pm10_wildfires", "non_methane_vocs"),
                   formula = c("o3", "no2", "no", "nh3", "so2", 'hno3', "co", "ch4", "tpm25", "tpm10","pm10", "tnmvoc"),
                   units = c(10^9*2,10^9*2, 10^9*2,10^9*2,10^9*2, 10^9*2,10^9*2,10^9*2, 10^9,10^9,10^9,10^9),
                   max = c(300, 120, 50, 100, 100, 100, 40, 50, 50, 80,80, 60),
                   seq = c(20, 20, 5, 10, 10, 10, 10, 5, 5, 10,10, 5),
                   title = c("'O'[3]*' ('* mu*'g/m'^3*')'", "'NO'[2]*' ('* mu*'g/m'^3*')'","'NO ('* mu*'g/m'^3*')'",
                             "'NH'[3]*' ('* mu*'g/m'^3*')'","'SO'[2]*' ('* mu*'g/m'^3*')'","'HNO'[3]*' ('* mu*'g/m'^3*')'",
                             "'CO'*' ('* mu*'g/m'^3*')'", "'CH'[4]*' ('* mu*'g/m'^3*')'","'PM'[2.5]*' ('* mu*'g/m'^3*')'",
                             "'PM'[10]*' ('* mu*'g/m'^3*')'","'PM'[10]*' ('* mu*'g/m'^3*')'","'NMVOC'*' ('* mu*'g/m'^3*')'"))

bs <- list.files("bricks/", full.names = FALSE)

for (b_r in bs){
tryCatch({
b <- rast(paste0("bricks/",b_r))

lyr_nams = colsplit(names(b),"_",c("model","species","species1","start_date","hour"))

start_date <-  paste0(lyr_nams$start_date[1], " 00:00")

d8s_1 = lubridate::ymd_hm(start_date) + lubridate::hours(as.numeric(lyr_nams$hour)-1)

d8s_df = data.frame(date = d8s_1)

d8s = format(d8s_1)

all_pm2p5 = all_pm25
aq_pm2p5 = aq_pm25

if(!lyr_nams$species[1] == "pmwf"){
  
  aq_dat = get(paste0("all_",lyr_nams$species[1]))
  
  aq_d8s = left_join(d8s_df,aq_dat,by = "date")
  
  names(aq_d8s) = c("date","code","species")
  
  aq_sites = get(paste0("aq_",lyr_nams$species[1])) |> 
    select(code,geometry)

  aq_df = left_join(aq_d8s,aq_sites,by = "code") |> 
    filter(!is.na(code)) |> 
    st_as_sf()
  
  aq_df2 = aq_df[st_is_valid(aq_df),]
  
  }

# aq_d = filter(aq_df, date == aq_df$date[1])
# 
# mapview(aq_d)

# create scale
conc_bks <- c(0,0.1,0.25,0.5,1,2,4,8,20,40,80,120)
if(lyr_nams$species[1] == "pm2p5"){
conc_bks = seq(0,40,5)
leg_txt = "'PM'[2.5]*' ('* mu*'g/m'^3*')'"
}
if(lyr_nams$species[1] == "pm10"){
  conc_bks = seq(0,80,10)
  leg_txt = "'PM'[10]*' ('* mu*'g/m'^3*')'"
}
if(lyr_nams$species[1] == "pmwf"){
  conc_bks = seq(0,30,5)
  leg_txt = "'PM'[10]*' ('* mu*'g/m'^3*')'"
}


## define palette for population density
conc_pal <- cols4all::c4a("tol.incandescent", n = NROW(conc_bks))

# get basemap
bg <- basemaps::basemap_raster(b[[1]], map_res = 0.99, map_service = "carto", map_type = "light")

# set to override limit of 64 frames
tmap_options(facet.max = 100)

names(b) = d8s

# ## create plot using tmap
# tm_ws <- tm_shape(bg)+
#   tm_rgb()+
#   tm_shape(b[[1:5]]) +
#   tm_raster(col.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks),col_alpha = 0.6, col.legend = tm_legend(title = parse(text = leg_txt)))+
#   tm_legend(position = c(0.80,0.988), frame = FALSE)+
#   tm_shape(aq_df)+
#   tm_dots(fill = "species",size = 1, fill.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks),fill_alpha = 0.6,fill.legend = tm_legend(show = FALSE))+
#   tm_layout(frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
#   tm_title(text = paste0("CAMS reanalysis Ensemble median\n ",d8s), position = c(0.05,0.96))+
#   tm_facets(nrow = 1, ncol = 1)+
#   tm_animate(fps = 3) +
#   tm_credits("Source: CAMS European air quality reanalyses. Copernicus Atmosphere Monitoring Service (CAMS) Atmosphere Data Store, DOI: 10.24381/7cc0465a")
# 
# #tmap_animation(tm_ws, filename = paste0("plots/",lyr_nams$model[1],"_",lyr_nams$species[1],".gif"), width =2900, height = 3000, dpi = 300, delay = 40)
# tmap_animation(tm_ws, filename = paste0("plots/",lyr_nams$model[1],"_",lyr_nams$species[1],".gif"),dpi = 200,  delay = 40)
# #tmap_animation(tm_ws, filename = paste0("plots/wildfires.gif"), width =5600, height = 6000, dpi = 500, delay = 40)
# i=1
# #df2 = dplyr::filter(aq_df, date == d8s_1[96-24])

b = b[[1:(96-24)]]

m_nam = stringr::str_to_title(lyr_nams$model[1])

frames = purrr::map(seq_len(nlyr(b)), \(i) {
  tm_shape(bg) + tm_rgb() +
    tm_shape(b[[i]]) + tm_raster(col.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks),col_alpha = 0.6, col.legend = tm_legend(title = parse(text = leg_txt))) +
    tm_layout(frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
    tm_legend(position = c(0.80,0.988), frame = FALSE)+
    tm_shape(dplyr::filter(aq_df, date == d8s_1[i])) +
    tm_dots(fill = "species",size = 0.6, col = "grey", fill.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks), fill_alpha = 0.6,fill.legend = tm_legend(show = FALSE)) +
    tm_title(text = paste0(m_nam,"\n ",d8s[i]), position = c(0.05,0.96))+
    tm_credits("Source: CAMS European air quality reanalyses. Copernicus Atmosphere Monitoring Service (CAMS) Atmosphere Data Store, DOI: 10.24381/7cc0465a")+
    tm_animate(fps=2)
})

tmap_animation(frames, paste0("plots/",lyr_nams$model[1],"_",lyr_nams$species[1],".gif"),dpi = 180,width =1350, height = 1700)

}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}



hist(wm, main = "Raster Value Distribution")

min(b_plot)

wm <- app(b,which.max)

plot(wm)

freq_table <- freq(wm)

vals <- data.frame(values(wm))

# Convert raster to values vector
vals <- values(b)

# Frequency distribution
freq_dist <- table(vals)

y <- 2025

start_date <-  paste0(y, "-01-01", "00:00")

end_date <- paste0(y, "-09-30", "23:00")

kr8_d8 <- seq(
  from=as.POSIXct(start_date, tz="UTC"),
  to=as.POSIXct(end_date, tz="UTC"),
  by="hour"
)

kr8_days <- as.character(seq(
  from=as.POSIXct(start_date, tz="UTC"),
  to=as.POSIXct(end_date, tz="UTC"),
  by="day"
) )







names(b) <- dayz







vals2find  <- c(3, 8, 12, 5, 8, 2)
listofvals <- c(2, 5, 12)

which(vals2find %in% listofvals)

md <- app(r_daily,max)
values(md)[values(md) > 0] <- NA

plot(wmd)
