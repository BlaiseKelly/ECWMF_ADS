library(terra)
library(raster)
library(lubridate)
library(dplyr)
library(tmap)

y <- 2025

start_date <-  paste0(y, "-01-01", " 00:00")

end_date <- paste0(y, "-09-30", " 23:00")

kr8_d8 <- seq(
  from=as.POSIXct(start_date, tz="UTC"),
  to=as.POSIXct(end_date, tz="UTC"),
  by="hour"
)

bs <- list.files("bricks", "pmwf_", full.names = TRUE)

b <- rast(bs)

# for each cell find the hour when the concentration was the highest
wm <- app(b,which.max)

# create scale
plot_bks <- seq(0,max(values(wm)),600)

bg <- basemaps::basemap_raster(wm, map_service = "carto", map_type = "light")

## create plot using tmap
tm1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(wm) +
  tm_raster(col.scale = tm_scale_intervals(values = "brewer.paired",value.na = NA, breaks = plot_bks),col_alpha = 0.6, col.legend = tm_legend(title = "hour"))+
  tm_legend(position = c(0.85,0.988), frame = FALSE)+
  tm_layout(frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
  tm_title(text = paste0("Which hour did each cell have the max concentration\nin the period ",start_date, " to ", end_date),position = c(0.03,0.96))+
  tm_credits("Source: CAMS European air quality reanalyses. Copernicus Atmosphere Monitoring Service (CAMS) Atmosphere Data Store, DOI: 10.24381/7cc0465a")

tmap_save(tm1, filename = paste0("plots/which_max.png"), width =1900, height = 2000, dpi = 220)

# plot frequency of this
hist(wm, main = "Raster Value Distribution")

# sort descending
freq_table <- freq(wm) |> 
  arrange(desc(count))

vals2get <- freq_table |> filter(value > value[1]-15 & value < value[1]+15)

sum(freq_table$count[1])/sum(freq_table$count)

max_hour <- format(kr8_d8)[freq_table$value[1]]

min_d82plot <- format(kr8_d8[freq_table$value[1]-48])
max_d82plot <- format(kr8_d8[freq_table$value[1]+48])

# manually pick out a range from the above
d8s2plot <- seq(
  from=as.POSIXct(min_d82plot, tz="UTC"),
  to=as.POSIXct(max_d82plot, tz="UTC"),
  by="hour"
)

# which layers of the original hour raster does this correspond to
layers2get <- match(d8s2plot, kr8_d8)

#swap the layer names again for the actual hourly dates
names(b) <- kr8_d8

# subset the raster brick
b_plot <- subset(b,min(layers2get):max(layers2get))

# make v low values NA so they don't mask the underlying map
b_plot[b_plot < 0.05] <- NA

# create scale
conc_bks <- c(0,1,2,4,8,20,40,80, 100,200,500)

## define palette for population density
conc_pal <- cols4all::c4a("tol.incandescent", n = NROW(pop_bks))

# get basemap
bg <- basemaps::basemap_raster(b_plot, map_res = 0.99, map_service = "carto", map_type = "light")

# set to override limit of 64 frames
tmap_options(facet.max = 200)

## create plot using tmap
tm_ws <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(b_plot) +
  tm_raster(col.scale = tm_scale_intervals(values = conc_pal,value.na = NA, breaks = conc_bks),col_alpha = 0.6, col.legend = tm_legend(title = parse(text = "'PM'[10]*' ('* mu*'g/m'^3*')'")))+
  tm_legend(position = c(0.85,0.988), frame = FALSE)+
  tm_layout(frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
  tm_title(text = paste0("CAMS reanalysis Ensemble median\n", format(d8s2plot)), position = c(0.05,0.96))+
  tm_facets(nrow = 1, ncol = 1)+
  tm_credits("Source: CAMS European air quality reanalyses. Copernicus Atmosphere Monitoring Service (CAMS) Atmosphere Data Store, DOI: 10.24381/7cc0465a")

tmap_animation(tm_ws, filename = paste0("plots/wildfires.gif"), width =2900, height = 3000, dpi = 300, delay = 40)
#tmap_animation(tm_ws, filename = paste0("plots/wildfires.gif"), width =5600, height = 6000, dpi = 500, delay = 40)

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
