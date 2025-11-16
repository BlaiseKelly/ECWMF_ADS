library(terra)
library(raster)
library(lubridate)
library(dplyr)

bs <- list.files("bricks", "pmwf_", full.names = TRUE)

b <- rast(bs)

hist(wm, main = "Raster Value Distribution")

d8s <- data.frame(names(b))

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





dayz <- yday(kr8_d8)

names(b) <- dayz

r_daily <- tapp(b, index=names(b), fun=mean)

wmd <- app(r_daily,which.max)

hist(wmd, main = "Raster Value Distribution")
freq_table <- freq(wmd) |> 
  arrange(desc(count))

highest_days <- kr8_days[freq_table$value[1:10]]

d8s2plot <- seq(
  from=as.POSIXct("2025-04-06", tz="UTC"),
  to=as.POSIXct("2025-04-11", tz="UTC"),
  by="hour"
)

layers2get <- match(d8s2plot, kr8_d8)

b_plot <- b[[min(layers2get):max(layers2get)]]



vals2find  <- c(3, 8, 12, 5, 8, 2)
listofvals <- c(2, 5, 12)

which(vals2find %in% listofvals)

md <- app(r_daily,max)
values(md)[values(md) > 0] <- NA

plot(wmd)
