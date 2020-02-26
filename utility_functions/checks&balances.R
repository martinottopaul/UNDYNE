library(raster)
source("/gpfs/work/ramacher/storage/EXPOSURE/utility_functions/raster_sums.R")

setwd("/gpfs/work/ramacher/storage/EXPOSURE/output/Hamburg/")

static <- stack("static/total_static_exposure_2016.nc")
dynamic <- stack("dynamic_ua/total_dynamic_exposure_ua_2016.nc", varname = "Total Exposure to NO2")
osm <- stack("dynamic_osm/total_dynamic_OSM_exposure_2016.nc", varname = "Total Exposure to NO2")

### raster
static_total <- raster("static/avg_total_static_exposure_2016.nc")
plot(static_total)
dynamic_total <- raster("dynamic_ua/avg_total_dynamic_ua_exposure_2016.nc", varname = "Total Exposure to NO2")
plot(dynamic_total)
osm_total <- raster("dynamic_osm/avg_total_dynamic_osm_exposure_2016.nc", varname = "Total Exposure to NO2")
plot(osm_total)

diff_ua_total <- static_total-dynamic_total
plot(diff_ua_total)
plot(diff_ua_total/static_total*100)
diff_osm_total <- static_total-osm_total
plot(diff_osm_total)
plot(diff_osm_total/static_total*100)

plot(osm_total-dynamic_total)

### sum of raster values
static_sum <- raster_sums(static)
dynamic_sum <- raster_sums(dynamic)
osm_sum <- raster_sums(osm)

library(openair)
date <- as.POSIXct(t$vals, origin="1970-01-01 00:00:00", tz = "GMT")
conc <- stack(statrecphour, varname = pollutant)
conc_mean <- raster_means(conc)
myData <- data.frame(date,static_sum,dynamic_sum,osm_sum, conc_mean)

timePlot(myData, pollutant = c("static_sum","dynamic_sum","osm_sum"), avg.time = "day", stack = F)
timeProp(myData, pollutant = "dynamic_sum", proportion = "static_sum")
timeVariation(myData, pollutant = c("static_sum","dynamic_sum", "osm_sum"))
timeVariation(myData, pollutant = c("dynamic_sum", "osm_sum"), difference = T, normalise = T)
scatterPlot(myData, x ="dynamic_sum", y="osm_sum", z = "conc_mean")
trendLevel(myData, pollutant = "static_sum", y = "conc_mean")
myData$ratio <- myData$dynamic_sum/myData$osm_sum
trendLevel(myData, pollutant = "ratio")

