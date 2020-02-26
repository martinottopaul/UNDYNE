### script to download OSM data for different transport environments
### transport environments are: regional trains, light trains, subway, bus, car, bike, foot
### for each environment linestring data is downloaded and converted to a raster grid with
### grid cell values = length of intersected lines in a grid cell / total length of all lines in domain
### this grid can then be combined with (weighted) population sums to distribute population 
### in each transport environment
### (c) Martin Otto Paul Ramacher (Helmholtz-Zentrum Geesthacht), 2020/15/01

#load packages
library(osmdata)
library(sf)
library(raster)
source("/gpfs/work/ramacher/storage/EXPOSURE/utility_functions/osm_lines_to_area_fractions.R")

### output folder
output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/"

### define domain based on CityChem Output (statrecp.nc)
domain <- nc_open("/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/ref/01/statrecp.nc")
crs <- ncatt_get(nc = domain, varid = 0, attname = "proj4_string")$value
x <- ncatt_get(nc = domain, varid = 0, attname = "XCELL")$value
y <- ncatt_get(nc = domain,varid = 0, attname = "YCELL")$value
domain <- raster(nrow = domain$dim$i$len, ncol = domain$dim$j$len,
                 xmn = min(domain$dim$i$vals-x/2), ymn = min(domain$dim$j$vals-y/2),
                 xmx = max(domain$dim$i$vals+x/2), ymx = max(domain$dim$j$vals+y/2), 
                 crs = crs)
domain <- setValues(domain,rep(1,ncell(domain)))
domain_sf <- rasterToPolygons(domain)
domain_sf <- spTransform(domain_sf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

### download all transport environments from OSM

## regional trains
regio <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("usage",c("main")) %>%  add_osm_feature("railway","rail")
regio <- osmdata_sf(regio)
regio <- regio$osm_lines
#plot(regio[,"osm_id"])
regio_grid <- osm_area_fraction(domain,osm_lines = regio)
#plot(regio_grid)
writeRaster(regio_grid, paste0(output_folder,"me_regionaltrains_osm.tif"), overwrite =T)

## s-bahn / light_rail trains
light_rail <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("railway",c("light_rail"))
light_rail <- osmdata_sf(light_rail)
light_rail <- light_rail$osm_lines
#plot(light_rail[,"osm_id"])
light_rail_grid <- osm_area_fraction(domain,light_rail)
#plot(light_rail_grid)
writeRaster(light_rail_grid, paste0(output_folder,"me_suburbantrains_osm.tif"), overwrite =T)

## u-bahn / subway trains
subway <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("railway",c("subway"))
subway <- osmdata_sf(subway)
subway <- subway$osm_lines
#plot(subway[,"osm_id"])
subway_grid <- osm_area_fraction(domain, subway)
#plot(subway_grid)
writeRaster(subway_grid, paste0(output_folder,"me_subway_osm.tif"), overwrite =T)

## bus (no highways, only major roads, no residential roads)
bus <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("highway", c("primary", "primary_link", "secondary", "secondary_link",
                                                                       "tertiary", "tertiary_link"))
bus <- osmdata_sf(bus)
bus <- bus$osm_lines
#plot(bus[,"osm_id"])
bus_grid <- osm_area_fraction(domain, bus)
#plot(bus_grid)
writeRaster(bus_grid, paste0(output_folder,"me_bus_osm.tif"), overwrite =T)

## car (same categories as used in emissions processing)
car <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("highway", c("motorway", "motorway_link","trunk","trunk_link",
                                                                       "primary", "primary_link", "secondary", "secondary_link",
                                                                       "tertiary", "tertiary_link"))
car <- osmdata_sf(car)
car <- car$osm_lines
car <- car[is.na(car$tunnel),]
#plot(car[,"osm_id"])
car_grid <- osm_area_fraction(domain, car)
#plot(car_grid)
#writeRaster(car_grid, paste0(output_folder,"me_car_osm_secondary.tif"), overwrite =T)
writeRaster(car_grid, paste0(output_folder,"me_car_osm_tertiary.tif"), overwrite =T)
#writeRaster(car_grid, paste0(output_folder,"me_car_osm_residential.tif"), overwrite =T)

## bike (all possibilities to ride a bike in the city)
bike1 <- osmdata_sf(opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("bicycle",c("yes")) %>% add_osm_feature("highway"))
#plot(bike1$osm_lines[,"osm_id"])
bike2 <- osmdata_sf(opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("cycleway"))
#plot(bike2$osm_lines[,"osm_id"])
bike3 <- osmdata_sf(opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("highway",c("cycleway")))
#plot(bike3$osm_lines[,"osm_id"])
bike <- rbind(bike1$osm_lines[,1],bike2$osm_lines[,1],bike3$osm_lines[,1])
bike <- unique(bike)
#plot(bike[,"osm_id"])
bike_grid <- osm_area_fraction(domain, bike)
#plot(bike_grid)
writeRaster(bike_grid, paste0(output_folder,"me_bike_osm.tif"), overwrite =T)

## foot
foot <- opq(bbox = st_bbox(domain_sf)) %>% add_osm_feature("highway",c("footway"))
foot <- osmdata_sf(foot)
foot <- foot$osm_lines
#plot(foot[,"osm_id"])
foot_grid <- osm_area_fraction(domain, foot)
#plot(foot_grid)
writeRaster(foot_grid, paste0(output_folder,"me_foot_osm.tif"), overwrite =T)