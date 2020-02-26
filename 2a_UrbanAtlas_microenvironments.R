library(raster)
library(rgdal)
library(ncdf4)

####input
citychem_statrecp <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/01/statrecp.nc"

urban_atlas_population_grid <- "/gpfs/work/ramacher/storage/EXPOSURE/population/UA_Population_grid_100m_HH.tif"

urban_atlas_shapefile <- "/gpfs/work/ramacher/storage/EXPOSURE/UA2012/DE002L1_HAMBURG/Shapefiles/DE002L1_HAMBURG_UA2012.shp"

### fraction of blue collar workers (white collar = 1 - blue_collar)
blue_collar <- 0.454

### folder with code 2012 raster files derived from Urban Atlas 2012
output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/ua_landuse/"


############# SECTION 1: Rasterizing of UrbanAtlas Land Use Code Polygons

### define domain based on CityChem Output (statrecp.nc)
domain <- nc_open(citychem_statrecp)
crs <- ncatt_get(nc = domain, varid = 0, attname = "proj4_string")$value
x <- ncatt_get(nc = domain, varid = 0, attname = "XCELL")$value
y <- ncatt_get(nc = domain,varid = 0, attname = "YCELL")$value
domain <- raster(nrow = domain$dim$i$len, ncol = domain$dim$j$len,
                 xmn = min(domain$dim$i$vals-x/2), ymn = min(domain$dim$j$vals-y/2),
                 xmx = max(domain$dim$i$vals+x/2), ymx = max(domain$dim$j$vals+y/2), 
                 crs = crs)
domain <- setValues(domain,rep(1,ncell(domain)))

###read urban Atlas Shape file
UA2012 <- readOGR(urban_atlas_shapefile, integer64 = "warn.loss", stringsAsFactors = F)
### transform UA2012 to CRS (projection) of grid definition
UA2012 <- spTransform(UA2012, crs)
### crop UA2012 layer to our domain and keep only CODE2012 as variables
UA2012 <- UA2012[c("CODE2012")]
UA2012@data$CODE2012 <- as.integer(UA2012$CODE2012)
UA2012 <- crop(UA2012, domain)

### Rasterizing UrbanAtlas Polygon Land Use (CODE2012) data based on domain definition from CTM output
## For each domain cell of of our domain, the intersecting UrbanAtlas polygons are cropped.
## Then a land cover is selected and each polygon which falls within the domain cell is cropped.
## The cropped polygon is used to calculate the cover of the selected domain cell, whic is
## then stored as value in the domain cell. This is repeated for all domain cells of our domain
## and for all land cover codes.
## Thus, this function is an equal-area rasterization of UrbanAtlas land use types.

print("Converting UA2012 land use codes in UrbanAtlas polygons to raster layer based on domain definition of CTM output")

### aggregate domain by a factor of 10 and crop UA2012 layer to reduce calculation costs in loops
grid_agg <- aggregate(domain,10)

### lists for outputs
ua_tiles_agg <- list()
ua_tiles <- list()
UA_CODE2012_list <- list()

### List of all Urban Atlas Codes of interest
#UA_CODE <- unique(UA2012$CODE2012)
UA_CODE <- c(11100,12100,13100,13300,12300,12210,12220,14100,14200)

### initiate progress bar
progress_bar_cluster = txtProgressBar(min = 0, max = ncell(grid_agg), initial = 0, style = 3) 

for(k in 1:length(UA_CODE))
{
  #k <- 1
  for(j in 1:ncell(grid_agg))
  #for(j in 1:10)
  {  
    #j <- 1
    cell_agg <- rasterFromCells(grid_agg, j)
    cell_agg_poly <- crop(UA2012, cell_agg)
    #spplot(cell_agg_poly["CODE2012"])
    
    for (i in 1:ncell(crop(domain,cell_agg)))
    {
      #i <- 1
      cell <- rasterFromCells(crop(domain,cell_agg), i)
      cell_poly <- crop(cell_agg_poly, cell)
      #spplot(cell_poly["CODE2012"])
      
      if(is.element(UA_CODE[k],cell_poly$CODE2012))
      {
        cell_poly <- cell_poly[cell_poly$CODE2012==UA_CODE[k],]
        #spplot(cell_poly["CODE2012"])
        ua_tiles[[i]] <- rasterize(cell_poly,cell,getCover=T)
        #plot(ua_tiles[[i]])
      } else
      {
        ua_tiles[[i]] <- setValues(cell,0)
        #plot(ua_tiles[[i]])
      }
    }
    ua_tiles_agg[[j]]  <- do.call(merge, ua_tiles)
    #plot(ua_tiles_agg[[j]])
    setTxtProgressBar(progress_bar_cluster,j)
    }  
  
  ### Merge all raster tiles to one raster
  UA_CODE2012_grid <- do.call(merge, ua_tiles_agg)
  #plot(UA_CODE2012_grid)
  #sum(getValues(UA_CODE2012_grid))
  writeRaster(UA_CODE2012_grid, paste0(output_folder,"UA_CODE2012_",UA_CODE[k] ,"_grid_100m_HH.tif"), overwrite = T)
  UA_CODE2012_list[[k]] <- UA_CODE2012_grid
}

# CHECK: The sum of values of all raster layers summed 
# should be equal to the number of grid cells in our grid definition:
#sum(getValues(sum(stack(UA_CODE2012_list))))==ncell(domain)
#plot(sum(stack(UA_CODE2012_list)))


############# SECTION 2: Preparation of Microenvironments HOME, WORK, TRANSPORT, OTHER

### HOME environment --> equal to (gridded) Urban Atlas Population distribution
home_me <- raster(urban_atlas_population_grid)
home_me <- home_me/sum(getValues(home_me))
plot(home_me)
writeRaster(home_me,paste0(output_folder,"me_home_ua2012.tif"), overwrite = T)

### WORK environment (with splitting into blue collar (industry, port, etc.) 
### and office/service/shops/management/...)
### 11100 Continouous urban fabric = offices/shops/etc. --> 70% work environment 
work_11100 <- raster(paste0(output_folder,list.files(output_folder, pattern = "11100")))*0.7*(1-blue_collar)
### 12100 Industrial, commercial, public, military and private units --> 100% work environment
work_12100 <- raster(paste0(output_folder,list.files(output_folder, pattern = "12100")))*blue_collar
### 13100 Mineral extraction and dump sites --> 100% work environment
work_13100 <- raster(paste0(output_folder,list.files(output_folder, pattern = "13100")))*blue_collar
### 13300 Construction sites --> 100% work environment
work_13300 <- raster(paste0(output_folder,list.files(output_folder, pattern = "13300")))*blue_collar
### 12300 Port areas --> 100% work environment
work_12300 <- raster(paste0(output_folder,list.files(output_folder, pattern = "12300")))*blue_collar

### sum up all work environment to microenvironment work
work_me <- sum(stack(work_11100,work_12100,work_13100,work_13300,work_12300))
### normalise environment with total of fractions
work_me <- work_me/sum(getValues(work_me))
plot(work_me)
writeRaster(work_me,paste0(output_folder,"me_work_ua2012.tif"), overwrite = T)

########## OTHER Environment (Green Urban Areas, Sport/Leisure Facilities and a bit of continouous fabric)
### 14100 Green Urban Areas
other_14100 <- raster(paste0(output_folder,list.files(output_folder, pattern = "14100")))
### 14200 Sports and Leisure Facilities
other_14200 <- raster(paste0(output_folder,list.files(output_folder, pattern = "14200")))
### 11100 <-  Continouous urban fabric = offices/shops/etc. --> 10% other environment 
other_11100 <- raster(paste0(output_folder,list.files(output_folder, pattern = "11100")))*0.1

### sum up all other environment to microenvironment other
other_me <- sum(stack(other_14100,other_14200,other_11100))
### normalise other with total of fractions
other_me <- other_me/sum(getValues(other_me))
plot(other_me)
writeRaster(other_me,paste0(output_folder,"me_other_ua2012.tif"), overwrite = T)

### TRANSPORT/TRAFFIC environment from Urban Atlas
## can be replaced with transport environment from OSM
### 12210 Fast transit roads and associated land --> 50% transport environment due to a lot assoc. land
transport_12210 <- raster(paste0(output_folder,list.files(output_folder, pattern = "12210")))*0.5
### 12220 Other roads and associated land
transport_12220 <- raster(paste0(output_folder,list.files(output_folder, pattern = "12220")))

### sum up all (road) traffic environment to microenvironment transport/traffic
transport_me <- sum(stack(transport_12210, transport_12220))
### normalise transport environment with total of fractions
transport_me <- transport_me/sum(getValues(transport_me))
plot(transport_me)
writeRaster(transport_me,paste0(output_folder,"me_transport_ua2012.tif"), overwrite = T)