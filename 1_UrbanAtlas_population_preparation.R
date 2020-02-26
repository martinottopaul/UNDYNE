### script to distribute Urban Atlas 2012 Population Polygons to
### a raster grid based on a raster domain definition, which follows the
### domain definition as used in CityChem simulations.
### For each grid cell of the domain, the intersecting polygons (containing
### number of population) are gathered and weighted with the fraction of 
### ratio of intersected polygon area and total polygon area.
### The output is rounded to integers and corrected for possible value losses.
### (c) Martin Otto Paul Ramacher (Helmholtz-Zentrum Geesthacht), 2020/01

### inputs
## CityChem statrecp.nc output file (for domain definition)
citychem_statrecp <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/01/statrecp.nc"
## Copernicus Urban Atlas 2012 shapefile (for population polygons)
urban_atlas_shapefile <- "/gpfs/work/ramacher/storage/EXPOSURE/UA2012/DE002L1_HAMBURG/Shapefiles/DE002L1_HAMBURG_UA2012.shp"
## Output folder to store the population grid
output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/population/"
### end of inputs

library(raster)
library(rgdal)
library(ncdf4)

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

print("GRID/RASTER DEFINITION BASED ON CTM OUTPUT netCDF:")
domain

###Urban Atlas Shape file einlesen und in die richtige Projektion bringen
UA2012 <- readOGR(urban_atlas_shapefile, integer64 = "warn.loss")

### transform UA2012 to CRS (projection) of grid definition
UA2012 <- spTransform(UA2012, crs)

### Total Population from UA2012 layer before cropping to our domain
print(paste0("Total Population from UA2012 layer BEFORE cropping to our domain: ", 
             sum(UA2012$Pop2012)))

### Cropping UA2012 layer to our domain and keeping only Pop2012 and Shape_Area as variables
UA2012 <- UA2012[c("Pop2012", "Shape_Area")]
UA2012 <- crop(UA2012, domain)
print(paste0("Total Population from UA2012 layer AFTER cropping to our domain: ", 
             sum(UA2012$Pop2012)))

### Rasterizing UrbanAtlas Polygon Population data based on grid definition from CTM output
## For each grid cell of of our domain, the intersecting UrbanAtlas polygons are cropped.
## Then the area of each polygon which falls within the grid cell is calculated and divided
## by the original area of each polygon. This ratio is then multiplied with the population
## data of each corresponding polygon. The sum of these values is the new grid cell population value.
## Thus, the function is an equal-area rasterization of UrbanAtlas population data to any grid.

print("Converting population data in UrbanAtlas polygons to raster layer based on grid definition of CTM output")

### aggregate grid by a factor of 10 and crop UA2012 layer to reduce calculation costs in loops
grid_agg <- aggregate(domain,10)

### lists for outputs
ua_tiles_agg <- list()
ua_tiles <- list()

### initiate progress bar
progress_bar = txtProgressBar(min = 0, max = ncell(grid_agg), initial = 0, style = 3) 

for(j in 1:ncell(grid_agg))
{  
  cell_agg <- rasterFromCells(grid_agg, j)
  
  if (!is.null(crop(UA2012, cell_agg)))
  {
    cell_agg_poly <- crop(UA2012, cell_agg)
    #spplot(cell_agg_poly["Pop2012"])
    
    for (i in 1:ncell(crop(domain,cell_agg)))
    {
      cell <- rasterFromCells(crop(domain,cell_agg), i)
      if (!is.null(crop(cell_agg_poly, cell)))
      {
        cell_poly <- crop(cell_agg_poly, cell)  
        #spplot(cell_poly["Pop2012"])
        ua_tiles[[i]] <- setValues(cell , sum(area(cell_poly)/cell_poly@data$Shape_Area*cell_poly$Pop2012))
      } else 
      {
        ua_tiles[[i]] <- setValues(cell , 0)
      }
      #plot(ua_tiles[[i]])
    }
    ua_tiles_agg[[j]]  <- do.call(merge, ua_tiles)
    #plot(ua_tiles_agg[[j]])
  } else
  {
    cell_agg <- disaggregate(cell_agg,10)
    ua_tiles_agg[[j]]  <- setValues(cell_agg,0)
  }
  setTxtProgressBar(progress_bar,j)
}  

### Merge all raster tiles to one raster
UA_Population_grid <- do.call(merge, ua_tiles_agg)
plot(UA_Population_grid)
UA_Population_grid <- round(UA_Population_grid)
plot(UA_Population_grid)

### write raster grid with population density according to our CTM domain definition
### --> input to microenvironment distribution tool
writeRaster(UA_Population_grid, paste0(output_folder,"UA_Population_grid_100m_HH.tif"), overwrite = T)
