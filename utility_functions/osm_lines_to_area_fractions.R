## function to identify the normalized fractions of sf linestring elements 
## (downloaded from OSM) in grid cells of a raster definition.
## each grid cells fraction == length of intersected lines / total length of all lines in domain
## (c) Martin Otto Paul Ramacher (Helmholtz-Zentrum Geesthacht), 2020/15/01

osm_area_fraction <- function(domain,osm_lines)
{
  options(warn=-1)
  library(raster)
  library(sf)
  
  ### use domain raster as output raster for OSM output fractions
  osm_grid <- domain
  
  ### convert domain to sf object for further processing
  domain_sf <- st_as_sf(rasterToPolygons(domain))
  
  ### transform OSM linesources to CRS (projection) of domain definition
  osm <- st_transform(osm_lines[,1], crs=crs(domain))
  
  ### calculate total lenght [m] of all OSM line elements for normalisation of line element fractions
  osm_total <- as.integer(sum(as.numeric(st_length(osm))))
  
  ### initiate progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(domain_sf$geometry), initial = 0, style = 3) 
  
  ### intersect each grid cell with OSM line elements
  ### if there are line elements intersecting with a grid cell of our domain,
  ### the length of the intersecting OSM line elements is calculated and
  ### weighted with the sum of all OSM line elements in the total domain.
  ### this value becomes the new grid cell value and represents the
  ### normalized fraction of OSM lines in a grid cell for the total domain.
  for(i in 1:length(domain_sf$geometry))
  {
    ### intersect grid cell i with line elements
    osm_cell <- st_intersection(osm,domain_sf$geometry[i])
    ### divide length of intersected line elements with total length and
    ### store the value to grid cell i.
    ### if there is no intersection the value will be 0.
    osm_grid[i] <- sum(as.numeric(st_length(osm_cell))/osm_total) 
    ### update progress bar
    setTxtProgressBar(progress_bar,i)
  } 
  
  ### check results visually
  #plot(osm_grid)
  
  ### if line elements exceed the domain, the sum of all normalized length elements is not 1
  ### if this is the case, the value is corrected, so that the total grid sum is 1
  if (sum(getValues(osm_grid)) < 1)
  {
    osm_grid <- osm_grid*1/sum(getValues(osm_grid))
  }
  #sum(getValues(osm_grid))
  options(warn=0)
  return(osm_grid)
}