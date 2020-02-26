### calcualte sum of rasterstacks
raster_sums <- function(rasterstack)
{
  progress_bar_cluster = txtProgressBar(min = 0, max = nlayers(rasterstack), initial = 0, style = 3) 
raster_sums <- vector()

for(i in 1:nlayers(rasterstack))  
{
  raster_sums[i] <- sum(getValues(rasterstack[[i]]))
  
  setTxtProgressBar(progress_bar_cluster,i)
}

return(raster_sums)

}
