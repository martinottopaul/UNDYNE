### calcualte sum of rasterstacks
raster_means <- function(rasterstack)
{
  progress_bar_cluster = txtProgressBar(min = 0, max = nlayers(rasterstack), initial = 0, style = 3) 
raster_means <- vector()

for(i in 1:nlayers(rasterstack))  
{
  raster_means[i] <- mean(getValues(rasterstack[[i]]))
  
  setTxtProgressBar(progress_bar_cluster,i)
}

return(raster_means)

}
