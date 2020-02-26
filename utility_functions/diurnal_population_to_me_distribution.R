dynamo <- function(microenvironment, diurnal_population)
{
  microenvironment <- raster(microenvironment)
  
  print(paste0("diurnally distribute microenvironment specific hourly population (",
               deparse(substitute(diurnal_population)),") total to ", 
               names(microenvironment),":"))
  
  progress_bar_cluster = txtProgressBar(min = 0, max = NROW(diurnal_population), initial = 0, style = 3) 
  
  me_diurnal <- list()
  
  for(i in 1:NROW(diurnal_population))
  {
    ### multiply population at hour i with normalized environment grid
    ### to top-down distribute population in total domain
    me_diurnal[[i]] <- microenvironment*diurnal_population[i]
    
    ### replace population of 1 and lower with 0
    ### and add the "lost" people (<0) before rounding
    ### all values to full integers
    ### ATTENTION: due to this procedure there is less than +/- 0.1% deviation in population totals
    #before <- sum(getValues(me_diurnal[[i]]))
    #me_diurnal[[i]][me_diurnal[[i]] < 1] <- 0
    #after <- sum(getValues(me_diurnal[[i]]))
    #me_diurnal[[i]] <- me_diurnal[[i]]*(before/after)
    #me_diurnal[[i]] <- round(me_diurnal[[i]])
    #final <- sum(getValues(me_diurnal[[i]]))
    
    ### update progress bar
    setTxtProgressBar(progress_bar_cluster,i)
    
  }
  
  return(stack(me_diurnal))
}