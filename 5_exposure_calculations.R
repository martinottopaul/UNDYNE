### read statrecphour.nc
require(rgdal)
require(ncdf4)
require(raster)
source("/gpfs/work/ramacher/storage/EXPOSURE/4_spatiotemporal_distribution.R")

### location of hourly concentration output from CityChem
#statrecphour <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/ref/post/statrecphour.2016.nc"
#statrecphour <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/no_lsrc/post/statrecphour.2016.nc"
statrecphour <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/base/post/statrecphour.2016.nc"
#statrecphour <- "/gpfs/work/ramacher/storage/OUTPUT/Hamburg/2016/max/post/statrecphour.2016.nc"

### exposure to which pollutant?
#pollutant <- "NO2"
pollutant <- "PM25"

### infiltration factor scenario (ref, min, max)
scenario <- "min"

### mode of exposure calculation: "static", "dynamic_ua", "dynamic_ua_osm"
### static = pollutant conc. values applied to gridded population based on residential adresses only
### dynamic = pollutant conc. values applied to diurnal varying population in urban atlas environments
### for dynamic modes make sure script 4_spatiotemporal_distribution.R is setup with correct paths in the input section
mode <- "dynamic"

### with or without OSM transport environments? TRUE/FALSE
OSM <- TRUE

### output folder for daily exposure outputs (ncdf)
#output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/output/Hamburg/dynamic_osm_tertiary/ref/pm25/"
output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/post/if_sensitivity/dynamic_osm_tertiary/base_min/"

### do you want to write daily population output (ncdf)
pop_out <- FALSE
### to where?
#pop_out_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/population/dynamic_osm_tertiary/       "
        
### for "static" mode gridded population (module 1) is necessary input
if(mode == "static")
{
        pop_static <- "/gpfs/work/ramacher/storage/EXPOSURE/population/UA_Population_grid_100m_HH.tif"
}
### for dynamic modes make sure script 4_spatiotemporal_distribution.R is setup with correct paths in the input section
### and run the script to get diurnal population for weekdays and weekends
if(mode == "dynamic")
{
        diurnal_pop <- spatiotemporal_distribution(OSM = OSM)
}

#### Infiltration Factor Section c(winter,summer)
if(mode == "static" || mode == "dynamic")
{
        if(pollutant == "NO2")
        {
                foi_home <- c(0.7,0.8)          
        }        
        
        if(pollutant == "PM25")
        {
                foi_home <- c(0.5,0.6)          
        }        
}
if(mode == "dynamic")
{
        if(pollutant == "NO2")
        {
                foi_work <- c(0.75,0.85)
                foi_othe <- c(0.8,0.9)
                foi_tran <- c(1,1)       
        }        
        
        if(pollutant == "PM25")
        {
                foi_work <- c(0.5,0.6)
                foi_othe <- c(0.8,0.9)
                foi_tran <- c(1,1)       
        }        
}
if(OSM==TRUE)
{
                foi_foot <- c(1,1)
                foi_bike <- c(1,1)
                
                if(scenario == "ref")
                {
                        if(pollutant == "NO2")
                        {
                                foi_car <- c(0.9,0.9)
                                foi_bus <- c(0.9,0.9)
                                foi_sub <- c(0.6,0.6)
                                foi_ligh <- c(0.7,0.7)
                                foi_rail <- c(0.6,0.6)
                        }        
                        
                        if(pollutant == "PM25")
                        {
                                foi_car <- c(0.7,0.8)
                                foi_bus <- c(0.9,0.9)
                                foi_sub <- c(0.7,0.7)
                                foi_ligh <- c(0.7,0.7)
                                foi_rail <- c(0.6,0.6)
                        }        
                }        
                
                if(scenario == "min")
                {
                        if(pollutant == "NO2")
                        {
                                foi_car <- c(0.7,0.7)
                                foi_bus <- c(0.7,0.7)
                                foi_sub <- c(0.4,0.4)
                                foi_ligh <- c(0.5,0.5)
                                foi_rail <- c(0.4,0.4)  
                        }        
                        
                        if(pollutant == "PM25")
                        {
                                foi_car <- c(0.5,0.6)
                                foi_bus <- c(0.7,0.7)
                                foi_sub <- c(0.5,0.5)
                                foi_ligh <- c(0.5,0.5)
                                foi_rail <- c(0.4,0.4)
                        }        
                }
                
                if(scenario == "max")
                {
                        if(pollutant == "NO2")
                        {
                                foi_car <- c(1,1)
                                foi_bus <- c(1,1)
                                foi_sub <- c(0.8,0.8)
                                foi_ligh <- c(0.9,0.9)
                                foi_rail <- c(0.8,0.8)                          
                        }        
                        
                        if(pollutant == "PM25")
                        {
                                foi_car <- c(0.9,1)
                                foi_bus <- c(1,1)
                                foi_sub <- c(0.9,0.9)
                                foi_ligh <- c(0.9,0.9)
                                foi_rail <- c(0.8,0.8)
                        }        
                }
}

#############################################
############################################# START!
#############################################

### extract dimensions from CityChem statrecphour for writing consistent ncdf4 output
### necessary for all approaches
nc <- nc_open(statrecphour)
x <- nc$dim$i
y <- nc$dim$j
t <- nc$dim$time
nc_close(nc)

### progress bar for exposure calculation and writing
progress_bar <- txtProgressBar(min = 1, max = t$len/24, initial = 0, style = 3)

for(i in 1:(t$len/24))
{
        ### identify date of day i (baed on 24 hour intervals)
        date <- as.POSIXct(t$vals[seq(1,t$len,24)[i]],
                           origin="1970-01-01 00:00:00", tz = "GMT")
        
        ### set the right O/I factor dependant on season
        if(months(date,abbreviate = T) == "Apr" || months(date,abbreviate = T) == "May" ||
           months(date,abbreviate = T) == "Mai" || months(date,abbreviate = T) == "Jun" ||
           months(date,abbreviate = T) == "Jul" || months(date,abbreviate = T) == "Aug" ||
           months(date,abbreviate = T) == "Sep")
        {
                foi <- 2
        } else {
                foi <- 1
        }
        
        ### set the right diurnal population dependent on workday or weekend
        if(weekdays(date,abbreviate = T) == "Sa" ||
           weekdays(date,abbreviate = T) == "Su" ||
           weekdays(date,abbreviate = T) == "So")
        {
                day_type <- 2
        } else {
                day_type <- 1
        }
        
        ### read 24 hours concentration for selected pollutant and day i concentration from statrecphour as rasterstack
        conc <- stack(statrecphour, varname = pollutant)[[seq(1,t$len,24)[i]:seq(24,t$len,24)[i]]]
        
        ############################################# STATIC EXPOSURE!
        
        if(mode == "static")
        {
                #conc, pop_static, foi_home, foi, t, i, x, y, output_folder, pollutant, pop_out, pop_out_folder
                
                ### read static population grid (tif)
                pop_raster <- raster(pop_static)
                
                ### calculate exposure incl. season dependant infiltration factor for home environments
                exposure_total <- conc*pop_raster*foi_home[foi]
                
                ### write daily ncdf output
                ## time dimension for day i in daily ncdf output
                day <- t
                day$vals <- t$vals[seq(1,t$len,24)[i]:seq(24,t$len,24)[i]]
                date <- as.POSIXct(day$vals[1], origin="1970-01-01 00:00:00", tz = "GMT")
                
                ### define dimensions for daily exposure netcdf output
                exposure_total_dim <- ncvar_def(paste0("Total Exposure to ",pollutant), "people*conc", list(x,y,day))
                ### create nc_output for daily exposure
                exposure_nc_out <- nc_create(paste0(output_folder,"diurnal_static_exposure_",pollutant,"_",
                                                    strftime(date, format = "%F"),".nc"),
                                             exposure_total_dim, force_v4 = T)
                ### fill variables in file
                ncvar_put(exposure_nc_out, exposure_total_dim, getValues(flip(exposure_total,"y")))
                ### close filled nc file with exposure field for day i
                nc_close(exposure_nc_out)
                
                if (pop_out == TRUE)
                {
                        ### define dimensions for daily population netcdf output
                        pop_dim <- ncvar_def("Population", "people per grid cell", list(x,y,day))
                        ### create nc_output for daily population
                        pop_nc_out <- nc_create(paste0(pop_out_folder,"static_population",
                                                       strftime(date, format = "%F"),".nc"),
                                                pop_dim, force_v4 = T)
                        ### fill variables in file
                        ncvar_put(pop_nc_out, pop_dim, 
                                  getValues(flip(stack(replicate(24, pop_raster)),"y")))
                        ### close filled nc file with population field for day i
                        nc_close(pop_nc_out)
                }
        }  
        
        ############################################# DYNAMIC EXPOSURE!
        
        if(mode == "dynamic")
        {
                if(OSM == FALSE)
                {
                        #conc, pop_static, foi_home, foi_work, foi_other, foi_tran, 
                        #foi, t, i, x, y, output_folder, pollutant, pop_out, pop_out_folder, day_type
                        
                        exposure_home <- diurnal_pop[[day_type]][[1]]*conc*foi_home[foi]
                        exposure_work <- diurnal_pop[[day_type]][[2]]*conc*foi_work[foi]
                        exposure_othe <- diurnal_pop[[day_type]][[3]]*conc*foi_othe[foi]
                        exposure_tran <- diurnal_pop[[day_type]][[4]]*conc*foi_tran[foi]
                        
                        ### write daily ncdf output
                        ## time dimension for day i in dayily ncdf output
                        day <- t
                        day$vals <- t$vals[seq(1,t$len,24)[i]:seq(24,t$len,24)[i]]
                        date <- as.POSIXct(day$vals[1], origin="1970-01-01 00:00:00", tz = "GMT")
                        
                        ### define dimensions for hourly netcdf output
                        exposure_home_dim <- ncvar_def(paste0("Home Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_work_dim <- ncvar_def(paste0("Work Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_othe_dim <- ncvar_def(paste0("Other Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_tran_dim <- ncvar_def(paste0("Transport Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_tota_dim <- ncvar_def(paste0("Total Exposure to ",pollutant), "people*conc", list(x,y,day))
                        
                        ### create nc_output for hourly exposure
                        exposure_nc_out <- nc_create(paste0(output_folder,"diurnal_dynamic_exposure_",pollutant,"_",
                                                            strftime(date, format = "%F"),".nc"),
                                                     list(exposure_home_dim, exposure_work_dim, exposure_othe_dim,
                                                          exposure_tran_dim, exposure_tota_dim), force_v4 = T)
                        ### fill variables in file
                        ncvar_put(exposure_nc_out, exposure_home_dim, getValues(flip(exposure_home,"y")))
                        ncvar_put(exposure_nc_out, exposure_work_dim, getValues(flip(exposure_work,"y")))
                        ncvar_put(exposure_nc_out, exposure_othe_dim, getValues(flip(exposure_othe,"y")))
                        ncvar_put(exposure_nc_out, exposure_tran_dim, getValues(flip(exposure_tran,"y")))
                        ncvar_put(exposure_nc_out, exposure_tota_dim, getValues(flip(exposure_home+exposure_work+
                                                                                             exposure_othe+exposure_tran,"y")))
                        
                        ### close filled nc file with exposure field
                        nc_close(exposure_nc_out)
                        
                        if (pop_out == TRUE)
                        {
                                ### define dimensions for daily population netcdf output
                                pop_dim_home <- ncvar_def("Home Population", "people per grid cell", list(x,y,day))
                                pop_dim_work <- ncvar_def("Work Population", "people per grid cell", list(x,y,day))
                                pop_dim_othe <- ncvar_def("Other Population", "people per grid cell", list(x,y,day))
                                pop_dim_tran <- ncvar_def("Transport Population", "people per grid cell", list(x,y,day))
                                pop_dim_tota <- ncvar_def("Total Population", "people per grid cell", list(x,y,day))
                                ### create nc_output for daily population
                                pop_nc_out <- nc_create(paste0(pop_out_folder,"dynamic_population",
                                                               strftime(date, format = "%F"),".nc"),
                                                        list(pop_dim_home, pop_dim_work, pop_dim_othe,
                                                             pop_dim_tran, pop_dim_tota), force_v4 = T)
                                ### fill variables in file
                                ncvar_put(pop_nc_out, pop_dim_home, getValues(flip(diurnal_pop[[day_type]][[1]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_work, getValues(flip(diurnal_pop[[day_type]][[2]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_othe, getValues(flip(diurnal_pop[[day_type]][[3]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_tran, getValues(flip(diurnal_pop[[day_type]][[4]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_tota, getValues(flip(diurnal_pop[[day_type]][[1]]+ 
                                                                                           diurnal_pop[[day_type]][[2]]+
                                                                                           diurnal_pop[[day_type]][[3]]+
                                                                                           diurnal_pop[[day_type]][[4]],"y")))
                                ### close filled nc file with population field for day i
                                nc_close(pop_nc_out)
                        }
                }
                
                if(OSM == TRUE)
                {
                        
                        #conc, pop_static, foi_home, foi_work, foi_other, foi_foot, foi_bike, foi_car,
                        # foi_bus, foi_sub, foi_ligh, foi_rail
                        #foi, t, i, x, y, output_folder, pollutant, pop_out, pop_out_folder, day_type
                        
                        exposure_home <- diurnal_pop[[day_type]][[1]]*conc*foi_home[foi]
                        exposure_work <- diurnal_pop[[day_type]][[2]]*conc*foi_work[foi]
                        exposure_othe <- diurnal_pop[[day_type]][[3]]*conc*foi_othe[foi]
                        exposure_foot <- diurnal_pop[[day_type]][[4]]*conc*foi_foot[foi]
                        exposure_bike <- diurnal_pop[[day_type]][[5]]*conc*foi_bike[foi]
                        exposure_car <- diurnal_pop[[day_type]][[6]]*conc*foi_car[foi]
                        exposure_bus <-  diurnal_pop[[day_type]][[7]]*conc*foi_bus[foi]
                        exposure_sub <-  diurnal_pop[[day_type]][[8]]*conc*foi_sub[foi]
                        exposure_ligh <-  diurnal_pop[[day_type]][[9]]*conc*foi_ligh[foi]
                        exposure_rail <-  diurnal_pop[[day_type]][[10]]*conc*foi_rail[foi]
                        exposure_tran <- exposure_foot+exposure_bike+exposure_car+
                                exposure_bus+exposure_sub+exposure_ligh+exposure_rail
                        
                        ### write daily ncdf output
                        ## time dimension for day i in dayily ncdf output
                        day <- t
                        day$vals <- t$vals[seq(1,t$len,24)[i]:seq(24,t$len,24)[i]]
                        date <- as.POSIXct(day$vals[1], origin="1970-01-01 00:00:00", tz = "GMT")
                        
                        ### define dimensions for hourly netcdf output
                        exposure_home_dim <- ncvar_def(paste0("Home Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_work_dim <- ncvar_def(paste0("Work Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_othe_dim <- ncvar_def(paste0("Other Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_foot_dim <- ncvar_def(paste0("Foot Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_bike_dim <- ncvar_def(paste0("Bike Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_car_dim <- ncvar_def(paste0("Car Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_bus_dim <- ncvar_def(paste0("Bus Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_sub_dim <- ncvar_def(paste0("Subway Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_ligh_dim <- ncvar_def(paste0("Suburban Trains Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_rail_dim <- ncvar_def(paste0("Regional Trains Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_tran_dim <- ncvar_def(paste0("Transport Exposure to ",pollutant), "people*conc", list(x,y,day))
                        exposure_tota_dim <- ncvar_def(paste0("Total Exposure to ",pollutant), "people*conc", list(x,y,day))
                        
                        ### create nc_output for hourly exposure
                        exposure_nc_out <- nc_create(paste0(output_folder,"diurnal_dynamic_OSM_exposure_",pollutant,"_",
                                                            strftime(date, format = "%F"),".nc"),
                                                     list(exposure_home_dim, exposure_work_dim, exposure_othe_dim,
                                                          exposure_foot_dim, exposure_bike_dim, exposure_car_dim,
                                                          exposure_bus_dim, exposure_sub_dim, exposure_ligh_dim,
                                                          exposure_rail_dim, exposure_tran_dim, exposure_tota_dim), 
                                                     force_v4 = T)
                        ### fill variables in file
                        ncvar_put(exposure_nc_out, exposure_home_dim, getValues(flip(exposure_home,"y")))
                        ncvar_put(exposure_nc_out, exposure_work_dim, getValues(flip(exposure_work,"y")))
                        ncvar_put(exposure_nc_out, exposure_othe_dim, getValues(flip(exposure_othe,"y")))
                        ncvar_put(exposure_nc_out, exposure_foot_dim, getValues(flip(exposure_foot,"y")))
                        ncvar_put(exposure_nc_out, exposure_bike_dim, getValues(flip(exposure_bike,"y")))
                        ncvar_put(exposure_nc_out, exposure_car_dim, getValues(flip(exposure_car,"y")))
                        ncvar_put(exposure_nc_out, exposure_bus_dim, getValues(flip(exposure_bus,"y")))
                        ncvar_put(exposure_nc_out, exposure_sub_dim, getValues(flip(exposure_sub,"y")))
                        ncvar_put(exposure_nc_out, exposure_ligh_dim, getValues(flip(exposure_ligh,"y")))
                        ncvar_put(exposure_nc_out, exposure_rail_dim, getValues(flip(exposure_rail,"y")))
                        ncvar_put(exposure_nc_out, exposure_tran_dim, getValues(flip(exposure_tran,"y")))
                        ncvar_put(exposure_nc_out, exposure_tota_dim, getValues(flip(exposure_home+exposure_work+
                                                                                             exposure_othe+exposure_tran,"y")))
                        
                        ### close filled nc file with exposure field
                        nc_close(exposure_nc_out)
                        
                        if (pop_out == TRUE)
                        {
                                ### define dimensions for daily population netcdf output
                                pop_dim_home <- ncvar_def("Home Population", "people per grid cell", list(x,y,day))
                                pop_dim_work <- ncvar_def("Work Population", "people per grid cell", list(x,y,day))
                                pop_dim_othe <- ncvar_def("Other Population", "people per grid cell", list(x,y,day))
                                pop_dim_foot <- ncvar_def("Foot Population", "people per grid cell", list(x,y,day))
                                pop_dim_bike <- ncvar_def("Bike Population", "people per grid cell", list(x,y,day))
                                pop_dim_car <- ncvar_def("Car Population", "people per grid cell", list(x,y,day))
                                pop_dim_bus <- ncvar_def("Bus Population", "people per grid cell", list(x,y,day))
                                pop_dim_sub <- ncvar_def("Subway Population", "people per grid cell", list(x,y,day))
                                pop_dim_ligh <- ncvar_def("Suburban Train Population", "people per grid cell", list(x,y,day))
                                pop_dim_rail <- ncvar_def("Regional Train Population", "people per grid cell", list(x,y,day))
                                pop_dim_tran <- ncvar_def("Transport Population", "people per grid cell", list(x,y,day))
                                pop_dim_tota <- ncvar_def("Total Population", "people per grid cell", list(x,y,day))
                                ### create nc_output for daily population
                                pop_nc_out <- nc_create(paste0(pop_out_folder,"dynamic_osm_population",
                                                               strftime(date, format = "%F"),".nc"),
                                                        list(pop_dim_home, pop_dim_work, pop_dim_othe,
                                                             pop_dim_foot, pop_dim_bike, pop_dim_car,
                                                             pop_dim_bus, pop_dim_sub, pop_dim_ligh,
                                                             pop_dim_rail, pop_dim_tran, pop_dim_tota), force_v4 = T)
                                ### fill variables in file
                                ncvar_put(pop_nc_out, pop_dim_home, getValues(flip(diurnal_pop[[day_type]][[1]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_work, getValues(flip(diurnal_pop[[day_type]][[2]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_othe, getValues(flip(diurnal_pop[[day_type]][[3]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_foot, getValues(flip(diurnal_pop[[day_type]][[4]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_bike, getValues(flip(diurnal_pop[[day_type]][[5]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_car, getValues(flip(diurnal_pop[[day_type]][[6]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_bus, getValues(flip(diurnal_pop[[day_type]][[7]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_sub, getValues(flip(diurnal_pop[[day_type]][[8]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_ligh, getValues(flip(diurnal_pop[[day_type]][[9]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_rail, getValues(flip(diurnal_pop[[day_type]][[10]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_tran, getValues(flip(diurnal_pop[[day_type]][[4]]+
                                                                                           diurnal_pop[[day_type]][[5]]+
                                                                                           diurnal_pop[[day_type]][[6]]+
                                                                                           diurnal_pop[[day_type]][[7]]+
                                                                                           diurnal_pop[[day_type]][[8]]+
                                                                                           diurnal_pop[[day_type]][[9]]+
                                                                                           diurnal_pop[[day_type]][[10]],"y")))
                                ncvar_put(pop_nc_out, pop_dim_tota, getValues(flip(diurnal_pop[[day_type]][[1]]+ 
                                                                                           diurnal_pop[[day_type]][[2]]+
                                                                                           diurnal_pop[[day_type]][[3]]+
                                                                                           diurnal_pop[[day_type]][[4]]+
                                                                                           diurnal_pop[[day_type]][[5]]+
                                                                                           diurnal_pop[[day_type]][[6]]+
                                                                                           diurnal_pop[[day_type]][[7]]+
                                                                                           diurnal_pop[[day_type]][[8]]+
                                                                                           diurnal_pop[[day_type]][[9]]+
                                                                                           diurnal_pop[[day_type]][[10]],"y")))
                                
                                ### close filled nc file with population field for day i
                                nc_close(pop_nc_out)
                        }
                }
        }
        setTxtProgressBar(progress_bar,i)
}

#q(save="no")